//===- rv.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors kloessner
//

#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/IR/LegacyPassManager.h>

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/Analysis/BranchProbabilityInfo.h>

#include "rv/rv.h"
#include "rv/analysis/VectorizationAnalysis.h"
#include "rv/intrinsics.h"

#include "rv/transform/loopExitCanonicalizer.h"
#include "rv/transform/divLoopTrans.h"

#include "rv/PlatformInfo.h"
#include "rv/vectorizationInfo.h"
#include "rv/analysis/reductionAnalysis.h"

#include "rv/transform/Linearizer.h"

#include "rv/transform/splitAllocas.h"
#include "rv/transform/structOpt.h"
#include "rv/transform/srovTransform.h"
#include "rv/transform/irPolisher.h"
#include "rv/transform/bosccTransform.h"
#include "rv/transform/CoherentIFTransform.h"
#include "rv/transform/redOpt.h"
#include "rv/transform/memCopyElision.h"
#include "rv/transform/lowerDivergentSwitches.h"

#include "native/NatBuilder.h"

#include "utils/rvTools.h"

#include "rvConfig.h"
#include "report.h"

#include "rv/transform/maskExpander.h"

#include "rv/transform/crtLowering.h"


using namespace llvm;

namespace rv {

VectorizerInterface::VectorizerInterface(PlatformInfo & _platInfo, Config _config)
        : config(_config)
        , platInfo(_platInfo)
{ }

static void
EmbedInlinedCode(BasicBlock & entry, Loop & hostLoop, LoopInfo & loopInfo, std::set<BasicBlock*> & funcBlocks) {
  for (auto itSucc : successors(&entry)) {
    auto & succ = *itSucc;

    // block was newly inserted -> embed in loopInfo
    if (funcBlocks.insert(&succ).second) {
      hostLoop.addBasicBlockToLoop(&succ, loopInfo);
      EmbedInlinedCode(succ, hostLoop, loopInfo, funcBlocks);
    }
  }
}

#define IF_DEBUG_CRT IF_DEBUG

void
VectorizerInterface::lowerRuntimeCalls(VectorizationInfo & vecInfo, LoopInfo & loopInfo)
{
  auto & scalarFn = vecInfo.getScalarFunction();
  auto & mod = *scalarFn.getParent();

  std::vector<CallInst*> callSites;

  // blocks that are known to be in the function
  std::set<BasicBlock*> funcBlocks;
  for (auto & BB : scalarFn) {
    funcBlocks.insert(&BB);
  }

  for (auto & BB : scalarFn) {
    if (!vecInfo.inRegion(BB)) continue;

    for (auto & Inst : BB) {
      auto * call = dyn_cast<CallInst>(&Inst);
      if (!call) continue;
      auto * callee = dyn_cast<Function>(call->getCalledValue());
      if (!callee) continue;
      if (callee->isIntrinsic() || !callee->isDeclaration()) continue;

      Function * implFunc = requestScalarImplementation(callee->getName(), *callee->getFunctionType(), mod);
      IF_DEBUG_CRT { if (!implFunc) errs() << "CRT: could not find implementation for " << callee->getName() << "\n"; }

      if (!implFunc) continue;

      IF_DEBUG_CRT { errs() << "CRT: implementing " << callee->getName() << " with " << implFunc->getName() << "\n"; }

      // replaced called function and prepare for inlining
      auto itStart = callee->use_begin();
      auto itEnd = callee->use_end();
      for (auto itUse = itStart; itUse != itEnd; ) {
        auto & userInst = *itUse->getUser();
        itUse++;
        auto * caller = dyn_cast<CallInst>(&userInst);
        if (!caller) continue;
        if (!vecInfo.inRegion(*caller->getParent())) continue;
        callSites.push_back(caller);
        caller->setCalledFunction(implFunc);
      }
    }
  }

  // TODO repair loopInfo

  // FIXME this invalidates loop info and thus the region
  for (auto * call : callSites) {
    auto & entryBB = *call->getParent();
    auto * hostLoop = loopInfo.getLoopFor(&entryBB);
    InlineFunctionInfo IFI;
    InlineFunction(call, IFI);

    if (hostLoop) EmbedInlinedCode(entryBB, *hostLoop, loopInfo, funcBlocks);
  }
}


void
VectorizerInterface::analyze(VectorizationInfo& vecInfo,
                             const DominatorTree & domTree,
                             const PostDominatorTree& postDomTree,
                             const LoopInfo& loopInfo)
{
    IF_DEBUG {
      errs() << "Initial PlatformInfo:\n";
      platInfo.dump();

      errs() << "VA before analysis:\n";
      vecInfo.dump();
    }

    // determines value and control shapes
    VectorizationAnalysis vea(config, platInfo, vecInfo, domTree, postDomTree, loopInfo);
    vea.analyze();
}

bool
VectorizerInterface::linearize(VectorizationInfo& vecInfo,
                 DominatorTree& domTree,
                 PostDominatorTree& postDomTree,
                 LoopInfo& loopInfo,
                 BranchProbabilityInfo * pbInfo)
{
    // use a fresh domtree here
    // DominatorTree fixedDomTree(vecInfo.getScalarFunction()); // FIXME someone upstream broke the domtree
    domTree.recalculate(vecInfo.getScalarFunction());

    // early lowering of divergent switch statements
    LowerDivergentSwitches divSwitchTrans(vecInfo, loopInfo);
    if (divSwitchTrans.run()) {
      postDomTree.recalculate(vecInfo.getScalarFunction()); // FIXME
      domTree.recalculate(vecInfo.getScalarFunction()); // FIXME
    }

    // lazy mask generator
    MaskExpander maskEx(vecInfo, domTree, postDomTree, loopInfo);

    // convert divergent loops inside the region to uniform loops
    DivLoopTrans divLoopTrans(platInfo, vecInfo, maskEx, domTree, loopInfo);
    divLoopTrans.transformDivergentLoops();

    postDomTree.recalculate(vecInfo.getScalarFunction()); // FIXME
    domTree.recalculate(vecInfo.getScalarFunction()); // FIXME

    // insert CIF branches if desired
    if (config.enableCoherentIF) {
      CoherentIFTransform CoherentIFTrans(vecInfo, platInfo, maskEx, domTree, postDomTree, loopInfo, pbInfo);
      CoherentIFTrans.run();
    }

    // insert BOSCC branches if desired
    if (config.enableHeuristicBOSCC) {
      BOSCCTransform bosccTrans(vecInfo, platInfo, maskEx, domTree, postDomTree, loopInfo, pbInfo);
      bosccTrans.run();
    }
    // expand masks after BOSCC
    maskEx.expandRegionMasks();

    postDomTree.recalculate(vecInfo.getScalarFunction()); // FIXME
    domTree.recalculate(vecInfo.getScalarFunction()); // FIXME

    IF_DEBUG {
      errs() << "--- VecInfo before Linearizer ---\n";
      vecInfo.dump();
    }

    // FIXME use external reduction analysis result (if available)
    ReductionAnalysis reda(vecInfo.getScalarFunction(), loopInfo);
    auto * hostLoop = loopInfo.getLoopFor(&vecInfo.getEntry());
    if (hostLoop) reda.analyze(*hostLoop);

    // optimize reduction data flow
    ReductionOptimization redOpt(vecInfo, reda, domTree);
    redOpt.run();

    // partially linearize acyclic control in the region
    Linearizer linearizer(vecInfo, maskEx, domTree, loopInfo);
    linearizer.run();

    IF_DEBUG {
      errs() << "--- VecInfo after Linearizer ---\n";
      vecInfo.dump();
    }

    return true;
}

// flag is set if the env var holds a string that starts on a non-'0' char
bool
VectorizerInterface::vectorize(VectorizationInfo &vecInfo, DominatorTree &domTree, LoopInfo & loopInfo, ScalarEvolution & SE, MemoryDependenceResults & MDR, ValueToValueMapTy * vecInstMap) {
  // divergent memcpy lowering
  MemCopyElision mce(platInfo, vecInfo);
  mce.run();

  // split structural allocas
  if (config.enableSplitAllocas) {
    SplitAllocas split(vecInfo);
    split.run();
  } else {
    Report() << "Split allocas opt disabled (RV_DISABLE_SPLITALLOCAS != 0)\n";
  }

  // transform allocas from Array-of-struct into Struct-of-vector where possibe
  if (config.enableStructOpt) {
    StructOpt sopt(vecInfo, platInfo.getDataLayout());
    sopt.run();
  } else {
    Report() << "Struct opt disabled (RV_DISABLE_STRUCTOPT != 0)\n";
  }

  // Scalar-Replication-Of-Varying-(Aggregates): split up structs of vectorizable elements to promote use of vector registers
  if (config.enableSROV) {
    SROVTransform srovTransform(vecInfo, platInfo);
    srovTransform.run();
  } else {
    Report() << "SROV opt disabled (RV_DISABLE_SROV != 0)\n";
  }

  auto * hostLoop = loopInfo.getLoopFor(&vecInfo.getEntry());
  ReductionAnalysis reda(vecInfo.getScalarFunction(), loopInfo);
  if (hostLoop) reda.analyze(*hostLoop);

// vectorize with native
  NatBuilder natBuilder(config, platInfo, vecInfo, domTree, MDR, SE, reda);
  natBuilder.vectorize(true, vecInstMap);

  // IR Polish phase: promote i1 vectors and perform early instruction (read: intrinsic) selection
  if (config.enableIRPolish) {
    IRPolisher polisher(vecInfo.getVectorFunction(), config);
    polisher.polish();
    Report() << "IR Polisher enabled (RV_ENABLE_POLISH != 0)\n";
  }

  IF_DEBUG verifyFunction(vecInfo.getVectorFunction());

  return true;
}

void
VectorizerInterface::finalize() {
  // TODO strip finalize
}

template <typename Impl>
static void lowerIntrinsicCall(CallInst* call, Impl impl) {
  call->replaceAllUsesWith(impl(call));
  call->eraseFromParent();
}

static bool
lowerIntrinsicCall(CallInst* call) {
  switch (GetIntrinsicID(*call)) {
    case RVIntrinsic::Unknown:
    default:
      return false;

    case RVIntrinsic::Any:
    case RVIntrinsic::All:
    case RVIntrinsic::Extract:
    case RVIntrinsic::Shuffle:
    case RVIntrinsic::Align:
    case RVIntrinsic::Compact: {
      lowerIntrinsicCall(call, [] (const CallInst* call) {
        return call->getOperand(0);
      });
    } break;

    case RVIntrinsic::Insert: {
      lowerIntrinsicCall(call, [] (const CallInst* call) {
        return call->getOperand(2);
      });
    } break;

    case RVIntrinsic::VecLoad: {
      lowerIntrinsicCall(call, [] (CallInst* call) {
        IRBuilder<> builder(call);
        auto * ptrTy = PointerType::get(builder.getFloatTy(), call->getOperand(0)->getType()->getPointerAddressSpace());
        auto * ptrCast = builder.CreatePointerCast(call->getOperand(0), ptrTy);
        auto * gep = builder.CreateGEP(ptrCast, { call->getOperand(1) });
        return builder.CreateLoad(gep);
      });
    } break;

    case RVIntrinsic::VecStore: {
      lowerIntrinsicCall(call, [] (CallInst* call) {
        IRBuilder<> builder(call);
        auto * ptrTy = PointerType::get(builder.getFloatTy(), call->getOperand(0)->getType()->getPointerAddressSpace());
        auto * ptrCast = builder.CreatePointerCast(call->getOperand(0), ptrTy);
        auto * gep = builder.CreateGEP(ptrCast, { call->getOperand(1) });
        return builder.CreateStore(call->getOperand(2), gep);
      });
    } break;

    case RVIntrinsic::Mask: {
      lowerIntrinsicCall(call, [] (CallInst* call) {
        return ConstantInt::getTrue(call->getContext()); }
      );
    } break;

    case RVIntrinsic::Ballot:
    case RVIntrinsic::PopCount: {
      lowerIntrinsicCall(call, [] (CallInst* call) {
        IRBuilder<> builder(call);
        return builder.CreateZExt(call->getOperand(0), call->getType());
      });
    } break;
  }

  return true;
}

bool
lowerIntrinsics(Module & mod) {
  bool changed = false;
  // TODO re-implement using RVIntrinsic enum
  const char* names[] = {"rv_any", "rv_all", "rv_extract", "rv_insert", "rv_mask", "rv_load", "rv_store", "rv_shuffle", "rv_ballot", "rv_align", "rv_popcount", "rv_compact"};
  for (int i = 0, n = sizeof(names) / sizeof(names[0]); i < n; i++) {
    auto func = mod.getFunction(names[i]);
    if (!func) continue;

    for (
      auto itUse = func->use_begin();
      itUse != func->use_end();
      itUse = func->use_begin())
    {
      auto *user = itUse->getUser();

      if (!isa<CallInst>(user)) {
        errs() << "Intrinsic used in non-call context: " << *user << "\n";
      }

      changed |= lowerIntrinsicCall(cast<CallInst>(user));
    }
  }
  return changed;
}

bool
lowerIntrinsics(Function & func) {
  bool changed = false;
  for (auto & block : func) {
    BasicBlock::iterator itStart = block.begin(), itEnd = block.end();
    for (BasicBlock::iterator it = itStart; it != itEnd; ) {
      auto * inst = &*it++;
      auto * call = dyn_cast<CallInst>(inst);
      if (call) changed |= lowerIntrinsicCall(call);
    }
  }
  return changed;
}



} // namespace rv
