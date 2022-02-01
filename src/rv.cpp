//===- rv/rv.cpp - basic vectorizer pipeline --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/IR/LegacyPassManager.h>

#include "llvm/IR/PassManager.h"
#include <llvm/Analysis/BranchProbabilityInfo.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Verifier.h>
#include "llvm/Analysis/LoopInfo.h"

#include "rv/rv.h"
#include "rv/analysis/VectorizationAnalysis.h"
#include "rv/intrinsics.h"

// Transform also exposed as LLVM passes
#include "rv/passes/irPolisher.h"
#include "rv/passes/loopExitCanonicalizer.h"

#include "rv/PlatformInfo.h"
#include "rv/vectorizationInfo.h"
#include "rv/analysis/reductionAnalysis.h"

// RV internal transformations.
#include "rv/transform/CoherentIFTransform.h"
#include "rv/transform/Linearizer.h"
#include "rv/transform/bosccTransform.h"
#include "rv/transform/guardedDivLoopTrans.h"
#include "rv/transform/lowerDivergentSwitches.h"
#include "rv/transform/memCopyElision.h"
#include "rv/transform/redOpt.h"
#include "rv/transform/splitAllocas.h"
#include "rv/transform/srovTransform.h"
#include "rv/transform/structOpt.h"

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
VectorizerInterface::lowerRuntimeCalls(VectorizationInfo & vecInfo, FunctionAnalysisManager & FAM)
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
      auto * callee = call->getCalledFunction();
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

  // must not invalidate LI
  auto & LI = *FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction());
  bool Changed = !callSites.empty();
  for (auto * call : callSites) {
    auto & entryBB = *call->getParent();
    auto * hostLoop = LI.getLoopFor(&entryBB);
    InlineFunctionInfo IFI;
    InlineFunction(*call, IFI);

    if (hostLoop) EmbedInlinedCode(entryBB, *hostLoop, LI, funcBlocks);
  }
  if (Changed) {
    auto PA = PreservedAnalyses::all();
    PA.abandon<DominatorTreeAnalysis>();
    PA.abandon<PostDominatorTreeAnalysis>();
    FAM.invalidate(vecInfo.getScalarFunction(), PA);
  }
}


void
VectorizerInterface::analyze(VectorizationInfo& vecInfo,
                             FunctionAnalysisManager& FAM)
{
    IF_DEBUG {
      errs() << "Initial PlatformInfo:\n";
      platInfo.dump();

      errs() << "VA before analysis:\n";
      vecInfo.dump();
    }

    // determines value and control shapes
    VectorizationAnalysis vea(config, platInfo, vecInfo, FAM);
    vea.analyze();
}

bool
VectorizerInterface::linearize(VectorizationInfo& vecInfo,
                 FunctionAnalysisManager & FAM) {
    
    // TODO make this part of a new optimization phase
    // Scalar-Replication-Of-Varying-(Aggregates): split up structs of vectorizable elements to promote use of vector registers
    if (config.enableSROV) {
      SROVTransform srovTransform(vecInfo, platInfo);
      bool Changed = srovTransform.run();
      while (Changed) {
        // re-run DA
        vecInfo.forgetInferredProperties();
        analyze(vecInfo, FAM);

        // re-run SROV
        Changed = srovTransform.run();
      }
    } else {
      Report() << "SROV opt disabled (RV_DISABLE_SROV != 0)\n";
    }
  
    // early lowering of divergent switch statements
    LowerDivergentSwitches divSwitchTrans(vecInfo, FAM);
    divSwitchTrans.run();

    // FIXME materialize masks only very late in the process (risk of mask invalidation through transformations)
    MaskExpander maskEx(vecInfo, FAM);

    // convert divergent loops inside the region to uniform loops
    GuardedDivLoopTrans guardedDLT(platInfo, vecInfo, FAM);
    guardedDLT.transformDivergentLoops();

    // insert CIF branches if desired
    if (config.enableCoherentIF) {
      CoherentIFTransform CoherentIFTrans(vecInfo, platInfo, maskEx, FAM);
      CoherentIFTrans.run();
    }

    // insert BOSCC branches if desired
    if (config.enableHeuristicBOSCC) {
      BOSCCTransform bosccTrans(vecInfo, platInfo, maskEx, FAM);
      bosccTrans.run();
    }
    // expand masks after BOSCC
    maskEx.expandRegionMasks();

    IF_DEBUG {
      errs() << "--- VecInfo before Linearizer ---\n";
      vecInfo.dump();
    }

    // FIXME use external reduction analysis result (if available)
    ReductionAnalysis reda(vecInfo.getScalarFunction(), FAM);
    auto & LI = *FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction());
    auto * hostLoop = LI.getLoopFor(&vecInfo.getEntry());
    if (hostLoop) reda.analyze(*hostLoop);

    // optimize reduction data flow
    ReductionOptimization redOpt(vecInfo, reda, FAM);
    redOpt.run();

    // partially linearize acyclic control in the region
    Linearizer linearizer(config, vecInfo, maskEx, FAM);
    linearizer.run();

    IF_DEBUG {
      errs() << "--- VecInfo after Linearizer ---\n";
      vecInfo.dump();
    }

    return true;
}

// flag is set if the env var holds a string that starts on a non-'0' char
bool
VectorizerInterface::vectorize(VectorizationInfo &vecInfo, FunctionAnalysisManager &FAM, ValueToValueMapTy * vecInstMap) {
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
  // FIXME Cannot happen before DA re-run because StructOpt modifies ptr shapes to created contiguous stack accesses!
  if (config.enableStructOpt) {
    StructOpt sopt(vecInfo, platInfo.getDataLayout());
    sopt.run();
  } else {
    Report() << "Struct opt disabled (RV_DISABLE_STRUCTOPT != 0)\n";
  }


  auto &LI = *FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction());
  auto * hostLoop = LI.getLoopFor(&vecInfo.getEntry());
  ReductionAnalysis reda(vecInfo.getScalarFunction(), FAM);
  if (hostLoop) reda.analyze(*hostLoop);

// vectorize with native
  NatBuilder natBuilder(config, platInfo, vecInfo, reda, FAM);
  natBuilder.vectorize(true, vecInstMap);

  // IR Polish phase: promote i1 vectors and perform early instruction (read: intrinsic) selection
  if (config.enableIRPolish) {
    IRPolisher Polisher(vecInfo.getVectorFunction());
    Polisher.polish();
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
        // FIXME: Re-use the intrinsic types.
        IRBuilder<> builder(call);
        auto *DataTy = builder.getFloatTy();
        auto *ptrTy = PointerType::get(
            DataTy, call->getOperand(0)->getType()->getPointerAddressSpace());
        auto *ptrCast = builder.CreatePointerCast(call->getOperand(0), ptrTy);
        auto *gep = builder.CreateGEP(DataTy, ptrCast, call->getOperand(1));
        return builder.CreateLoad(builder.getFloatTy(), gep);
      });
    } break;

    case RVIntrinsic::VecStore: {
      lowerIntrinsicCall(call, [] (CallInst* call) {
        IRBuilder<> builder(call);
        auto *DataTy = builder.getFloatTy();
        auto *ptrTy = PointerType::get(
            DataTy, call->getOperand(0)->getType()->getPointerAddressSpace());
        auto *ptrCast = builder.CreatePointerCast(call->getOperand(0), ptrTy);
        auto *gep = builder.CreateGEP(DataTy, ptrCast, call->getOperand(1));
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
    case RVIntrinsic::NumLanes:
      return ConstantInt::get(call->getType(), 1, false);
    case RVIntrinsic::LaneID:
      return ConstantInt::get(call->getType(), 0, false);
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
