//===- src/transform/maskExpander.cpp - IR generator for edge and block predicates  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/transform/maskExpander.h"
#include "rvConfig.h"
#include "rv/MaskBuilder.h"
#include "utils/rvTools.h"
#include "rv/vectorizationInfo.h"

#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/PatternMatch.h"
#include "llvm/IR/CFG.h"

#include <cassert>

#if 1
#define IF_DEBUG_ME IF_DEBUG
#else
#define IF_DEBUG_ME if (true)
#endif

using namespace llvm;

namespace rv {

MaskExpander::MaskExpander(VectorizationInfo & _vecInfo, FunctionAnalysisManager & FAM)
: vecInfo(_vecInfo)
, FAM(FAM)
, loopInfo(*FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction()))
, boolTy(Type::getInt1Ty(vecInfo.getContext()))
, trueConst(ConstantInt::getTrue(vecInfo.getContext()))
, falseConst(ConstantInt::getFalse(vecInfo.getContext()))
{}

MaskExpander::~MaskExpander()
{}




Mask*
MaskExpander::getEdgeMask(const BasicBlock & begin, const BasicBlock & end) {
  IndexSet edgeIndices;
  auto & term = *begin.getTerminator();
  getPredecessorEdges(term, end, edgeIndices);
  assert(edgeIndices.size() == 1);
  return getEdgeMask(term, edgeIndices[0]);
}

void
MaskExpander::getPredecessorEdges(const Instruction & termInst, const  BasicBlock & BB, IndexSet & oIndices) const {
  for (size_t i = 0; i < termInst.getNumSuccessors(); ++i) {
    auto * succBlock = termInst.getSuccessor(i);
    if (succBlock != &BB) continue;
    oIndices.push_back(i);
  }
}

Mask &
MaskExpander::requestBranchMask(Instruction & term, int succIdx, IRBuilder<> & builder) {
  MaskBuilder MBuilder(vecInfo);

  auto & sourceBlock = *term.getParent();
  IF_DEBUG_ME { errs() << "# requestBranchMask( " << sourceBlock.getName() << ", " << succIdx << ")\n"; }
  auto * cached = getBranchMask(term, succIdx);
  if (cached) {
    IF_DEBUG_ME { errs() << "\tCached: " << *cached << "\n"; }
    return *cached;
  }

  if (isa<BranchInst>(term)) {
    auto & branch = cast<BranchInst>(term);
    auto * condVal = branch.isConditional() ? branch.getCondition() : trueConst;

    if (succIdx == 0) {
      Mask BrMask = Mask::inferFromPredicate(*condVal);
      return setBranchMask(sourceBlock, succIdx, BrMask);
    }

    assert(succIdx == 1);

    auto & negCond = *builder.CreateNot(condVal, "neg." + condVal->getName());
    if (!isa<Constant>(negCond)) vecInfo.setVectorShape(negCond, vecInfo.getVectorShape(*condVal));
    return setBranchMask(sourceBlock, succIdx, Mask::inferFromPredicate(negCond));

  } else if (isa<SwitchInst>(term)) {
    auto & switchTerm = cast<SwitchInst>(term);
    auto & switchVal = *switchTerm.getCondition();
    auto valShape = vecInfo.getVectorShape(switchVal);
    auto & defaultDest = *switchTerm.getDefaultDest();

    auto itCase = SwitchInst::CaseIt::fromSuccessorIndex(&switchTerm, succIdx);
    auto & caseBlock = *switchTerm.getSuccessor(succIdx);

    // default case mask = !(case1 || case2 || .. )
    if (&caseBlock == &defaultDest) {
      assert(succIdx == 0);
      Mask joinedMask = Mask::getAllFalse(caseBlock.getContext());
      for (size_t i = 1; i < switchTerm.getNumSuccessors(); ++i) {
        auto & caseCmp = requestBranchMask(switchTerm, i, builder);
        if (joinedMask.knownAllFalse()) joinedMask = caseCmp;
        else {
          joinedMask = MBuilder.CreateOr(builder, joinedMask, caseCmp, "orcase_" + std::to_string(i));
          vecInfo.setVectorShape(joinedMask, valShape);
        }
      }

      auto defaultMask = MBuilder.CreateNot(builder, joinedMask);
      vecInfo.setVectorShape(defaultMask, valShape);
      return setBranchMask(sourceBlock, succIdx, defaultMask);

    // case test, mask = (switchVal == caseVal)
    } else {
      auto & caseVal = *itCase->getCaseValue();
      auto & caseCmp = *builder.CreateICmp(ICmpInst::ICMP_EQ, &caseVal, &switchVal, "caseeq_" + std::to_string(caseVal.getSExtValue()));
      vecInfo.setVectorShape(caseCmp, valShape);
      return setBranchMask(sourceBlock, succIdx, Mask::inferFromPredicate(caseCmp));
    }

  } else if (isa<InvokeInst>(term)) {
    // eagerly lower all successors
    auto & invokeMask = requestBlockMask(sourceBlock);
    setBranchMask(sourceBlock, 0, invokeMask);
    setBranchMask(sourceBlock, 1, Mask::getAllTrue());

    if (succIdx == 0) {
      return invokeMask;
    } else {
      return setBranchMask(sourceBlock, succIdx, Mask::getAllTrue());
    }
  }

  assert(false && "unsupported terminator!");
  abort();
}

Mask &
MaskExpander::requestJoinedEdgeMask(Instruction & term, IndexSet succIdx) {
  assert(succIdx.size() == 1 && "TODO implement for multiple edges (switch)");
  return requestEdgeMask(term, succIdx[0]);
}


Mask &
MaskExpander::requestEdgeMask(llvm::BasicBlock & source, BasicBlock & dest) {
  auto & srcTerm = *source.getTerminator();
  IndexSet edgeIndices;
  getPredecessorEdges(srcTerm, dest, edgeIndices);
  return requestJoinedEdgeMask(srcTerm, edgeIndices);
}

MaskExpander::EdgePred &
MaskExpander::requestEdgePred(const llvm::BasicBlock & SrcBlock, int SuccIdx) {
  auto Key = std::make_pair(&SrcBlock, SuccIdx);
  auto ItMask = edgeMasks.find(Key);
  if (ItMask != edgeMasks.end()) return ItMask->second;
  return edgeMasks.insert(std::make_pair(Key, EdgePred())).first->second;
}

MaskExpander::EdgePred*
MaskExpander::getEdgePred(const llvm::BasicBlock & srcBlock, int succIdx) {
  auto Key = std::make_pair(&srcBlock, succIdx);
  auto It = edgeMasks.find(Key);
  if (It != edgeMasks.end()) return &It->second;
  return nullptr;
}

Mask&
MaskExpander::requestEdgeMask(Instruction & term, int succIdx) {
  auto *cachedMask = getEdgeMask(term, succIdx);
 if (cachedMask) return *cachedMask;

  // allocate an edge handle
  requestEdgePred(*term.getParent(), succIdx);
  auto & BB = *term.getParent();
  IF_DEBUG_ME { errs() << "# requestEdgeMask( " << BB.getName() << ", " << succIdx << ")\n"; }

  // start buildin a edge mask
  // block entry mask
  auto & blockMask = requestBlockMask(BB);

  // branch mask
  IRBuilder<> builder(BB.getTerminator());
  MaskBuilder MBuilder(vecInfo);
  Mask branchPred = requestBranchMask(term, succIdx, builder); // edge predicate relative to block

  Mask edgeMask;
  if (blockMask.knownAllTrue()) {
    edgeMask = branchPred;
  } else {
    edgeMask = MBuilder.CreateAnd(builder, blockMask, branchPred, "edge_" + BB.getName().str() + "." + std::to_string(succIdx));
    auto maskShape = vecInfo.getVectorShape(blockMask);
    auto branchShape = vecInfo.getVectorShape(branchPred);
    vecInfo.setVectorShape(edgeMask, VectorShape::join(maskShape, branchShape));
  }

  Mask& InsertedMask = setEdgeMask(BB, succIdx, edgeMask);
  IF_DEBUG_ME errs() << "\t" << InsertedMask << "\n";
  return InsertedMask;
}

Mask&
MaskExpander::requestBlockMask(BasicBlock & BB) {
  assert(vecInfo.inRegion(BB));

  IF_DEBUG_ME { errs() << "# requestBlockMask( " << BB.getName() << ")\n"; }

  // region entry mask
  // - WFV mode: use the mask arg if available
  // - region mode: true
  auto & entryBlock = vecInfo.getEntry();
  if (&BB == &entryBlock) {
    IF_DEBUG_ME {errs() << "region entryMask: true\n"; }
    return setBlockMask(BB, Mask::getAllTrue());
  }

  // return the cached result
  auto *CachedMask = getBlockMask(BB);
  if (CachedMask) return *CachedMask;

  // all-uniform control context
  bool BlockHasVaryingControlMask = true;
  if (vecInfo.getVaryingPredicateFlag(BB, BlockHasVaryingControlMask)) {
    if (!BlockHasVaryingControlMask) {
      return setBlockMask(BB, Mask::inferFromPredicate(*trueConst));
    }
  }

  // Otw, start buildling a mask
  IF_DEBUG_ME { errs() << "Construct mask:\n"; }

  // if this is a uniform loop use the preheader edge mask
  auto * bbLoop = loopInfo.getLoopFor(&BB);
  if (bbLoop && (&BB == bbLoop->getHeader())) {
    assert(!vecInfo.isDivergentLoop(*bbLoop) && "maskExpander can not handle divergent loops directly! (use divLoopTrans)");
    IF_DEBUG_ME { errs() << " -> uniform loop header.\n"; }

    auto * loopPreHead = bbLoop->getLoopPreheader();
    assert(loopPreHead);

    IndexSet initIndices;
    getPredecessorEdges(*loopPreHead->getTerminator(), BB, initIndices);
    auto & loopEdgeMask = requestJoinedEdgeMask(*loopPreHead->getTerminator(), initIndices);
    return setBlockMask(BB, loopEdgeMask);
  }

  auto itBegin = pred_begin(&BB);
  auto itEnd = pred_end(&BB);

  // check if we post-dominate our idom (optimization)
  auto &DT = FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction());
  auto &PDT = FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction());
  auto * domNode = DT.getNode(&BB);
  auto &idomBlock = *domNode->getIDom()->getBlock();

// try to re-use the SESE entry mask
  if (vecInfo.inRegion(idomBlock) && PDT.dominates(&BB, &idomBlock)) {
    auto & idomMask = requestBlockMask(idomBlock);
    IF_DEBUG_ME { errs() << " -> SESE entry mask re-use profitable.\n"; }

    return setBlockMask(BB, idomMask);
  }

  // Defer lowering by phi selection
  int numPreds = std::distance(itBegin, itEnd);
  IRBuilder<> Builder(&*BB.getFirstInsertionPt());
  PHINode *PredPhi = Builder.CreatePHI(boolTy, numPreds, BB.getName() + ".cm");
  // PHINode *AVLPhi = Builder.CreatePHI(avlTy, numPreds, BB.getName() + ".cm"); // TODO implement AVL support

  for (auto itPred = itBegin; itPred != itEnd; ++itPred) {
    auto * predBlock = *itPred;

    IF_DEBUG_ME { errs() << "\t" << predBlock->getName() << ": "; }
    if (!vecInfo.inRegion(*predBlock)) continue;

    // check the incoming edge
    IndexSet predIndices;
    auto & predTerm = *predBlock->getTerminator();
    getPredecessorEdges(predTerm, BB, predIndices);

    // generate a dominating definition (if necessary)
    Mask InCMask = requestJoinedEdgeMask(predTerm, predIndices);

    if (vecInfo.getVectorShape(InCMask).isUniform()) {
      // exploit partial linearization knowing that control will only reach
      // through uniform control-flow edge iff it is supposed to reach here.
      PredPhi->addIncoming(Builder.getTrue(), predBlock);
    } else {
      auto *InCPred = &InCMask.requestPredAsValue(Builder.getContext());
      PredPhi->addIncoming(InCPred, predBlock);
    }

    // TODO handle the AVL
    // AVLPhi->addIncoming(Builder.getTrue(), predBlock);
  }

  // Simplify if there is only a single incoming edge 
  // this is necessary to not fool Linearizer into believing this was a LCSSA phi
  Mask BlockMask;
  if (PredPhi->getNumIncomingValues() == 1) {
    // discard the phi, keeping the only incoming predicate
    Value * InPred = PredPhi->getIncomingValue(0);
    BlockMask = Mask::inferFromPredicate(*InPred);
    PredPhi->eraseFromParent();

  } else {
    // keep the phi node
    vecInfo.setVectorShape(*PredPhi, VectorShape::varying());
    setShadowInput(*PredPhi, *falseConst);

    BlockMask = Mask::inferFromPredicate(*PredPhi);
  }

  IF_DEBUG_ME { errs() << "> " << BlockMask << "\n"; }

  // cache the mask
  return setBlockMask(BB, BlockMask);
}


Mask&
MaskExpander::setEdgeMask(BasicBlock & BB, int succIdx, Mask BrMask) {
  EdgePred & EP = requestEdgePred(BB, succIdx);
  EP.edgeMask = BrMask;
  return EP.edgeMask.getValue();
}

Mask&
MaskExpander::setBranchMask(BasicBlock & BB, int succIdx, Mask BrMask) {
  EdgePred & EP = requestEdgePred(BB, succIdx);
  EP.branchMask = BrMask;
  return EP.branchMask.getValue();
}

void
MaskExpander::expandRegionMasks() {
  IF_DEBUG_ME {
    errs() << "-- Region before MaskExpander --\n";
    vecInfo.dump();
    errs() << ":: MaskExpander log ::\n";
  }

  // eagerly all remaining masks
  IF_DEBUG_ME { errs() << "- expanding block and edge masks\n"; }
  for (auto & BB : vecInfo.getScalarFunction()) {
    if (!vecInfo.inRegion(BB)) continue;

    // request the block control mask
    auto & blockMask = requestBlockMask(BB);

    // request all incoming edges
    // auto itBegin = pred_begin(&BB);
    // auto itEnd = pred_end(&BB);
    for (auto *predBlock : predecessors(&BB)) {
      if (!vecInfo.inRegion(*predBlock)) continue;
      IndexSet predIndices;
      auto & predTerm = *predBlock->getTerminator();
      getPredecessorEdges(predTerm, BB, predIndices);
      requestJoinedEdgeMask(predTerm, predIndices);
    }

    // TODO AND-in in block predicate (also rename to requestCMask).
    vecInfo.setMask(BB, blockMask);
  }

  // finalize loop live masks by adding masks on loop entry
  // patchLoopMasks();

  IF_DEBUG_ME {
    errs() << "-- Region after MaskExpander --\n";
    vecInfo.dump();
    errs() << "-- verifying IR (non-dom anticipated):\n";
    verifyFunction(vecInfo.getScalarFunction(), &errs());
    errs() << "-- EOF MaskExpander --\n";
  }
}

} // namespace rv
