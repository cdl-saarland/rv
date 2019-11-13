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

#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "llvm/IR/Verifier.h"
#include <llvm/IR/PatternMatch.h>

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
  if (cached) return *cached;

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
      Mask joinedMask = Mask::getAllTrue();
      for (size_t i = 1; i < switchTerm.getNumSuccessors(); ++i) {
        auto & caseCmp = requestBranchMask(switchTerm, i, builder);
        if (joinedMask.knownAllTrue()) joinedMask = caseCmp;
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
  auto key = std::make_pair(&srcBlock, succIdx);
  auto it = edgeMasks.find(key);
  if (it != edgeMasks.end()) return &it->second;
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
  auto & branchPred = requestBranchMask(term, succIdx, builder); // edge predicate relative to block

  Mask edgeMask = Mask::getAllTrue();
  if (blockMask.knownAllTrue()) {
    edgeMask = branchPred;
  } else {
    edgeMask = MBuilder.CreateAnd(builder, blockMask, branchPred, "edge_" + BB.getName().str() + "." + std::to_string(succIdx));
    auto maskShape = vecInfo.getVectorShape(blockMask);
    auto branchShape = vecInfo.getVectorShape(branchPred);
    vecInfo.setVectorShape(edgeMask, VectorShape::join(maskShape, branchShape));
  }

  Mask& InsertedMask = setEdgeMask(BB, succIdx, edgeMask);
  IF_DEBUG_ME errs() << "\t" << edgeMask << "\n";
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

    {
      IF_DEBUG_ME { errs() << " -> SESE entry mask re-use profitable.\n"; }

      // request all incoming edges
      for (auto itPred = itBegin; itPred != itEnd; ++itPred) {
        auto * predBlock = *itPred;
        IndexSet predIndices;
        auto & predTerm = *predBlock->getTerminator();
        getPredecessorEdges(predTerm, BB, predIndices);
        requestJoinedEdgeMask(predTerm, predIndices);
      }

      return setBlockMask(BB, idomMask);
    }
  }

  // single join phi for uniform masks
  int numPreds = std::distance(itBegin, itEnd);
  auto * uniPredPhi = PHINode::Create(boolTy, numPreds, "inpreds_" + BB.getName(), &*BB.begin());
  auto * uniAVLPhi = PHINode::Create(avlTy, numPreds, "inavls_" + BB.getName(), &*BB.begin());

  std::vector<Mask> orVec; // incoming divergent edge masks

  // factor in all edges
  Mask lastUniIn = Mask::getAllTrue();

  bool redundantUniPhi = true;
  VectorShape predPhiShape = VectorShape::uni();

  for (auto itPred = itBegin; itPred != itEnd; ++itPred) {
    auto * predBlock = *itPred;

    IF_DEBUG_ME { errs() << "\t" << predBlock->getName() << ": "; }

    auto predMask = requestBlockMask(*predBlock);

    // check if the dependence to cdBlock is uniform
    auto controlShape = VectorShape::join(vecInfo.getVectorShape(predMask), vecInfo.getVectorShape(*predBlock->getTerminator()));
    bool uniBranch = controlShape.isUniform();

  // uniform predecessor branch
#if 1
    if (uniBranch) {
      IF_DEBUG_ME { errs() << " uni " << uniPredPhi << "\n"; }
      redundantUniPhi &= (lastUniIn.knownAllTrue()) || (lastUniIn == predMask);
      lastUniIn = predMask;

      uniPredPhi->addIncoming(
          &predMask.requestPredAsValue(uniPredPhi->getContext()), predBlock);
      uniAVLPhi->addIncoming(
          &predMask.requestAVLAsValue(uniAVLPhi->getContext()), predBlock);

      predPhiShape =
          VectorShape::join(vecInfo.getVectorShape(predMask), predPhiShape);
    }
#endif

    // select all edges of predBlock that lead to BB
    IndexSet predIndices;
    auto & predTerm = *predBlock->getTerminator();
    getPredecessorEdges(predTerm, BB, predIndices);
    auto &edgeMask = requestJoinedEdgeMask(predTerm, predIndices);

    // mask is already accounted for by phi
    if (uniBranch) continue;

  // branch divergence is caused by immediate predecessor -> compute predicate
    IF_DEBUG_ME { errs() << " -> div from " << predBlock->getName() << " mask " << edgeMask << "\n"; }

    // TODO what about the AVL?

    // generate a dominating definition (if necessary)
    if (edgeMask.getPred() && isa<Instruction>(edgeMask.getPred())) {
       auto * edgePredInst = cast<Instruction>(edgeMask.getPred());

      if (!DT.dominates(edgeMaskInst->getParent(), &BB)) {
        std::string defBlockName = edgeMaskInst->getParent()->getName().str();
        auto edgeMaskShape = vecInfo.getVectorShape(*edgeMask);
        auto itInsert = &*BB.begin();
        auto * edgePredPhi = PHINode::Create(boolTy, numPreds, "edgepred_domphi_" + defBlockName, itInsert);
        vecInfo.setVectorShape(*edgePredPhi, edgePredShape);
        for (auto it = itBegin; it != itEnd; ++it) {
          Value * inVal = falseConst;
          if (*it == *itPred) {
            inVal = edgePredInst;
          }
          edgePredPhi->addIncoming(inVal ,*it);
        }
        // phi is reaching def
        edgeMask.setPred(edgePredPhi);
      }
    }

    orVec.push_back(edgeMask);
  }

// start constructing the block mask
  Mask blockMask = Mask::getAllTrue();

  IF_DEBUG_ME { errs() << "\t mask(" << BB.getName() << ") = \n"; }
  IF_DEBUG_ME { errs() << "\t\t uniPredPhi: " << *uniPredPhi << " redundant " << redundantUniPhi << "\n"; }
  size_t startIdx = 0;
  if (uniPredPhi->getNumIncomingValues() == 0) {
    assert(lastUniIn.knownAllTrue() && "uniform inputs but no incoming values in uni phi");
    // no uniform inputs
    uniPredPhi->eraseFromParent();
    uniPredPhi = nullptr;
    uniAVLPhi->eraseFromParent();
    uniAVLPhi = nullptr;
    blockMask = orVec[startIdx++];
  } else if (redundantUniPhi) {
    // all uniform inputs are identical -> erase the phi
    blockMask = lastUniIn;
    uniPredPhi->eraseFromParent();
    uniPredPhi = nullptr;
    uniAVLPhi->eraseFromParent();
    uniAVLPhi = nullptr;
  } else {
    // multiple uniform inputs
    blockMask = Mask(uniPredPhi, uniAVLPhi);
  }

  // if we keep the phi set its shape
  if (uniPredPhi) {
    vecInfo.setVectorShape(*uniPredPhi, predPhiShape);
  }

  // join all incoming varying masks
  IRBuilder<> builder(&BB, BB.getFirstInsertionPt());
  MaskBuilder MBuilder(vecInfo);
  VectorShape maskShape = vecInfo.getVectorShape(blockMask);
  for (size_t i = startIdx; i < orVec.size(); ++i) {
    auto divShape = vecInfo.getVectorShape(orVec[i]);
    maskShape = VectorShape::join(maskShape, divShape);

    blockMask = MBuilder.CreateOr(builder, blockMask, orVec[i]);
    vecInfo.setVectorShape(blockMask, maskShape);
  }

  IF_DEBUG_ME { errs() << "> " << blockMask << "\n"; }

  // cache the mask
  return setBlockMask(BB, blockMask);
}


Mask&
MaskExpander::setEdgeMask(BasicBlock & BB, int succIdx, Mask BrMask) {
  EdgePred & EP = requestEdgePred(BB, succIdx);
  EP.edgeMask = BrMask;
  return EP.edgeMask;
}

Mask&
MaskExpander::setBranchMask(BasicBlock & BB, int succIdx, Mask BrMask) {
  EdgePred & EP = requestEdgePred(BB, succIdx);
  EP.branchMask = BrMask;
  return EP.branchMask;
}

void
MaskExpander::expandRegionMasks() {
  IF_DEBUG_ME {
    errs() << "-- Region before MaskExpander --\n";
    vecInfo.dump();
    errs() << "-- MaskExpander log --\n";
  }

  // eagerly all remaining masks
  IF_DEBUG_ME { errs() << "- expanding acyclic masks\n"; }
  for (auto & BB : vecInfo.getScalarFunction()) {
    if (!vecInfo.inRegion(BB)) continue;

    auto & blockMask = requestBlockMask(BB);
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
