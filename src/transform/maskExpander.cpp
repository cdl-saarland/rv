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

#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "llvm/IR/Verifier.h"
#include <llvm/IR/PatternMatch.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Analysis/InstructionSimplify.h>

#include <cassert>

#if 1
#define IF_DEBUG_ME IF_DEBUG
#else
#define IF_DEBUG_ME if (true)
#endif


// use selects for mask arithmetic
// #define RV_BLEND_MASKS

using namespace llvm;

Value*
MatchMaskIntrinsic(Value & condVal) {
  auto * call = dyn_cast<CallInst>(&condVal);
  if (!call) return nullptr;

  auto * callee = call->getCalledFunction();
  if (!callee) return nullptr;

  if (callee->getName() == "rv_any") {
    return call->getArgOperand(0);
  }

  return nullptr;
}


namespace rv {

static
Value&
CreateAnd(IRBuilder<> & builder, Value & lhs, Value & rhs, const Twine & name=Twine()) {
  using namespace llvm::PatternMatch;

// Optimize for a common pattern
  Value * anyTestedMask = MatchMaskIntrinsic(rhs);
  if (anyTestedMask) {
    Value * X, *Y, *Z = nullptr;
    // lhs = and x (not y)
    // rhs = any (not y)
    if (match(&lhs, m_And(m_Value(X), m_Not(m_Value(Y)))) &&
        match(anyTestedMask, m_Not(m_Value(Z))) &&
        (Y == Z))
    {
      return lhs; // and lhs rhs
    }
  }

#ifdef RV_BLEND_MASKS
  auto * falseMask = ConstantInt::getFalse(lhs.getContext());
  return *builder.CreateSelect(&lhs, &rhs, falseMask, name);
#else
  return *builder.CreateAnd(&lhs, &rhs, name);
#endif
}

static
Value&
CreateOr(IRBuilder<> & builder, Value & lhs, Value & rhs, const Twine & name=Twine()) {
#ifdef RV_BLEND_MASKS
  auto * trueMask = ConstantInt::getTrue(lhs.getContext());
  return *builder.CreateSelect(&lhs, trueMask, &rhs, name);
#else
  return *builder.CreateOr(&lhs, &rhs, name);
#endif
}

static
Value&
CreateNot(IRBuilder<> & builder, Value & val, const Twine & name=Twine()) {
#ifdef RV_BLEND_MASKS
  auto * sel = dyn_cast<SelectInst>(&val);
  if (sel) {
    return *builder.CreateSelect(sel->getCondition(), sel->getFalseValue(), sel->getTrueValue(), name);
  }
  // otw use default codepath
#endif

return *builder.CreateNot(&val, name);
}

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




Value*
MaskExpander::getEdgeMask(const BasicBlock & begin, const BasicBlock & end) const {
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

Value &
MaskExpander::requestBranchMask(Instruction & term, int succIdx, IRBuilder<> & builder) {
  auto & sourceBlock = *term.getParent();
  IF_DEBUG_ME { errs() << "# requestBranchMask( " << sourceBlock.getName() << ", " << succIdx << ")\n"; }
  auto * cached = getBranchMask(term, succIdx);
  if (cached) return *cached;

  if (isa<BranchInst>(term)) {
    auto & branch = cast<BranchInst>(term);
    auto * condVal = branch.isConditional() ? branch.getCondition() : trueConst;

    if (succIdx == 0) {
      return *condVal;
    }

    assert(succIdx == 1);

    auto & negCond = CreateNot(builder, *condVal, "neg." + condVal->getName());
    if (!isa<Constant>(negCond)) vecInfo.setVectorShape(negCond, vecInfo.getVectorShape(*condVal));
    setBranchMask(sourceBlock, succIdx, negCond);
    return negCond;

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
      Value * joinedMask = nullptr;
      for (size_t i = 1; i < switchTerm.getNumSuccessors(); ++i) {
        auto & caseCmp = requestBranchMask(switchTerm, i, builder);
        if (!joinedMask) joinedMask = &caseCmp;
        else {
          joinedMask = &CreateOr(builder, *joinedMask, caseCmp, "orcase_" + std::to_string(i));
          vecInfo.setVectorShape(*joinedMask, valShape);
        }
      }

      auto & defaultMask = CreateNot(builder, *joinedMask);
      vecInfo.setVectorShape(defaultMask, valShape);
      setBranchMask(sourceBlock, succIdx, defaultMask);
      return defaultMask;

    // case test, mask = (switchVal == caseVal)
    } else {
      auto & caseVal = *itCase->getCaseValue();
      auto & caseCmp = *builder.CreateICmp(ICmpInst::ICMP_EQ, &caseVal, &switchVal, "caseeq_" + std::to_string(caseVal.getSExtValue()));
      vecInfo.setVectorShape(caseCmp, valShape);
      setBranchMask(sourceBlock, succIdx, caseCmp);
      return caseCmp;
    }

  } else if (isa<InvokeInst>(term)) {
    // eagerly lower all successors
    auto & invokeMask = requestBlockMask(sourceBlock);
    setBranchMask(sourceBlock, 0, invokeMask);
    auto & trueConst = *ConstantInt::getTrue(builder.getContext());
    setBranchMask(sourceBlock, 1, trueConst);

    if (succIdx == 0) {
      return invokeMask;
    } else {
      return trueConst;
    }
  }

  assert(false && "unsupported terminator!");
  abort();
}

Value &
MaskExpander::requestJoinedEdgeMask(Instruction & term, IndexSet succIdx) {
  assert(succIdx.size() == 1 && "TODO implement for multiple edges (switch)");
  return requestEdgeMask(term, succIdx[0]);
}


Value &
MaskExpander::requestEdgeMask(llvm::BasicBlock & source, BasicBlock & dest) {
  auto & srcTerm = *source.getTerminator();
  IndexSet edgeIndices;
  getPredecessorEdges(srcTerm, dest, edgeIndices);
  return requestJoinedEdgeMask(srcTerm, edgeIndices);
}

MaskExpander::EdgePred &
MaskExpander::requestEdgePred(const llvm::BasicBlock & srcBlock, int succIdx) {
  auto key = std::make_pair(&srcBlock, succIdx);
  auto it = edgeMasks.find(key);
  if (it != edgeMasks.end()) return it->second;
  return edgeMasks.insert(std::make_pair(key, EdgePred())).first->second;
}

const MaskExpander::EdgePred*
MaskExpander::getEdgePred(const llvm::BasicBlock & srcBlock, int succIdx) const {
  auto key = std::make_pair(&srcBlock, succIdx);
  auto it = edgeMasks.find(key);
  if (it != edgeMasks.end()) return &it->second;
  return nullptr;
}

Value&
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
  auto & branchPred = requestBranchMask(term, succIdx, builder); // edge predicate relative to block

  Value * edgeMask = nullptr;
  if (isa<Constant>(blockMask)) {
    edgeMask = &branchPred;
  } else {
    edgeMask = &CreateAnd(builder, blockMask, branchPred, "edge_" + BB.getName().str() + "." + std::to_string(succIdx));
    auto maskShape = vecInfo.getVectorShape(blockMask);
    auto branchShape = vecInfo.getVectorShape(branchPred);
    vecInfo.setVectorShape(*edgeMask, VectorShape::join(maskShape, branchShape));
  }

  setEdgeMask(BB, succIdx, *edgeMask);
  IF_DEBUG_ME errs() << "\t" << *edgeMask << "\n";
  return *edgeMask;
}

Value&
MaskExpander::requestBlockMask(BasicBlock & BB) {
  assert(vecInfo.inRegion(BB));

  IF_DEBUG_ME { errs() << "# requestBlockMask( " << BB.getName() << ")\n"; }

  // region entry mask
  // - WFV mode: use the mask arg if available
  // - region mode: true
  auto & entryBlock = vecInfo.getEntry();
  if (&BB == &entryBlock) {
    IF_DEBUG_ME {errs() << "region entryMask: true\n"; }
    setBlockMask(BB, *trueConst);
    return *trueConst;
  }

  // return the cached result
  auto * mask = getBlockMask(BB);
  if (mask) return *mask;

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
    setBlockMask(BB, loopEdgeMask);
    return loopEdgeMask;
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

      setBlockMask(BB, idomMask);
      // edgePhi->eraseFroParent(); // FIXME
      return idomMask;
    }
  }

  // single join phi for uniform masks
  int numPreds = std::distance(itBegin, itEnd);
  auto * uniPhi = PHINode::Create(boolTy, numPreds, "inmasks_" + BB.getName(), &*BB.begin());

  std::vector<Value*> orVec; // incoming divergent edge masks

  // factor in all edges
  Value * lastUniIn = nullptr;
  bool redundantUniPhi = true;
  size_t num_incomming_values = 0;
  VectorShape phiShape = VectorShape::uni();

  for (auto itPred = itBegin; itPred != itEnd; ++itPred) {
    auto * predBlock = *itPred;

    IF_DEBUG_ME { errs() << "\t" << predBlock->getName() << ": "; }

    auto & predMask = requestBlockMask(*predBlock);

    // check if the dependence to cdBlock is uniform
    auto controlShape = VectorShape::join(vecInfo.getVectorShape(predMask), vecInfo.getVectorShape(*predBlock->getTerminator()));
    bool uniBranch = controlShape.isUniform();

  // uniform predecessor branch
#if 1
    if (uniBranch) {
      IF_DEBUG_ME { errs() << " uni " << *uniPhi << "\n"; }
      redundantUniPhi &= (lastUniIn == nullptr) || (lastUniIn == &predMask);
      lastUniIn = &predMask;
      uniPhi->addIncoming(&predMask, predBlock);
      num_incomming_values++;
      phiShape = VectorShape::join(vecInfo.getVectorShape(predMask), phiShape);
    }
#endif

    // select all edges of predBlock that lead to BB
    IndexSet predIndices;
    auto & predTerm = *predBlock->getTerminator();
    getPredecessorEdges(predTerm, BB, predIndices);
    auto * edgeMask = &requestJoinedEdgeMask(predTerm, predIndices);

    // mask is already accounted for by phi
    if (uniBranch) continue;

  // branch divergence is caused by immediate predecessor -> compute predicate
    IF_DEBUG_ME { errs() << " -> div from " << predBlock->getName() << " mask " << *edgeMask << "\n"; }

    auto * edgeMaskInst = dyn_cast<Instruction>(edgeMask);

    // generate a dominating definition (if necessary)
    if (edgeMaskInst) {

      if (!DT.dominates(edgeMaskInst->getParent(), &BB)) {
        std::string defBlockName = edgeMaskInst->getParent()->getName().str();
        auto edgeMaskShape = vecInfo.getVectorShape(*edgeMask);
        auto itInsert = &*BB.begin();
        auto * edgePhi = PHINode::Create(boolTy, numPreds, "edgemask_domphi_" + defBlockName, itInsert);
        vecInfo.setVectorShape(*edgePhi, edgeMaskShape);
        for (auto it = itBegin; it != itEnd; ++it) {
          Value * inVal = falseConst;
          if (*it == *itPred) {
            inVal = edgeMaskInst;
          }
          edgePhi->addIncoming(inVal ,*it);
        }
        // phi is reaching def
        edgeMask = edgePhi;
      }
    }

    orVec.push_back(edgeMask);
    uniPhi->addIncoming(falseConst, predBlock);
    redundantUniPhi &= (lastUniIn == nullptr) || (lastUniIn == falseConst);
    lastUniIn = falseConst;
  }

// start constructing the block mask
  Value * blockMask = nullptr;

  IF_DEBUG_ME { errs() << "\t mask(" << BB.getName() << ") = \n"; }
  IF_DEBUG_ME { errs() << "\t\t uniPhi: " << *uniPhi << " redundant " << redundantUniPhi << "\n"; }
  size_t startIdx = 0;
  if (num_incomming_values == 0) {
    assert((!lastUniIn || lastUniIn == falseConst) && "uniform inputs but no incoming values in uni phi");
    // no uniform inputs
    uniPhi->eraseFromParent();
    uniPhi = nullptr;
    blockMask = orVec[startIdx++];
  } else if (redundantUniPhi) {
    // all uniform inputs are identical -> erase the phi
    blockMask = lastUniIn;
    uniPhi->eraseFromParent();
    uniPhi = nullptr;
  } else {
    // multiple uniform inputs
    blockMask = uniPhi;
  }

  // if we keep the phi set its shape
  if (uniPhi) {
    vecInfo.setVectorShape(*uniPhi, phiShape);
  }

  // join all incoming varying masks
  IRBuilder<> builder(&BB, BB.getFirstInsertionPt());
  VectorShape maskShape = vecInfo.getVectorShape(*blockMask);
  for (size_t i = startIdx; i < orVec.size(); ++i) {
    auto divShape = vecInfo.getVectorShape(*orVec[i]);
    maskShape = VectorShape::join(maskShape, divShape);

    blockMask = &CreateOr(builder, *blockMask, *orVec[i]);
    vecInfo.setVectorShape(*blockMask, maskShape);
  }

  IF_DEBUG_ME { errs() << "> " << *blockMask << "\n"; }

  // cache the mask
  setBlockMask(BB, *blockMask);
  return *blockMask;
}


void
MaskExpander::setEdgeMask(BasicBlock & BB, int succIdx, Value & mask) {
  requestEdgePred(BB, succIdx).edgeMask = &mask;
}

void
MaskExpander::setBranchMask(BasicBlock & BB, int succIdx, Value & mask) {
  requestEdgePred(BB, succIdx).branchMask = &mask;
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
    vecInfo.setPredicate(BB, blockMask);
  }

  // simplify constructed predicates, if possible.
  // setup LLVM analysis infrastructure
  LoopAnalysisManager LAM;
  FunctionAnalysisManager FAM;
  CGSCCAnalysisManager CGAM;
  ModuleAnalysisManager MAM;

  PassBuilder PB;
  PB.registerModuleAnalyses(MAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

  const SimplifyQuery Q = getBestSimplifyQuery(FAM, vecInfo.getScalarFunction());
  for (auto & BB : vecInfo.getScalarFunction()) {
    auto blockMask = vecInfo.getPredicate(BB);
    if (blockMask && isa<Instruction>(blockMask)) {
      auto simplMask = simplifyInstruction(cast<Instruction>(blockMask), Q);
      if (simplMask) {
        vecInfo.setPredicate(BB, *simplMask);

        VectorShape maskShape = vecInfo.getVectorShape(*blockMask);
        VectorShape simplShape = vecInfo.getVectorShape(*simplMask);
        if (maskShape.morePreciseThan(simplShape))
          vecInfo.setVectorShape(*simplMask, maskShape);
      }
    }
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


}
