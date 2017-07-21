//===- maskExpander.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#include "rv/transform/maskExpander.h"
#include "rvConfig.h"

#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "llvm/IR/Verifier.h"

#include <cassert>

#if 1
#define IF_DEBUG_ME IF_DEBUG
#else
#define IF_DEBUG_ME if (false)
#endif


// use selects for mask arithmetic
// #define RV_BLEND_MASKS

using namespace llvm;

namespace rv {

static
Value&
CreateAnd(IRBuilder<> & builder, Value & lhs, Value & rhs, const Twine & name=Twine()) {
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

MaskExpander::MaskExpander(VectorizationInfo & _vecInfo, const DominatorTree & _domTree, const llvm::PostDominatorTree & _postDomTree, const llvm::LoopInfo & _loopInfo)
: vecInfo(_vecInfo)
, domTree(_domTree)
, postDomTree(_postDomTree)
, loopInfo(_loopInfo)
, boolTy(Type::getInt1Ty(vecInfo.getContext()))
, trueConst(ConstantInt::getTrue(vecInfo.getContext()))
, falseConst(ConstantInt::getFalse(vecInfo.getContext()))
{
  // initialize edge masks
  for (auto & BB :vecInfo.getScalarFunction()) {
    if (!vecInfo.inRegion(BB)) continue;
     edgeMasks[&BB] = std::vector<EdgePred>(BB.getTerminator()->getNumSuccessors(), EdgePred());
  }
}

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
MaskExpander::getPredecessorEdges(const TerminatorInst & termInst, const  BasicBlock & BB, IndexSet & oIndices) const {
  for (size_t i = 0; i < termInst.getNumSuccessors(); ++i) {
    auto * succBlock = termInst.getSuccessor(i);
    if (succBlock != &BB) continue;
    oIndices.push_back(i);
  }
}

static Value*
MatchMaskIntrinsic(Value & condVal) {
  auto * call = dyn_cast<CallInst>(&condVal);
  if (!call) return nullptr;

  auto * callee = dyn_cast<Function>(call->getCalledValue());
  if (!callee) return nullptr;

  if (callee->getName() == "rv_any") {
    return call->getArgOperand(0);
  }

  return nullptr;
}

Value &
MaskExpander::requestBranchMask(TerminatorInst & term, int succIdx, IRBuilder<> & builder) {
  auto & sourceBlock = *term.getParent();
  IF_DEBUG_ME { errs() << "# requestBranchMask( " << sourceBlock.getName() << ", " << succIdx << ")\n"; }
  auto * cached = getBranchMask(term, succIdx);
  if (cached) return *cached;

  if (isa<BranchInst>(term)) {
    assert(isa<BranchInst>(term) && "TODO implement switches");
    auto & branch = cast<BranchInst>(term);
    auto * condVal = branch.isConditional() ? branch.getCondition() : trueConst;

    // look through rv_any calls
    Value * actualCond = MatchMaskIntrinsic(*condVal);
    if (actualCond) {
      errs() << "maskEx: recovered mask condition " << *actualCond <<" from " << *condVal << "\n";
      condVal = actualCond;
    }

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
      auto & caseVal = *itCase.getCaseValue();
      auto & caseCmp = *builder.CreateICmp(ICmpInst::ICMP_EQ, &caseVal, &switchVal, "caseeq_" + std::to_string(caseVal.getSExtValue()));
      vecInfo.setVectorShape(caseCmp, valShape);
      setBranchMask(sourceBlock, succIdx, caseCmp);
      return caseCmp;
    }
  }
  assert(false && "unsupported terminator!");
  abort();
}

Value &
MaskExpander::requestJoinedEdgeMask(TerminatorInst & term, IndexSet succIdx) {
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

Value&
MaskExpander::requestEdgeMask(TerminatorInst & term, int succIdx) {
  auto *cachedMask = getEdgeMask(term, succIdx);
  if (cachedMask) return *cachedMask;

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
    auto & mapping =vecInfo.getMapping();
    if (mapping.maskPos >= 0) {
      auto itArg = mapping.scalarFn->getArgumentList().begin();
      std::advance(itArg, mapping.maskPos);
      IF_DEBUG_ME {errs() << "entryMask: " << *itArg << "\n"; }
      setBlockMask(BB, *itArg);
      return *itArg;
    } else {
      IF_DEBUG_ME {errs() << "region entryMask: true\n"; }
      setBlockMask(BB, *trueConst);
      return *trueConst;
    }
  }

  // return the cached result
  auto * mask = getBlockMask(BB);
  if (mask) return *mask;

  // Otw, start buildling a mask
  IF_DEBUG_ME { errs() << "Construct mask:\n"; }

  // if this is a uniform loop use the preheader edge mask
  auto * bbLoop = loopInfo.getLoopFor(&BB);
  if (bbLoop && (&BB == bbLoop->getHeader())) {
    assert(!vecInfo.isDivergentLoop(bbLoop) && "maskExpander can not handle divergent loops directly! (use divLoopTrans)");
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
  auto * domNode = domTree.getNode(&BB);
  auto &idomBlock = *domNode->getIDom()->getBlock();

// try to re-use the SESE entry mask
  if (vecInfo.inRegion(idomBlock) && postDomTree.dominates(&BB, &idomBlock)) {
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

      if (!domTree.dominates(edgeMaskInst->getParent(), &BB)) {
        std::string defBlockName = edgeMaskInst->getParent()->getName();
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
  }

// start constructing the block mask
  Value * blockMask = nullptr;

  IF_DEBUG_ME { errs() << "\t mask(" << BB.getName() << ") = \n"; }
  IF_DEBUG_ME { errs() << "\t\t uniPhi: " << *uniPhi << " redundant " << redundantUniPhi << "\n"; }
  size_t startIdx = 0;
  if (uniPhi->getNumIncomingValues() == 0) {
    assert(!lastUniIn && "uniform inputs but no incoming values in uni phi");
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
MaskExpander::gatherLoopDivergenceInfo(Loop & loop, VectorShape & oLoopDivergenceShape, EdgeVec & oDivergentExits, EdgeVec & oKillExits) {
  SmallVector<Loop::Edge, 4> exitEdges;
  loop.getExitEdges(exitEdges);

  // TODO find the appropriate shape for this tracker
  oLoopDivergenceShape = vecInfo.isDivergentLoop(&loop) ? VectorShape::varying() : VectorShape::uni(); // TODO infer loop shape // doesn't work: vecInfo.getVectorShape(header);

  for (auto & exitEdge : exitEdges) {
    auto & exitingBlock = *const_cast<BasicBlock*>(exitEdge.first); // TODO
    auto & exitBlock = *const_cast<BasicBlock*>(exitEdge.second); // TODO

    // TODO infer loop divergence shape

    // filter out exits from nested loops
    if (loopInfo.getLoopFor(&exitingBlock) != &loop) {
      // skip exits from nested loops
      continue;
    }

    // categorize exits from this loop
    if (vecInfo.isKillExit(exitBlock)) {
      oKillExits.push_back(exitEdge);
    } else {
      oDivergentExits.push_back(exitEdge);
    }
  }
}

// if P and C are on a loop tree path this returns whether C is a proper child loop of P
static bool
IsInsideLoop(Loop * P, Loop * C) {
  if (!P) return true; // all loops are in the parent loop
  if (!C) return true; // parent loop contains parent loop
  return P->getLoopDepth() <= C->getLoopDepth(); // C is on a nested loop level
}

// TODO this code currently generates full predication for all loops in the region
Loop*
MaskExpander::requestLoopMasks(Loop & loop) {
  IF_DEBUG_ME { errs () << "# requestLoopMask  " << loop.getName() << "\n"; }

  // only process loops within the region
  bool regionLoop =
    (!vecInfo.inRegion(*loop.getHeader())) ||
    (loop.getHeader() == &vecInfo.getEntry()); // skip the region loop

  if (regionLoop) {
    IF_DEBUG_ME { errs () << "- not in region loop  " << loop.getName() << "\n"; }

    for (auto * childLoop : loop) {
      requestLoopMasks(*childLoop);
    }
    return &loop;
  }

  // loop is nested in region and may need predication

  IF_DEBUG_ME { errs () << "- loop  " << loop.getName() << "\n"; }


  // query the divergence shape of this loop
  auto & preHeader = *loop.getLoopPreheader();
  auto & header = *loop.getHeader();
  auto & latchBlock = *loop.getLoopLatch();

  // TODO find the appropriate shape for this tracker
  VectorShape loopDivShape = VectorShape::varying(); // TODO infer loop shape // doesn't work: vecInfo.getVectorShape(header);

  EdgeVec uniformExits;
  EdgeVec divergentExits;
  gatherLoopDivergenceInfo(loop, loopDivShape, divergentExits, uniformExits);

  // create a loop live mask
  std::string headerPhiName = "live_" + loop.getName().str();
  auto & loopMaskPhi = *PHINode::Create(boolTy, 2, headerPhiName, &*loop.getHeader()->begin());
  vecInfo.setVectorShape(loopMaskPhi, loopDivShape);

  // register this mask before descending into children
  setBlockMask(header, loopMaskPhi);
  setLoopLiveMask(loop, loopMaskPhi);

  Loop * outerDivExitLoop = &loop; // outer most loop reached by divergent exits of child loops
  // TODO look at outerDivExitLoop AND loopDivShape to identify uniform loops without any divergence-leaking edges

// materialize all masks in child loops first
  // on the way, find the outer most loop that contains a divergent exit of the child loops (outerDivExitLoop)
  for (auto * childLoop : loop) {
    auto * childOuterMostExitLoop = requestLoopMasks(*childLoop);
    if (IsInsideLoop(childOuterMostExitLoop, outerDivExitLoop)) {
      outerDivExitLoop = childOuterMostExitLoop;
    }
  }

// check if we can avoid a loop live mask (TODO)

// Create a loop live mask and exit masks for all non-kill exits
  // SmallVector<Loop::Edge, 4> exitEdges;
  // loop.getExitEdges(exitEdges);

#if 1
  // nothing todo about uniform exits?
  // TODO we may need LCSSA phis if outside divergence taps on liveout masks
  for (auto & uniExitEdge : uniformExits) {
    auto & exitingBlock = *const_cast<BasicBlock*>(uniExitEdge.first); // TODO
    auto & exitBlock = *const_cast<BasicBlock*>(uniExitEdge.second); // TODO

    IndexSet exitIndices;
    auto & exitingTerm = *exitingBlock.getTerminator();
    getPredecessorEdges(exitingTerm, exitBlock, exitIndices);

    requestJoinedEdgeMask(exitingTerm, exitIndices);
  }
#endif

  //
  for (auto & exitEdge : divergentExits) {
    auto & exitingBlock = *const_cast<BasicBlock*>(exitEdge.first); // TODO
    auto & exitBlock = *const_cast<BasicBlock*>(exitEdge.second); // TODO

    // create two phis:
    // - a LCSSA phi to track the liveout mask of this divergent loop exit
    // - a phi that tracks all edges that have left through this exit
    // tracker phi (loop header)
    std::string trackerPhiName = "exitmask_track_" + exitingBlock.getName().str();
    std::string updateTrackerName = "exitmask_upd_" + exitingBlock.getName().str();
    auto & trackerPhi = *PHINode::Create(boolTy, 1, trackerPhiName, &*header.begin());
    trackerPhi.addIncoming(falseConst, &preHeader);
    vecInfo.setVectorShape(trackerPhi, loopDivShape); // FIXME set the actual exit shape

    // update outermost exit loop info
    auto * exitLoop = loopInfo.getLoopFor(&exitBlock);
    outerDivExitLoop = IsInsideLoop(outerDivExitLoop, exitLoop) ? outerDivExitLoop : exitLoop;

    // request the exiting mask
    IndexSet exitIndices;
    auto & exitingTerm = *exitingBlock.getTerminator();
    getPredecessorEdges(exitingTerm, exitBlock, exitIndices);
    auto & exitTakenMask = requestJoinedEdgeMask(exitingTerm, exitIndices);
    auto exitTakenShape = vecInfo.getVectorShape(exitTakenMask);

    // update the exit mask phi for this exit (inside loop)
    IRBuilder<> builder(&exitingBlock, exitingTerm.getIterator());
    auto * trackerUpdate = &CreateOr(builder, exitTakenMask, trackerPhi, updateTrackerName);
    vecInfo.setVectorShape(*trackerUpdate, exitTakenShape);
    assert(domTree.dominates(&exitingBlock, &latchBlock) && "TODO implement exit mask repair");

    // loop exit mask (outside)
#if 0
    // exit block mask (LCSSA phi)
    // FIXME we can not create a LCSSA phi here since the Linearizer will treat is as payload and start blending in undef values (since a varying value is leaking from a divergent loop)
    std::string exitPhiName = "mask.lcssa_" + exitingBlock.getName().str();
    auto & loopExitPhi = *PHINode::Create(boolTy, 1, exitPhiName, &*exitBlock.begin());
    vecInfo.setVectorShape(loopExitPhi, loopDivShape); // FIXME

    // attach liveout update to exit
    loopExitPhi.addIncoming(trackerUpdate, &exitingBlock);

    setBlockMask(exitBlock, loopExitPhi); // receives the exitBlock mask

#else
    // do not create an LCSSA phi to not be treated a leaking varying liveout in the Linearizer (this value is control-uniform and only value divergent)
    setBlockMask(exitBlock, *trackerUpdate); // receives the exitBlock mask
#endif

    // promote exit update to latch
    SmallVector<PHINode*, 8> phiVec;
    SSAUpdater ssaUpdater(&phiVec);
    ssaUpdater.Initialize(boolTy, updateTrackerName +".prom");
    ssaUpdater.AddAvailableValue(&header, &trackerPhi);
    ssaUpdater.AddAvailableValue(&exitingBlock, trackerUpdate);
    auto * promotedUpdate = ssaUpdater.GetValueAtEndOfBlock(&latchBlock);

    // promote shapes
    for (auto * phi : phiVec) {
      vecInfo.setVectorShape(*phi, exitTakenShape);
    }

    // attach promoted update to tracker phi
    trackerPhi.addIncoming(promotedUpdate, &latchBlock); // TODO promote definition
  }

  // attach live mask update to mask phi
  IndexSet latchIndices;
  getPredecessorEdges(*latchBlock.getTerminator(), header, latchIndices);
  auto & updatedLiveMask = requestJoinedEdgeMask(*latchBlock.getTerminator(), latchIndices);
  loopMaskPhi.addIncoming(&updatedLiveMask, &latchBlock);

  // return the outer most divergent exit level
  std::string outerMostName = outerDivExitLoop ? outerDivExitLoop->getName().str() : "<none>";

  IF_DEBUG_ME { errs () << "# DONE requestLoopMask  " << loop.getName() << " outer most div exit: " << outerMostName << "\n";}

  return outerDivExitLoop;
}

void
MaskExpander::setEdgeMask(BasicBlock & BB, int succIdx, Value & mask) {
  auto it = edgeMasks.find(&BB);
  if (it != edgeMasks.end()) {
    if (it->second.size() <= (size_t) succIdx) {
      it->second.resize(succIdx + 1);
    }
  } else {
    size_t minSize = std::max(succIdx + 1, 4);
    edgeMasks[&BB] = std::vector<EdgePred>(minSize);
  }

  edgeMasks[&BB][succIdx].edgeMask = &mask;
}

void
MaskExpander::setBranchMask(BasicBlock & BB, int succIdx, Value & mask) {
  auto it = edgeMasks.find(&BB);
  if (it != edgeMasks.end()) {
    if (it->second.size() <= (size_t) succIdx) {
      it->second.resize(succIdx + 1);
    }
  } else {
    edgeMasks[&BB] = std::vector<EdgePred>(succIdx + 1);
  }

  edgeMasks[&BB][succIdx].branchMask = &mask;
}

void
MaskExpander::patchLoopMasks() {
  IF_DEBUG_ME {
    errs() << "- loop entry masks -";
  }

  for (auto & it : loopPhis) {
    const auto & loop = *it.first;
    auto & headerPhi = *it.second;

    auto & preHeaderBlock = *loop.getLoopPreheader();
    IndexSet loopEntryIndices;
    auto & preTerm = *preHeaderBlock.getTerminator();
    getPredecessorEdges(preTerm, *headerPhi.getParent(), loopEntryIndices);
    auto & loopEntryMask = requestJoinedEdgeMask(preTerm, loopEntryIndices);
    headerPhi.addIncoming(&loopEntryMask, &preHeaderBlock);
  }
}

void
MaskExpander::expandRegionMasks() {
  IF_DEBUG_ME {
    errs() << "-- Region before MaskExpander --\n";
    vecInfo.dump();
    errs() << "-- MaskExpander log --\n";
  }

#if 0
  // FIXME moved to divLoopTrans where required
  // expand loops (and the masks they need)
  IF_DEBUG_ME { errs() << "- expanding loop masks\n"; }
  for (auto * loop : loopInfo) {
    requestLoopMasks(*loop);
  }
#endif

  // eagerly all remaining masks
  IF_DEBUG_ME { errs() << "- expanding acyclic masks\n"; }
  for (auto & BB : vecInfo.getScalarFunction()) {
    if (!vecInfo.inRegion(BB)) continue;

    auto & blockMask = requestBlockMask(BB);
    vecInfo.setPredicate(BB, blockMask);
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
