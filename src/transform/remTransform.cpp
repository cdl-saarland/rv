//===- src/transform/remTransform.cpp - scalar remainder-loop generator --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/transform/remTransform.h"

#include "rv/analysis/reductionAnalysis.h"
#include "rv/vectorizationInfo.h"
#include "rv/transform/loopCloner.h"
#include "rv/utils.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Instructions.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"

#include "rvConfig.h"
#include "rv/rvDebug.h"
#include "report.h"


#include <map>
#include <set>

#if 1
#define IF_DEBUG_REM IF_DEBUG
#else
#define IF_DEBUG_REM if (true)
#endif

using namespace llvm;

namespace rv {

// TODO: Move to CFGTools module.
static BasicBlock *splitPreheaderEdge(BasicBlock &From, int BranchIdx, DominatorTree &DT,
                             PostDominatorTree &PDT, LoopInfo &LI) {
  auto *F = From.getParent();
  auto &FromTerm = *From.getTerminator();
  BasicBlock *To = FromTerm.getSuccessor(BranchIdx);
  auto *SplitBlock =
      BasicBlock::Create(From.getContext(), From.getName() + ".split", F, To);
  BranchInst::Create(To, SplitBlock);
  FromTerm.setSuccessor(BranchIdx, SplitBlock);
  for (auto &Phi : To->phis())
    Phi.replaceIncomingBlockWith(&From, SplitBlock);

  // Fix LI.
  auto *FromLoop = LI.getLoopFor(&From);
  auto *ToLoop = LI.getLoopFor(To);

  // Preheader edge (from parent loop).
  if (FromLoop && ToLoop && (FromLoop->getLoopDepth() < ToLoop->getLoopDepth()))
    FromLoop->addBasicBlockToLoop(SplitBlock, LI);
  // Same loop level or exiting edge.
  if (FromLoop && ToLoop &&
      (FromLoop->getLoopDepth() >= ToLoop->getLoopDepth()))
    ToLoop->addBasicBlockToLoop(SplitBlock, LI);

  // Fix DT (FIXME: branch to loop header only atm).
  auto *SplitDT = DT.addNewBlock(SplitBlock, &From);
  DT.changeImmediateDominator(DT.getNode(To), SplitDT);

  // Fix PDT.
  auto *SplitPDT = PDT.addNewBlock(SplitBlock, To);
  PDT.changeImmediateDominator(SplitPDT, PDT.getNode(To));
  return SplitBlock;
}

struct IterValue {
  Value & val;
  int timeOffset;

  IterValue(Value & _val, int _timeOff)
  : val(_val)
  , timeOffset(_timeOff)
  {}
};

static Value*
UnwindCasts(Value* val) {
  auto * castInst = dyn_cast<CastInst>(val);

  while (castInst) {
     val = castInst->getOperand(0);
     castInst = dyn_cast<CastInst>(val);
  }

  return val;
}

class
BranchCondition { // LoopExitCondition
  bool loopExitOnTrue; // exit taken if \p evaluates to true
  bool exitWhenEqual; // exit on phi == n
  CmpInst & cmp; // the original comparison
  CmpInst::Predicate adjustedPred; // predicate to be used
  int cmpReductIdx;
  StridePattern & sp;
  VectorShape redShape;

public:

  bool
  exitsOnTrue() const { return loopExitOnTrue; }

  BranchCondition(bool _loopExitOnTrue, bool _exitWhenEqual, llvm::CmpInst & _cmp, CmpInst::Predicate _adjustedPred, int _cmpReductIdx, StridePattern & _sp, int vectorWidth)
  : loopExitOnTrue(_loopExitOnTrue)
  , exitWhenEqual(_exitWhenEqual)
  , cmp(_cmp)
  , adjustedPred(_adjustedPred)
  , cmpReductIdx(_cmpReductIdx)
  , sp(_sp)
  , redShape(sp.getShape(vectorWidth))
  {}

  // return a branch condition object if this condition can be transformed
  static BranchCondition *
  analyze(llvm::BranchInst & loopExitBr, int vectorWidth, ReductionAnalysis & reda, Loop & loop) {
    if (!loopExitBr.isConditional()) {
      Report() << "loopExitCond: not an conditional loop exit!\n";
      return nullptr;
    }


  // match the loop exit condition to a ICmpInst of phi with a loop invariant bound.
    const bool loopExitOnTrue = !loop.contains(loopExitBr.getSuccessor(0));

    auto * testCmp = dyn_cast<ICmpInst>(loopExitBr.getOperand(0));
    if (!testCmp) {
      Report() << "loopExitCond: not an ICmpInst loop exit:\n\t";
      loopExitBr.print(Report(), true);
      return nullptr;
    }
    ICmpInst & cmp = *testCmp;

    int reductIdx = -1;
    StridePattern * red = nullptr;

    for (int i = 0; i < (int) cmp.getNumOperands(); ++i) {
      auto * opVal = cmp.getOperand(i);
      auto * inst = dyn_cast<Instruction>(opVal);

      if (!inst) continue;

      // step through sext and the like
      auto * baseVal = UnwindCasts(inst);
      inst = dyn_cast<Instruction>(baseVal);
      if (!inst) continue;

      // loop invariant operand
      if (!loop.contains(inst->getParent())) continue;

      auto * valRed = reda.getStrideInfo(*inst);
      if (!valRed) {
        Report() << "loopExitCond: is not an inductive stride " << *inst << "\n";
        // loop carried operand is not part of a recognized reduction -> abort
        return nullptr;
      }

      if (reductIdx > -1) {
        Report() << "loopExitCond: both cmp operands are loop carried " << *inst << "\n";
        return nullptr; // multiple loop carried values enter this cmp -> abort
      }

      reductIdx = i;
      red = valRed;
    }

    if (!red) {
      Report() << "loopExitCond: branch condition does not operate on inductive strides " << cmp << "\n";
      return nullptr;
    }

    const StridePattern & sp = *red;

  // analyze the loop exit condition
    bool tmpExitOnTrue = loopExitOnTrue;

    // this implicitly assumes that the loop iteration variable I is incremented by a constant C without wrapping
    const bool nswFlag = sp.reductor->hasNoSignedWrap();
    const bool nuwFlag = sp.reductor->hasNoUnsignedWrap();

    const auto cmpPred = cmp.getPredicate();
    CmpInst::Predicate adjustedPred = cmpPred;

    // (negatePhi) normalize to positive increments
    // "br (i < n), exit, loop" -> "br (-i > n), exit, loop"
    auto redShape = sp.getShape(vectorWidth);
    int incStep = redShape.getStride();
    bool negatePhi = false;
    if (incStep < 0) {
      negatePhi = true;
      adjustedPred = CmpInst::getSwappedPredicate(adjustedPred);
    }

    // br (i != n), A, B --> br (i == n) B, A
    if (cmpPred == CmpInst::ICMP_NE) {
      adjustedPred = CmpInst::ICMP_EQ; // for completeness
      tmpExitOnTrue = !tmpExitOnTrue;
    }

    // (exitWhenEqual) determine whether the exit will be taken on "phi == n"
    bool exitWhenEqual = false;
    if (
        // br (i < n), loop, exit
        (((cmpPred == CmpInst::ICMP_SLT) || (cmpPred == CmpInst::ICMP_ULT)) && !tmpExitOnTrue) ||
        // br (i >= n), exit, loop
        (((cmpPred == CmpInst::ICMP_SGE) || (cmpPred == CmpInst::ICMP_UGE)) && tmpExitOnTrue) ||
        // br (i == n), exit, loop
        ((cmpPred == CmpInst::ICMP_EQ) && tmpExitOnTrue)
    ) {
          exitWhenEqual = true;
    }

    // br (i == n), A, B --> br (i >= n), A, B
    if (cmpPred == CmpInst::ICMP_EQ) {
      assert(adjustedPred == CmpInst::ICMP_EQ);
      if (nswFlag) {
        adjustedPred = CmpInst::ICMP_SGE;
      } else {
        if (!nuwFlag) {
          Report() << "loopExitCond: could not legalize ICMP_EQ to ICMP_*GE!\n";
          return nullptr;
        }
        adjustedPred = CmpInst::ICMP_UGE;
      }
    }

    // return to orientation of the original branch (loopExitOnTrue)
    if (tmpExitOnTrue != loopExitOnTrue) {
      adjustedPred = CmpInst::getInversePredicate(adjustedPred);
      tmpExitOnTrue = loopExitOnTrue;
    }

    // replace "-phi < n" with "phi > n"
    if (negatePhi) {
      adjustedPred = CmpInst::getSwappedPredicate(adjustedPred);
    }

    // Cmp predicate tests before assumes that i is at index 0 and the 'n' at index 1.
    // Swap the predicate again if this is not the case.
    if (reductIdx != 0)
      adjustedPred = CmpInst::getSwappedPredicate(adjustedPred);

    return new BranchCondition(loopExitOnTrue, exitWhenEqual, cmp, adjustedPred, reductIdx, *red, vectorWidth);
  }

  /// re-synthesize this condition with builder @builder
  // call embedFunc for all loop carred instructions
  // the test will be true if it applies for all iterations from [phi to phi + iterOffset]
  Value&
  synthesize(int iterOffset, std::string suffix, IRBuilder<> & builder, std::set<Value*> * valueSet, std::function<IterValue (Instruction&)> embedFunc) {
    auto * origReduct = cast<Instruction>(cmp.getOperand(cmpReductIdx));

    // Step through cast (if any).
    Instruction* IterCastOp = nullptr;
    if (origReduct->isCast()) {
      IterCastOp = origReduct;
      origReduct = cast<Instruction>(origReduct->getOperand(0));
    }

    // used to keep track
    bool tmpExitOnTrue = loopExitOnTrue;

    // the returned value evaluates to the mapped requested value at iteration offset @embReduct.timeOffset
    IterValue embReduct = embedFunc(*origReduct);

    auto * clonedCmp = cast<CmpInst>(cmp.clone());

    // remaining offset amount
    int effectiveOffset = iterOffset + embReduct.timeOffset;

    // short cut for same iteration tests
    if (effectiveOffset == 0) {
      clonedCmp->setOperand(cmpReductIdx, &embReduct.val);
      builder.Insert(clonedCmp, cmp.getName().str() + suffix);
      return *clonedCmp;
    }

    // finalize the predicate
    clonedCmp->setPredicate(adjustedPred);

  // determine the phi offset to test
    assert(tmpExitOnTrue == loopExitOnTrue);
    int offset = redShape.getStride() * effectiveOffset;

    // dont test "phi", but "phi-1" if the cmp predicate is exit on equal
    int offByOne = redShape.getStride() < 0 ? -1 : 1;

    offset = offset + offByOne * (int) (exitWhenEqual);

  // emit the test
    auto & val = embReduct.val;

    // increment the iteration variable as necessary
    Value * adjusted = nullptr;
    if (offset != 0) {
      const bool nswFlag = sp.reductor->hasNoSignedWrap();
      const bool nuwFlag = sp.reductor->hasNoUnsignedWrap();
      adjusted = builder.CreateAdd(&val, ConstantInt::get(val.getType(), offset), "", nswFlag, nuwFlag);
    } else {
      adjusted = &val;
    }

    if (isa<Instruction>(adjusted)) if (valueSet) valueSet->insert(adjusted);

    // Re-insert the cast (as necessary).
    if (IterCastOp) {
      bool IsSExt = IterCastOp->getOpcode() == Instruction::SExt;
      if (IsSExt)
        adjusted = builder.CreateSExtOrTrunc(adjusted, IterCastOp->getType());
      else
        adjusted = builder.CreateZExtOrTrunc(adjusted, IterCastOp->getType());
    }

    // Synthesize a new cmp.
    clonedCmp->setOperand(cmpReductIdx, adjusted);
    builder.Insert(clonedCmp, cmp.getName().str() + suffix);

    if (valueSet) valueSet->insert(clonedCmp);

    return *clonedCmp;
  }

  /// synthesize an EVL that models this condition 
  // call embedFunc for all loop carred instructions
  // the test will be true if it applies for all iterations from [phi to phi + iterOffset]
  Value&
  synthesizeEVL(int iterOffset, std::string suffix, IRBuilder<> & builder, std::set<Value*> * valueSet, std::function<IterValue (Instruction&)> embedFunc) {
    auto * origReduct = cast<Instruction>(cmp.getOperand(cmpReductIdx));

    // Step through cast (if any).
    Instruction* IterCastOp = nullptr;
    if (origReduct->isCast()) {
      IterCastOp = origReduct;
      origReduct = cast<Instruction>(origReduct->getOperand(0));
    }

    // used to keep track
    bool tmpExitOnTrue = loopExitOnTrue;

    // the returned value evaluates to the mapped requested value at iteration offset @embReduct.timeOffset
    IterValue embReduct = embedFunc(*origReduct);

    // remaining offset amount
    int effectiveOffset = iterOffset + embReduct.timeOffset;

#if 0
    // short cut for same iteration tests
    if (effectiveOffset == 0) {
      clonedCmp->setOperand(cmpReductIdx, &embReduct.val);
      builder.Insert(clonedCmp, cmp.getName().str() + suffix);
      return *clonedCmp;
    }
#endif

  // determine the phi offset to test
    assert(tmpExitOnTrue == loopExitOnTrue);
    int offset = redShape.getStride() * effectiveOffset;

    // dont test "phi", but "phi-1" if the cmp predicate is exit on equal
    int offByOne = redShape.getStride() < 0 ? -1 : 1;

    offset = offset + offByOne * (int) (exitWhenEqual);

  // emit the test
    auto & val = embReduct.val;

    // increment the iteration variable as necessary
    Value * adjusted = nullptr;
    if (offset != 0) {
      const bool nswFlag = sp.reductor->hasNoSignedWrap();
      const bool nuwFlag = sp.reductor->hasNoUnsignedWrap();
      adjusted = builder.CreateAdd(&val, ConstantInt::get(val.getType(), offset), "", nswFlag, nuwFlag);
    } else {
      adjusted = &val;
    }

    // Re-insert the cast (as necessary).
    if (IterCastOp) {
      bool IsSExt = IterCastOp->getOpcode() == Instruction::SExt;
      if (IsSExt)
        adjusted = builder.CreateSExtOrTrunc(adjusted, IterCastOp->getType());
      else
        adjusted = builder.CreateZExtOrTrunc(adjusted, IterCastOp->getType());
    }

   // subtract the bound to obtain a lane offset.
    auto LaneID = builder.CreateSub(cmp.getOperand(1 - cmpReductIdx) , adjusted, "laneid");
    return *LaneID;
  }
};


typedef std::map<PHINode*, PHINode*> PHIMap;
typedef std::map<Instruction*, Instruction*> InstMap;




struct LoopTransformer {
  Function & F;
  DominatorTree & DT;
  PostDominatorTree & PDT;
  LoopInfo & LI;

  Loop & ScalarL;
  Loop & ClonedL;

  // use tail predication (instead of branching to the scalar loop)
  bool useTailPredication;
  Value * AVL; // computed AVL (only set if useTailPredication is set)

  // exit condition builder for the vectorized loop
  BranchCondition & exitConditionBuilder;

  ValueToValueMapTy & vecValMap;
  ReductionAnalysis & reda;
  std::set<Value*> & uniOverrides;

  int vectorWidth;
  int tripAlign;

  // - original loop -
  //
  // entry
  //   |
  // ScalarL <>
  //   |
  //  Exit
  //
  // - transformed loop -
  // entry --> ... // entry branch to the loop
  //   |
  // vecGuard // cost model block and VectorL preheader
  //  |   \
  //  |   VectorL <> // vector loop produced by RV
  //  |    |
  //  |   VecToScalar --> Exit  // reduces vector values to scalars
  //  |   /
  //  ScalarGuard //  scalar loop preHeader
  //    |
  //  ScalarL <> --> Exit

// scalar loop context
  // the old preheader
  BasicBlock * entryBlock;

  // the single loop exit
  BasicBlock * loopExit;

// embedding blocks
  // either dispatches to the vectorized or the scalar loop
  BasicBlock * vecGuardBlock;

  // entry point to the scalar loop (scalar loop prehader)W
  BasicBlock * scalarGuardBlock;

  // exit of the vector loop to the scalar guard
  BasicBlock * vecToScalarExit;

  static
  BasicBlock*
  GetUniqueExiting(llvm::Loop & L) {
    auto * exitBlock = L.getExitBlock();
    for (auto & use : exitBlock->uses()) {
      auto * exitingBr = dyn_cast<BranchInst>(use.getUser());
      if (!exitingBr) continue;
      if (!L.contains(exitingBr->getParent())) continue;

      return exitingBr->getParent();
    }
    return nullptr;
  }

  LoopTransformer(Function & _F, DominatorTree & _DT, PostDominatorTree & _PDT, LoopInfo & _LI, ReductionAnalysis & _reda, std::set<Value*> & _uniOverrides, BranchCondition & _exitBuilder, Loop & _ScalarL, Loop & _ClonedL, ValueToValueMapTy & _vecValMap, bool _useTailPredication, int _vectorWidth, int _tripAlign)
  : F(_F)
  , DT(_DT)
  , PDT(_PDT)
  , LI(_LI)
  , ScalarL(_ScalarL)
  , ClonedL(_ClonedL)
  , useTailPredication(_useTailPredication)
  , AVL(nullptr)
  , exitConditionBuilder(_exitBuilder)
  , vecValMap(_vecValMap)
  , reda(_reda)
  , uniOverrides(_uniOverrides)
  , vectorWidth(_vectorWidth)
  , tripAlign(_tripAlign)
  , entryBlock(ScalarL.getLoopPreheader())
  , loopExit(ScalarL.getExitBlock())
  , vecGuardBlock(nullptr)
  , scalarGuardBlock(nullptr)
  , vecToScalarExit(nullptr)
  {

    assert(loopExit && "multi exit loops unsupported (yet)");
    assert(ScalarL.getExitingBlock() && "Scalar loop does not have a unique exiting block (unsupported)");

    // create all basic blocks
    setupControl();

    // repair value flow through the blocks
    repairValueFlow();
  }

  // set-up the vector loop CFG
  void
  setupControl() {
    auto & scalarHead = *ScalarL.getHeader();
    auto & context = scalarHead.getContext();
    auto & vecHead = LookUp(vecValMap, scalarHead);

    std::string loopName = ScalarL.getName().str();
    vecGuardBlock = BasicBlock::Create(context, loopName + ".vecg", &F, &scalarHead);
    scalarGuardBlock = BasicBlock::Create(context, loopName + ".scag", &F, &scalarHead);
    vecToScalarExit = BasicBlock::Create(context, loopName + ".vec2scalar", &F, &scalarHead);

    auto * parentLoop = ScalarL.getParentLoop();
    if (parentLoop) {
      parentLoop->addBasicBlockToLoop(vecGuardBlock, LI);
      parentLoop->addBasicBlockToLoop(scalarGuardBlock, LI);
      parentLoop->addBasicBlockToLoop(vecToScalarExit, LI);
    }

  // branch to vecGuard instead to the scalar loop
    auto * entryTerm = entryBlock->getTerminator();
    for (size_t i = 0; i < entryTerm->getNumSuccessors(); ++i) {
      if (&scalarHead == entryTerm->getSuccessor(i)) {
        entryTerm->setSuccessor(i, vecGuardBlock);
        break;
      }
    }

  // dispatch to vector loop header or the scalar guard
    Value * constTrue = ConstantInt::getTrue(context);
    Value * constFalse = ConstantInt::getFalse(context);
    // TODO skip the vector loop if iteration count to low
    auto * vecLoopCond = constTrue; 

    if (exitConditionBuilder.exitsOnTrue()) {
      BranchInst::Create(scalarGuardBlock, &vecHead, vecLoopCond, vecGuardBlock);
    } else {
      BranchInst::Create(&vecHead, scalarGuardBlock, vecLoopCond, vecGuardBlock);
    }

  // make the vector loop exit to vecToScalar
    auto * scaExiting = ScalarL.getExitingBlock();
    auto * scalarTerm = scaExiting->getTerminator();
    auto * vecLoopExiting = &LookUp(vecValMap, *scalarTerm->getParent());
    auto * vecTerm = cast<Instruction>(vecValMap[scalarTerm]);

    for (size_t i = 0; i < scalarTerm->getNumSuccessors(); ++i) {
      if (scalarTerm->getSuccessor(i) != loopExit) continue;
      vecTerm->setSuccessor(i, vecToScalarExit);
      break;
    }

  // branch from vecToScalarExit to the scalarGuard
    // TODO add an early exit branch (whenever no iterations remain)
    auto * remainderCond = useTailPredication ? constFalse : constTrue;
    BranchInst::Create(scalarGuardBlock, loopExit, remainderCond, vecToScalarExit);

  // make scalarGuard the new preheader of the scalar loop
    BranchInst::Create(&scalarHead, scalarGuardBlock);

  // update DomTree
    bool scaLoopDominatedExit = DT.dominates(scaExiting, loopExit);
    auto * vecGuardDomNode = DT.addNewBlock(vecGuardBlock, entryBlock); // entry >= vecGuardBlock
    DT.changeImmediateDominator(DT.getNode(&vecHead), vecGuardDomNode); // vecGuardBlock >= vecLoopHead
    auto * scaGuardNode = DT.addNewBlock(scalarGuardBlock, vecGuardBlock); // vecGuardBlock >= scaGuarBlock
    DT.changeImmediateDominator(DT.getNode(&scalarHead), scaGuardNode);
    DT.addNewBlock(vecToScalarExit, vecLoopExiting); // vecLoopExiting >= vecToScalarExit

    if (scaLoopDominatedExit) {
      DT.changeImmediateDominator(loopExit, vecGuardBlock);
    }

  // update postDomTree
    bool loopPostDomsEntry = PDT.dominates(&scalarHead, entryBlock);
    auto * vecLoopExitingPostDom = PDT.getNode(vecLoopExiting);
    PDT.addNewBlock(scalarGuardBlock, &scalarHead); // scalarHead >= scaGuardBlock
    auto * vecToScalarPostDom = PDT.addNewBlock(vecToScalarExit, loopExit); // loopExit >= vecToScalar
    PDT.changeImmediateDominator(vecLoopExitingPostDom, vecToScalarPostDom); // vecToScalar >= vecLoopExiting

    PDT.addNewBlock(vecGuardBlock, loopExit); // loopExit >= vecGuardBlock(?)

    if (loopPostDomsEntry) {
      // (if scaHead >= preHeader) vecGuard >= preHeader
      PDT.changeImmediateDominator(entryBlock, vecGuardBlock);
    }

    // TODO update PDT
  }

  void
  updateScalarLoopStartValues(ValueToValueMapTy & vecLoopPhis) {
    auto * scalHeader = ScalarL.getHeader();

    IRBuilder<> scaGuardBuilder(scalarGuardBlock, scalarGuardBlock->begin());

    for (auto & scalInst : *scalHeader) {
      if (!isa<PHINode>(scalInst)) break;
      auto & scalarPhi = cast<PHINode>(scalInst);

      std::string phiName = scalarPhi.getName().str();

      int preHeaderIdx = scalarPhi.getBasicBlockIndex(entryBlock);
      assert(preHeaderIdx >= 0);
      auto & initialValue = *scalarPhi.getIncomingValue(preHeaderIdx);

    // create a PHI for every loop header phi in the scalar loop
      // when coming from the vectorGuard -> use the old initial values
      // when coming from the vecToScalarExit -> use the reduced scalar values from the vector loop
      auto & vecPhi = LookUp(vecValMap, scalarPhi);
      auto * latchVal = vecPhi.getIncomingValue(1 - preHeaderIdx);
      auto & vectorLiveOut = cast<Instruction>(*latchVal);

      auto &scaGuardPhi = *scaGuardBuilder.CreatePHI(scalarPhi.getType(), 2, phiName + ".scaGuard");
      scaGuardPhi.addIncoming(&vectorLiveOut, vecToScalarExit);
      scaGuardPhi.addIncoming(&initialValue, vecGuardBlock);

    // take scaGuardPhi from the new preHeader of the scalar loop (scalarGuardBlock)
      scalarPhi.setIncomingBlock(preHeaderIdx, scalarGuardBlock);
      scalarPhi.setIncomingValue(preHeaderIdx, &scaGuardPhi);
    }
  }

  static
  Value&
  ReplicateExpression(std::string suffix, Value & val, ValueToValueMapTy & replMap, std::function<Value* (Instruction&, IRBuilder<>&)> leafFunc, IRBuilder<> & builder) {
    auto * inst = dyn_cast<Instruction>(&val);

    // preserve non-insts
    if (!inst) {
      return val;
    }

    // already copied?
    auto itRepl = replMap.find(inst);
    if (itRepl != replMap.end()) {
      return *itRepl->second;
    }

    // do we have a custom leaf mapping for this value?
    auto * replaceWith = leafFunc(*inst, builder);
    if (replaceWith) {
      return *replaceWith;
    }

    // we must stay in this basic block
    assert(!isa<PHINode>(val) && "can not replicate this!");

    // otw, start cloning
    auto & clone = *inst->clone();
    auto cloneName = inst->getName().str() + suffix;

    // TODO we can not have cycles in legal IR (we break on PHIs). Still insert now to not diverge on degenerate IR.
    replMap[&val] = &clone;

    // remap its operands
    for (size_t i = 0; i < inst->getNumOperands(); ++i) {
      auto & clonedOp = ReplicateExpression(suffix, *inst->getOperand(i), replMap, leafFunc, builder);
      clone.setOperand(i, &clonedOp);
    }

    // insert the clone after its dependences
    builder.Insert(&clone, cloneName);

    return clone;
  }

  static
  int
  GetLoopIncomingIndex(Loop & L, const PHINode & phi) {
    for (size_t i = 0; i < phi.getNumIncomingValues(); ++i) {
      auto * inBlock = phi.getIncomingBlock(i);
      if (L.contains(inBlock)) {
        return i;
      }
    }
    return -1;
  }

  // fix the vector loop exit condition
  void
  RepairVectorLoopCondition(ValueToValueMapTy & vecLoopPhis) {
    auto & vecHead = LookUp(vecValMap, *ScalarL.getHeader());

    // map vector phis to their shapes
    std::map<Value*, PHINode*> headerPhis;
    std::map<Value*, PHINode*> reductors;
    for (auto & Inst : *ScalarL.getHeader()) {
      auto * phi = dyn_cast<PHINode>(&Inst);
      if (!phi) break;
      auto & vecPhi = LookUp(vecValMap, *phi);
      headerPhis[phi] = &vecPhi;

      auto * sp = reda.getStrideInfo(*phi);
      if (sp) {
        reductors[sp->reductor] = &vecPhi;
      }
    }

    // replicate the vector loop exit condition
    auto & scaExiting = *GetUniqueExiting(ScalarL);
    auto & vecExiting = LookUp(vecValMap, scaExiting);

    auto & vecExitingBr = cast<BranchInst>(*vecExiting.getTerminator());

    // replicate the vector loop exit condition
    IRBuilder<> builder(&vecHead, vecHead.getTerminator()->getIterator());

    // for tail predication, stay inside the loop while there is at least one iteration remaining
    unsigned exitTriggerIteration = useTailPredication ? vectorWidth : 2 * vectorWidth;

    auto & exitVal =
      exitConditionBuilder.synthesize(exitTriggerIteration, ".vecExit", builder, &uniOverrides,
         [&](Instruction & inst) -> IterValue {
           assert (!isa<CallInst>(inst));

           // loop invariant value
           if (!ScalarL.contains(inst.getParent())) return IterValue(inst, 0);

           // did we hit the header phi
           auto itHeaderPhi = headerPhis.find(&inst);

           // determine the shape of the tested iteration variable
           Value * headerPhi = nullptr;
           int offset = 0;
           if (itHeaderPhi != headerPhis.end()) {
             // we are checking the header phi directly
             headerPhi = itHeaderPhi->second;
             offset = 0;
             return IterValue(*headerPhi, offset);
           }

           // The reductor itself.
           assert (reductors.find(&inst) != reductors.end());
           auto * matchingHeaderPhi = reductors[&inst];
           headerPhi = matchingHeaderPhi;
           offset = -1; // phi is at iteration -1 relative to reductor
           return IterValue(*headerPhi, offset);
        }
    );

    // use forwarded exit condition
    vecExitingBr.setCondition(&exitVal);

    // annotate the vector loop exit condition as uniform
    uniOverrides.insert(&exitVal);
  }

  // supplement the vector loop guard condition
  // the Vloop is executed if there is at least one full vector of iterations
  void
  SupplementVectorGuard() {
    // map vector phis to their shapes
    std::map<Value*, rv::VectorShape> valShapes;
    std::map<Value*, PHINode*> reductors;
    for (auto & Inst : *ScalarL.getHeader()) {
      auto * phi = dyn_cast<PHINode>(&Inst);
      if (!phi) break;
      auto * pat = reda.getStrideInfo(*phi);
      if (pat) {
        auto & reductor = *pat->reductor;
        auto redShape = pat->getShape(vectorWidth);
        valShapes[phi] = redShape;
        reductors[&reductor] = phi;
      }
    }

    // replicate the scalar loop exit condition
    auto & vecGuardBr = *cast<BranchInst>(vecGuardBlock->getTerminator());

    // replicate the vector loop exit condition
    IRBuilder<> builder(vecGuardBlock, vecGuardBr.getIterator());


    // Min number of loop iterations for vector instructions
    // FIXME: make this configurable / cost model dependent
    if (useTailPredication) {
      vecGuardBr.setCondition(exitConditionBuilder.exitsOnTrue()
                                  ? builder.getFalse()
                                  : builder.getTrue());
      return;
    }
    const unsigned VectorLoopThreshold = vectorWidth; // std::min(vectorWidth, 8);

    // synthesize(int iterOffset, std::string suffix, IRBuilder<> & builder, std::function<Instruction& (Instruction&)> embedFunc) {
    auto & exitVal =
      exitConditionBuilder.synthesize(VectorLoopThreshold, ".vecGuard", builder, nullptr,
         [&](Instruction & inst) -> IterValue {
           assert (!isa<CallInst>(inst));

           IF_DEBUG { errs() << inst << "\n"; }

           // loop invariant value
           if (!ScalarL.contains(inst.getParent())) return IterValue(inst, 0);

           // did we hit the header phi
           auto itHeaderPhi = valShapes.find(&inst);

           // determine the shape of the tested iteration variable
           PHINode * headerPhi = nullptr;
           int offset = 0;
           if (itHeaderPhi != valShapes.end()) {
             // we are checking the header phi directly
             headerPhi = cast<PHINode>(&inst);
           } else {
             // we checking the reductor result on the header phi (next iteration value)
             assert(reductors.find(&inst) != reductors.end() && "not testing a reductor");
             auto * matchingHeaderPhi = reductors[&inst];
             headerPhi = matchingHeaderPhi;
             // we are testing the reductor that evaluates to the next iteration value
             offset = -1; // phi is at iteration -1
           }

           // translate to vector loopt
           auto & vecPhi = cast<PHINode>(LookUp(vecValMap, *headerPhi));

           // FIXME don't reconstruct the init value -> map this somewhere before the transformation
           int initIdx = vecPhi.getBasicBlockIndex(vecGuardBlock);
           auto * initVal = vecPhi.getIncomingValue(initIdx);

           return IterValue(*initVal, offset);
          }
      );

    // use forwarded exit condition
    vecGuardBr.setCondition(&exitVal);
  }

  // replicate the scalar loop exit condition in vecToScalarExit
  void
  SupplementVectorExit(ValueToValueMapTy & vecLoopPhis) {
    auto & scaExiting = *GetUniqueExiting(ScalarL);
    auto & vecExiting = LookUp(vecValMap, scaExiting);

    auto & exitingBr = cast<BranchInst>(*scaExiting.getTerminator());
    int exitSuccIdx = exitingBr.getSuccessor(0) == loopExit ? 0 : 1;

    ValueToValueMapTy replMap;

    IRBuilder<> builder(vecToScalarExit, vecToScalarExit->getTerminator()->getIterator());

    auto & vecExitBr = *cast<BranchInst>(vecToScalarExit->getTerminator());

    // Whether we need control from the vector loop exit to the scalar loop.
    if (!useTailPredication && (tripAlign % vectorWidth != 0)) {
      IF_DEBUG { errs() << "remTrans: need a scalar remainder loop.\n"; }
    // replicate the exit condition
      // replace scalar reductors with their vector-loop versions
      auto & exitVal =
        ReplicateExpression(".v2s", *exitingBr.getCondition(), replMap,
           [&](Instruction & inst, IRBuilder<>&) -> Value* {
             // loop invariant value
             if (!ScalarL.contains(inst.getParent())) return &inst;

             // if we hit a reduction/induction value replace it with its vector version
             if (isa<PHINode>(inst) || reda.getStrideInfo(inst) || reda.getReductionInfo(inst)) {
               auto * loopVal = &LookUp(vecValMap, inst);
               auto * lcssaPhi = PHINode::Create(loopVal->getType(), 1, inst.getName().str() + ".lscca", &*vecToScalarExit->begin());
               lcssaPhi->addIncoming(loopVal, &vecExiting);
               return lcssaPhi;
             }

             // Otw, copy that operation
             return nullptr;
            },
        builder
        );

      vecExitBr.setCondition(&exitVal);

      // swap the exits to negate the condition
      if (exitSuccIdx == 0) {
        vecExitBr.setSuccessor(0, loopExit);
        vecExitBr.setSuccessor(1, scalarGuardBlock);
      }

      return;

    }

    // the scalar loop is never executed (full SIMD vectors for all iteration or
    // we are using tail predication for the vector loop)
    // --> unconditionally branch to the loop exit
    vecExitBr.setCondition(ConstantInt::getTrue(vecExitBr.getContext()));
    vecExitBr.setSuccessor(0, loopExit);
    vecExitBr.setSuccessor(1, scalarGuardBlock);
  }

  void
  updateExitLiveOuts(ValueToValueMapTy & vecLiveOuts) {
  // reduce all remaining live outs
    IRBuilder<> exitBuilder(loopExit, loopExit->begin());

    for (auto * BB : ScalarL.blocks()) {
      for (auto & Inst : *BB) {
        PHINode * mergePhi = nullptr;
        SmallVector<std::pair<User*, unsigned>, 3> CachedUses;
        for (auto & use : Inst.uses()) { CachedUses.emplace_back(use.getUser(), use.getOperandNo()); }
        for (auto cachedUse : CachedUses) {
          auto * userInst = cast<Instruction>(cachedUse.first);
          unsigned useOperandNo = cachedUse.second;

          if (ScalarL.contains(userInst)) continue;

          auto scaLiveOut = &Inst;
          auto vecLiveOut = &LookUp(vecValMap, Inst);

          if (isa<PHINode>(userInst) && userInst->getParent() == loopExit) {
            auto & userPhi = *cast<PHINode>(userInst);
            int exitingIdx = userPhi.getBasicBlockIndex(ScalarL.getExitingBlock());
            assert(exitingIdx >= 0);

            // vector loop is exiting to this block now as well
            userPhi.addIncoming(vecLiveOut, vecToScalarExit);

          } else {

            // Create a new phi node to receive this value
            if (!mergePhi) {
              std::string liveOutName = scaLiveOut->getName().str();
              mergePhi = exitBuilder.CreatePHI(scaLiveOut->getType(), 2, liveOutName + ".merge");
              mergePhi->addIncoming(scaLiveOut, ScalarL.getExitingBlock());
              mergePhi->addIncoming(vecLiveOut, vecToScalarExit);
              IF_DEBUG { errs() << "\tCreated merge phi " << *mergePhi << "\n"; }
            }

            userInst->setOperand(useOperandNo, mergePhi);
            IF_DEBUG { errs() << "\t- fixed user " << *userInst << "\n"; }
          }
        }
      }
    }
  }

  void
  fixVecLoopHeaderPhis() {
    auto & vecHead = LookUp(vecValMap, *ScalarL.getHeader());
    for (auto & Inst : vecHead) {
      auto * phi = dyn_cast<PHINode>(&Inst);
      if (!phi) break;
      int initOpIdx = phi->getBasicBlockIndex(entryBlock);
      phi->setIncomingBlock(initOpIdx, vecGuardBlock);
    }
  }

  void
  insertAVLComputation() {
    IF_DEBUG_REM{ for (auto *BB : ClonedL.blocks()) Dump(*BB); }

    auto &LoopHead = *ClonedL.getHeader();

    // map vector phis to their shapes
    std::map<Value*, PHINode*> headerPhis;
    std::map<Value*, PHINode*> reductors;
    for (auto & Inst : *ScalarL.getHeader()) {
      auto * phi = dyn_cast<PHINode>(&Inst);
      if (!phi) break;
      auto & vecPhi = LookUp(vecValMap, *phi);
      headerPhis[phi] = &vecPhi;

      auto * sp = reda.getStrideInfo(*phi);
      if (sp) {
        reductors[sp->reductor] = &vecPhi;
      }
    }

    // compute the lane index
    IRBuilder<> Builder(LoopHead.getFirstNonPHI());
    auto & RawAVL =
      exitConditionBuilder.synthesizeEVL(0, ".avl", Builder, &uniOverrides,
         [&](Instruction & inst) -> IterValue {
           assert (!isa<CallInst>(inst));

           // loop invariant value
           if (!ScalarL.contains(inst.getParent())) return IterValue(inst, 0);

           // did we hit the header phi
           auto itHeaderPhi = headerPhis.find(&inst);

           // determine the shape of the tested iteration variable
           Value * headerPhi = nullptr;
           int offset = 0;
           if (itHeaderPhi != headerPhis.end()) {
             // we are checking the header phi directly
             headerPhi = itHeaderPhi->second;
             offset = -2;
           } else {
             // we checking the reductor result on the header phi (next iteration value)
             assert(reductors.find(&inst) != reductors.end() && "not testing a reductor");
             auto * matchingHeaderPhi = reductors[&inst];
             headerPhi = matchingHeaderPhi;
             offset = -1; // phi is at iteration -1 relative to reductor
           }

           return IterValue(*headerPhi, offset);
       }
    );

    // Convert to native avl type
    Type *AVLTy = Builder.getInt32Ty();
    auto *AVLArg = Builder.CreateSExtOrTrunc(&RawAVL, AVLTy);
    uniOverrides.insert(AVLArg);
    
    // Normalize AVL to vectorization width
    auto *VWConst = ConstantInt::get(AVLTy, vectorWidth);
    auto &LessThanVW = *Builder.CreateICmpULT(AVLArg, VWConst, RawAVL.getName() + ".lt.mvl");
    uniOverrides.insert(&LessThanVW);
    this->AVL = Builder.CreateSelect(&LessThanVW, AVLArg, VWConst, "select.avl");
    uniOverrides.insert(AVL);

    // after splitting
    IF_DEBUG_REM {
      errs() << ":: after tail predication::\n";
      for (auto *BB : ClonedL.blocks()) Dump(*BB);
      errs() << "AVL: " << *AVL << "\n";
    }
  }

  void
  repairValueFlow() {
    ValueToValueMapTy vecLoopPhis, vecLiveOuts;

    // start edge now coming from vecGuardBlock (instead of old preheader entrBlock)
    fixVecLoopHeaderPhis();

    // when vectorizing with tail predication, insert the iteration guard now.
    if (useTailPredication) insertAVLComputation();

    // let the scalar loop start from the remainder vector loop remainder (if the VL was executed)
    updateScalarLoopStartValues(vecLoopPhis);

    // repair vector loop liveouts
    updateExitLiveOuts(vecLiveOuts);

  // supplement the scalar remainder condition (vecToScalarBlock -> loopExit edge)
    // this edge is taken if no iterations remain for the scalar loop
    SupplementVectorExit(vecLoopPhis);

    // repair the vector loop exit condition to check for the next iteration (instead on the phi + strie * vectorWidth -th)
    RepairVectorLoopCondition(vecLoopPhis);

  // supplement the vector loop guard condition (vecGuardBlock -> vector loop edge)
    // the vector loop will execute on at least one full vector
    SupplementVectorGuard();
  }
};

BranchCondition*
RemainderTransform::analyzeExitCondition(llvm::Loop & L, int vectorWidth) {
  auto * loopExiting = L.getExitingBlock();

  // loop exit conditions constraints
  auto * exitingBr = dyn_cast<BranchInst>(loopExiting->getTerminator());
  Report() << "Analyzing loop exiting block: " << loopExiting->getName()
           << "\n";

  if (!exitingBr) {
    Report() << "remTrans: exiting terminator is not a branch: " << *loopExiting->getTerminator() << "\n";
    return nullptr;
  }

  return BranchCondition::analyze(*exitingBr, vectorWidth, reda, L);
}

bool
RemainderTransform::canTransformLoop(llvm::Loop & L) {
  auto * loopExiting = L.getExitingBlock();
  if (!loopExiting) {
    Report() << "remTrans: multi-exit loops not supported yet\n";
    return false;
  }

  auto * loopLatch = L.getLoopLatch();
  if (!loopLatch) {
    Report() << "remTrans: multi-latch loops not supported yet\n";
    return false;
  }

  if (loopLatch != L.getExitingBlock()) {
    Report() << "remTrans: only support latch exit loops\n";
    return false;
  }

  if (!L.getLoopPredecessor()) {
    Report() << "remTrans: require a unique loop predecessor\n";
    return false;
  }

  // only attempt loops with recognized reduction patterns
  for (auto & Inst : *L.getHeader()) {
    auto * phi = dyn_cast<PHINode>(&Inst);
    if (!phi) break;

    if (!(reda.getReductionInfo(*phi) || reda.getStrideInfo(*phi))) {
      Report() << "remTrans: unsupported header PHI " << *phi << "\n";
      return false;
    }

  }

  return true;
}


RemainderTransform::RemainderTransform(llvm::Function &_F, llvm::FunctionAnalysisManager & FAM, ReductionAnalysis & _reda)
: F(_F)
, FAM(FAM)
, DT(FAM.getResult<llvm::DominatorTreeAnalysis>(F))
, PDT(FAM.getResult<llvm::PostDominatorTreeAnalysis>(F))
, LI(FAM.getResult<llvm::LoopAnalysis>(F))
, PBI(FAM.getResult<llvm::BranchProbabilityAnalysis>(F))
, reda(_reda)
{}

bool RemainderTransform::analyzeLoopStructure(Loop &L) {
  // run capability checks
  // CFG caps
  if (!canTransformLoop(L))
    return false;

  // branch condition caps
  auto *branchCond = analyzeExitCondition(L, 1);
  return branchCond;
}

PreparedLoop
RemainderTransform::createVectorizableLoop(Loop & L, ValueSet & uniOverrides, bool useTailPredication, int vectorWidth, int tripAlign) {
// run capability checks
  // CFG caps
  if (!canTransformLoop(L)) return PreparedLoop();

  // branch condition caps
  auto * branchCond = analyzeExitCondition(L, vectorWidth);
  if (!branchCond) {
    Report() << "remTrans: can not handle loop exit condition\n";
    IF_DEBUG_REM {
      L.print(outs());
      for (auto * BB : L.blocks()) {
        errs() << "\n";
        errs() << *BB;
      }
    }

    return PreparedLoop();
  }

  // Split the preheader edge if necessary.
  if (!L.getLoopPreheader()) {
    auto *ScaHead = L.getHeader();
    auto *LoopPred = L.getLoopPredecessor();
    auto *PredTerm = LoopPred->getTerminator();
    int LoopBranchIdx = -1;
    for (int i = 0; i < (int)PredTerm->getNumSuccessors(); ++i) {
      if (ScaHead == PredTerm->getSuccessor(i)) {
        LoopBranchIdx = i;
        break;
      }
    }
    assert((LoopBranchIdx >= 0) &&
           "Loop predecessor does not branch to loop?");

    splitPreheaderEdge(*LoopPred, LoopBranchIdx, DT, PDT, LI);

    assert(L.getLoopPreheader() &&
           "Could not establish preheader-ness");
  }

// otw, clone the scalar loop
  ValueToValueMapTy cloneMap;
  auto cloneInfo = CloneLoop(L, F, FAM, cloneMap);
#if 0
  LoopCloner loopCloner(F, DT, PDT, LI, PB);
  ValueToValueMapTy cloneMap;
  LoopCloner::LoopCloneInfo cloneInfo = loopCloner.CloneLoop(L, cloneMap);
#endif
  auto & clonedLoop = cloneInfo.clonedLoop;

  // reda.updateForClones(LI, cloneMap);

// embed the cloned loop
  LoopTransformer loopTrans(F, DT, PDT, LI, reda, uniOverrides, *branchCond, L, clonedLoop, cloneMap, useTailPredication, vectorWidth, tripAlign);

  // rebuild reduction information for cloned loop
  reda.analyze(clonedLoop);

  IF_DEBUG_REM {
    errs() << "-- function after remTrans --\n";
    Dump(F);
  }

  delete branchCond;

  return PreparedLoop(&clonedLoop, loopTrans.AVL);
}

} // namespace rv
