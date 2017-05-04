//
// Created by tkloessner on 24.04.17.
//

#include "rv/transform/LoopRemainderTransform.h"

#include <llvm/Transforms/Utils/BasicBlockUtils.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Transforms/Utils/LoopUtils.h>
#include <llvm/Analysis/ScalarEvolutionExpander.h>

using namespace llvm;

namespace rv {

void LoopRemainderTransform::emitMinimumIterationCountCheck(Loop* L, LoopInfo& LI,
                                                            BasicBlock* Bypass) {
  Value* Count = getOrCreateTripCount(L);
  BasicBlock* BB = L->getLoopPreheader();
  llvm::IRBuilder<> Builder(BB->getTerminator());

  // Generate code to check that the loop's trip count that we computed by
  // adding one to the backedge-taken count will not overflow.
  Value* CheckMinIters = Builder.CreateICmpULT(Count, ConstantInt::get(Count->getType(), VF),
                                               "min.iters.check");

  BasicBlock* NewBB = BB->splitBasicBlock(BB->getTerminator(), "min.iters.checked");
  if (L->getParentLoop()) {
    L->getParentLoop()->addBasicBlockToLoop(NewBB, LI);
  }
  ReplaceInstWithInst(BB->getTerminator(), BranchInst::Create(Bypass, NewBB, CheckMinIters));
  LoopBypassBlocks.push_back(BB);
}

void LoopRemainderTransform::emitVectorLoopEnteredCheck(Loop* L, LoopInfo& LI, BasicBlock* Bypass) {
  Value* TC = getOrCreateVectorTripCount(L);
  BasicBlock* BB = L->getLoopPreheader();
  IRBuilder<> Builder(BB->getTerminator());

  // Now, compare the new count to zero. If it is zero skip the vector loop and
  // jump to the scalar loop.
  Value* Cmp = Builder.CreateICmpEQ(TC, Constant::getNullValue(TC->getType()), "cmp.zero");

  // Generate code to check that the loop's trip count that we computed by
  // adding one to the backedge-taken count will not overflow.
  BasicBlock* NewBB = BB->splitBasicBlock(BB->getTerminator(), "vector.ph");
  if (L->getParentLoop()) {
    L->getParentLoop()->addBasicBlockToLoop(NewBB, LI);
  }
  ReplaceInstWithInst(BB->getTerminator(), BranchInst::Create(Bypass, NewBB, Cmp));
  LoopBypassBlocks.push_back(BB);
}

PHINode* LoopRemainderTransform::createInductionVariable(Loop* L, Value* Start, Value* End,
                                                         Value* Step)
{
  BasicBlock* Header = L->getHeader();
  BasicBlock* Latch = L->getLoopLatch();
  // As we're just creating this loop, it's possible no latch exists
  // yet. If so, use the header as this will be a single block loop.
  if (!Latch) {
    Latch = Header;
  }

  IRBuilder<> Builder(&*Header->getFirstInsertionPt());
  auto* Induction = Builder.CreatePHI(Start->getType(), 2, "index");

  Builder.SetInsertPoint(Latch->getTerminator());

  // Create i+step and fill the PHINode.
  Value* Next = Builder.CreateAdd(Induction, Step, "index.next");
  Induction->addIncoming(Start, L->getLoopPreheader());
  Induction->addIncoming(Next, Latch);
  // Create the compare.
  Value* ICmp = Builder.CreateICmpEQ(Next, End);
  Builder.CreateCondBr(ICmp, L->getExitBlock(), Header);

  // Now we have two terminators. Remove the old one from the block.
  Latch->getTerminator()->eraseFromParent();

  return Induction;
}

Value* LoopRemainderTransform::getOrCreateTripCount(Loop* L) {
  if (TripCount) return TripCount;

  // Find the loop boundaries.
  const SCEV* BackedgeTakenCount = SE->getBackedgeTakenCount(L);
  assert(BackedgeTakenCount != SE->getCouldNotCompute() && "Invalid loop count");

  Type* IdxTy = IntegerType::getInt64Ty(L->getHeader()->getModule()->getContext());

  // The exit count might have the type of i64 while the phi is i32. This can
  // happen if we have an induction variable that is sign extended before the
  // compare. The only way that we get a backedge taken count is that the
  // induction variable was signed and as such will not overflow. In such a case
  // truncation is legal.
  if (BackedgeTakenCount->getType()->getPrimitiveSizeInBits() > IdxTy->getPrimitiveSizeInBits()) {
    BackedgeTakenCount = SE->getTruncateOrNoop(BackedgeTakenCount, IdxTy);
  }
  BackedgeTakenCount = SE->getNoopOrZeroExtend(BackedgeTakenCount, IdxTy);

  // Get the total trip count from the count by adding 1.
  const SCEV* ExitCount = SE->getAddExpr(BackedgeTakenCount,
                                         SE->getOne(BackedgeTakenCount->getType()));

  const DataLayout& DL = L->getHeader()->getModule()->getDataLayout();

  // Expand the trip count and place the new instructions in the preheader.
  // Notice that the pre-header does not change, only the loop body.
  SCEVExpander Exp(*SE, DL, "induction");

  // Count holds the overall loop count (N).
  TripCount = Exp.expandCodeFor(ExitCount, ExitCount->getType(),
                                L->getLoopPreheader()->getTerminator());

  if (TripCount->getType()->isPointerTy()) {
    TripCount = CastInst::CreatePointerCast(TripCount, IdxTy, "exitcount.ptrcnt.to.int",
                                            L->getLoopPreheader()->getTerminator());
  }

  return TripCount;
}

Value* LoopRemainderTransform::getOrCreateVectorTripCount(Loop* L) {
  if (VectorTripCount) return VectorTripCount;

  Value* TC = getOrCreateTripCount(L);
  IRBuilder<> Builder(L->getLoopPreheader()->getTerminator());

  // Now we need to generate the expression for N - (N % VF), which is
  // the part that the vectorized body will execute.
  // The loop step is equal to the vectorization factor (num of SIMD elements)
  // times the unroll factor (num of SIMD instructions).
  Constant* Step = ConstantInt::get(TC->getType(), VF);
  Value* R = Builder.CreateURem(TC, Step, "n.mod.vf");
  VectorTripCount = Builder.CreateSub(TC, R, "n.vec");

  return VectorTripCount;
}

Loop* LoopRemainderTransform::createLoopEpilogueStructure(unsigned VF) {
  this->VF = VF;

  /*
   In this function we generate a new loop. The new loop will contain
   the vectorized instructions while the old loop will continue to run the
   scalar remainder.

       [ ] <-- loop iteration number check.
    /   |
   /    v
  |    [ ] <-- vector loop bypass (may consist of multiple blocks).
  |  /  |
  | /   v
  ||   [ ]     <-- vector pre header.
  |/    |
  |     v
  |    [  ] \
  |    [  ]_|   <-- vector loop.
  |     |
  |     v
  |   -[ ]   <--- middle-block.
  |  /  |
  | /   v
  -|- >[ ]     <--- new preheader.
   |    |
   |    v
   |   [ ] \
   |   [ ]_|   <-- old scalar loop to handle remainder.
    \   |
     \  v
      >[ ]     <-- exit block.
   ...
   */

  BasicBlock* VectorPH      = ScalarLoop->getLoopPreheader();
  BasicBlock* ExitBlock     = ScalarLoop->getExitBlock();
  assert(VectorPH && "Invalid loop structure");
  assert(ExitBlock && "Must have an exit block");

  // Split the single block loop into the two loop structure described above.
  BasicBlock* VecBody = VectorPH->splitBasicBlock(VectorPH->getTerminator(), "vector.body");
  BasicBlock* MiddleBlock = VecBody->splitBasicBlock(VecBody->getTerminator(), "middle.block");
  BasicBlock* ScalarPH = MiddleBlock->splitBasicBlock(MiddleBlock->getTerminator(), "scalar.ph");

  // Create and register the new vector loop.
  Loop* Lp = new Loop();
  Loop* ParentLoop = ScalarLoop->getParentLoop();

  // Insert the new loop into the loop nest and register the new basic blocks
  // before calling any utilities such as SCEV that require valid LoopInfo.
  if (ParentLoop) {
    ParentLoop->addChildLoop(Lp);
    ParentLoop->addBasicBlockToLoop(ScalarPH, LI);
    ParentLoop->addBasicBlockToLoop(MiddleBlock, LI);
  }
  else {
    LI.addTopLevelLoop(Lp);
  }
  Lp->addBasicBlockToLoop(VecBody, LI);

  // We need to test whether the backedge-taken count is uint##_max. Adding one
  // to it will cause overflow and an incorrect loop trip count in the vector
  // body. In case of overflow we want to directly jump to the scalar remainder
  // loop.
  emitMinimumIterationCountCheck(Lp, LI, ScalarPH);

  // Now, compare the new count to zero. If it is zero skip the vector loop and
  // jump to the scalar loop.
  emitVectorLoopEnteredCheck(Lp, LI, ScalarPH);

  // Generate the induction variable.

  // Find the loop boundaries.
  Value* TotalTC = getOrCreateTripCount(Lp);

  Value* VectorTC = getOrCreateVectorTripCount(Lp);
  Value* StartIdx = ConstantInt::get(TotalTC->getType(), 0);
  Constant* Step = ConstantInt::get(TotalTC->getType(), (uint64_t)VF);
  createInductionVariable(Lp, StartIdx, VectorTC, Step);

  // Add a check in the middle block to see if we have completed
  // all of the iterations in the first vector loop.
  // If (N - N%VF) == N, then we *don't* need to run the remainder.
  Value* CmpN = CmpInst::Create(Instruction::ICmp, CmpInst::ICMP_EQ, TotalTC, VectorTC, "cmp.n",
                                MiddleBlock->getTerminator());
  ReplaceInstWithInst(MiddleBlock->getTerminator(), BranchInst::Create(ExitBlock, ScalarPH, CmpN));

  // Fix the starting values.
  fixRemainderLoopPHIs();

  // Save the state.
  LoopMiddleBlock = MiddleBlock;
  LoopExitBlock = ExitBlock;

  return Lp;
}

}