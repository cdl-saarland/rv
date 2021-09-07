//===- rv/transform/guardedDivLoopTrans.cpp - make divergent loops uniform --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//


#include "rv/transform/divLoopTrans.h"

#include "rv/PlatformInfo.h"
#include "rv/transform/maskExpander.h"
#include "utils/rvTools.h"

#include "rvConfig.h"
#include "rv/rvDebug.h"
#include "report.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>

#include <llvm/Transforms/Utils/SSAUpdater.h>
#include <llvm/IR/Verifier.h>

#define IF_DEBUG_DLT IF_DEBUG

using namespace llvm;


namespace rv {

static BranchInst&
CreateIf(IRBuilder<> & builder, Value & Cond, BasicBlock & IfTrue) {
  std::string name = Cond.getName().str() + ".else";
  auto & falseBlock = *BasicBlock::Create(builder.getContext(), name, IfTrue.getParent(), &IfTrue);
  auto & falseBranch = *builder.CreateCondBr(&Cond, &IfTrue, &falseBlock);
  builder.SetInsertPoint(&falseBlock);
  return falseBranch;
}


static void
ForAllLiveouts(BasicBlock & exitBlock, std::function<void(PHINode & lcPhi, int slot)> userFunc) {
  // TODO implement this for relaxed LCSSA form
  auto itBegin = exitBlock.begin(), itEnd = exitBlock.end();
  for (auto it = itBegin; it != itEnd && isa<PHINode>(*it); ++it) {
    auto & lcPhi = cast<PHINode>(*it);
    assert(lcPhi.getNumIncomingValues() == 1 && "was not a normalized lcssa phi");
    userFunc(lcPhi, 0);
  }
}

// make \p inputVal the incoming value in all missing incoming value slots of \p phi.
static void
AttachMissingInputs(PHINode & phi, Value & inputVal) {
  // make trackerPhi the default input on all remaining incoming positions
  for (auto & blockUse : phi.getParent()->uses()) {
    auto userInst = dyn_cast<Instruction>(blockUse.getUser());
    if (!userInst || !userInst->isTerminator()) continue;

    auto * inBlock = userInst->getParent();
    if (phi.getBasicBlockIndex(inBlock) >= 0) continue;
    phi.addIncoming(&inputVal, inBlock);
  }
}

Instruction&
TransformSession::lowerLatchUpdate(TrackerDesc & desc) {
  auto & trackerPhi = *desc.trackerPhi;
  auto & updPhi = *desc.updatePhi;

  assert(!getShadowInput(updPhi) && "already has a shadow input!");

  // trackerPhi should be piped through on disabled lanes.
  setShadowInput(updPhi, trackerPhi);

  // preserve the tracker also on active lanes if the thread stays in the loop.
  AttachMissingInputs(updPhi, trackerPhi);

  // done
  return updPhi;
}

void
TransformSession::lowerTrackerUpdates() {
  lowerLatchUpdate(liveMaskDesc);

  for (auto & itUpdate : liveOutDescs) {
    lowerLatchUpdate(itUpdate.second);
  }
  for (auto & itUpdate : exitDescs) {
    lowerLatchUpdate(itUpdate.second);
  }
}

void
TransformSession::transformLoop() {
  IF_DEBUG_DLT { errs() << "TransformLoop " << loop.getName() << "\n"; }

  assert(vecInfo.isDivergentLoop(loop) && "trying to convert a non-divergent loop");

// creates cascading phi nodes to track loop live outs
  SmallVector<Loop::Edge, 4> loopExitEdges;
  loop.getExitEdges(loopExitEdges);

  auto & loopHeader = *loop.getHeader();
  auto loopName = loop.getName().str();

  IRBuilder<> trackerBuilder(&loopHeader, loopHeader.getFirstInsertionPt());
  auto * boolTy = trackerBuilder.getInt1Ty();

  const VectorShape loopControlShape = VectorShape::varying();

// create the loop live mask (predicate of the loop header)
  liveMaskDesc.trackerPhi = trackerBuilder.CreatePHI(boolTy, 2, loopName + ".live");
  vecInfo.setVectorShape(*liveMaskDesc.trackerPhi, loopControlShape);

// create a dedicated latch block for the loop
  requestPureLatch(); // creates maskUpdatePHi and pureLatch
  IRBuilder<> latchBuilder(pureLatch, pureLatch->getFirstInsertionPt());

// create tracking infrastructure for the loop exits
  for (auto & edge : loopExitEdges) {
    const VectorShape exitShape = VectorShape::varying();
    auto & exitingBlock = *const_cast<BasicBlock*>(edge.first);
    auto & exitBlock = *const_cast<BasicBlock*>(edge.second);
    auto exitName = exitBlock.getName().str();

  // track live out threads for this exit
    TrackerDesc exitDesc;
    exitDesc.trackerPhi = trackerBuilder.CreatePHI(boolTy, 2, exitName + ".xtrack");
    vecInfo.setVectorShape(*exitDesc.trackerPhi, exitShape);
    exitDesc.updatePhi = latchBuilder.CreatePHI(boolTy, 2, exitName + ".xupd");
    vecInfo.setVectorShape(*exitDesc.updatePhi, exitShape);
    exitDescs[&exitBlock] = exitDesc;
    exitDesc.trackerPhi->addIncoming(latchBuilder.getFalse(), loop.getLoopPreheader());
    exitDesc.trackerPhi->addIncoming(exitDesc.updatePhi, pureLatch);

    // if exiting from a nested loop we will need to create LCSSA phis in the rebound block
    Loop * leftLoop = loopInfo.getLoopFor(&exitingBlock);
    bool reboundingNestedExit = leftLoop != &loop;

    // collect continue block, control context
    auto exitingName = exitingBlock.getName().str();
    auto & exitingBr = *cast<BranchInst>(exitingBlock.getTerminator());
    bool exitOnFalse = exitingBr.getSuccessor(1) == &exitBlock;

// Rebound the exiting edge and create mask updates (loop live mask AND exit mask tracker)
    // fold the exiting edge back into the loop by the sending it to the latch
    // exitingBlock // original exitingBlock
    //   |     \
    //   |      reboundBlock (likely ==pureLatchBlock) // skip to loop latch
    //   |
    // continueBlock // in-loop successor of exitingBlock
    //
    // The exiting->pureLatchBlock edge replaces the exiting->exit edge in the original CFG
    // Fold all state into trackers in the select block
    // We will branch to the actual exitBlock after the loop has finished and recover the exiting->exit edge state from tracker phis
    // Since the exit condition is divergent, the linearizer will fold this pattern down into linear control + blends/selects

    // HINT:
    // this control transformation may temporarily break dominance in def-use chains (imagine i++ after the divergent exit)
    // However, the linearizer intentively folds the rebound pattern back down into sequential control.

    // rebound the exiting edge to the new pure latch
    // fold control down to i1-s using phi ndoes
    // the linearizer will convert those to selects later on
    // exiting edge

    // since we will select on exiting, break the exiting edge if exiting from the old latch
    BasicBlock * reboundBlock = &exitingBlock;
    if (reboundingNestedExit || (oldLatch == &exitingBlock)) {
      reboundBlock = BasicBlock::Create(exitingBlock.getContext(), exitName + ".rebound", exitingBlock.getParent(), pureLatch);
      if (vecInfo.isDivergentLoopExit(exitBlock)) {
        vecInfo.addDivergentLoopExit(*reboundBlock);
      }
      loop.addBasicBlockToLoop(reboundBlock, loopInfo);
      exitingBr.setSuccessor((int) exitOnFalse, reboundBlock);
      auto & reboundBr = *BranchInst::Create(pureLatch, reboundBlock);
      vecInfo.setVectorShape(reboundBr, VectorShape::uni());
    } else {
      exitingBr.setSuccessor(exitOnFalse, pureLatch);
    }

    // mask out this thread in the live mask when the exit is taken
    liveMaskDesc.updatePhi->addIncoming(ConstantInt::getFalse(exitingBlock.getContext()), reboundBlock);
    // set the exit flag if the exit is taken
    exitDesc.updatePhi->addIncoming(ConstantInt::getTrue(exitingBlock.getContext()), reboundBlock);

  // maintain LCSSA phis in reboundBlock
    if (reboundingNestedExit) {
      IRBuilder<> rebBuilder(reboundBlock, reboundBlock->begin());
      ForAllLiveouts(exitBlock, [&](PHINode & lcPhi, int slot) {
          auto & liveOut = *lcPhi.getIncomingValue(slot);
          auto & nestedPhi = *rebBuilder.CreatePHI(liveOut.getType(), 1, liveOut.getName() + ".lcssa");
          vecInfo.setVectorShape(nestedPhi, vecInfo.getVectorShape(lcPhi));
          nestedPhi.addIncoming(&liveOut, &exitingBlock);
          lcPhi.setIncomingValue(slot, &nestedPhi);
      });
    }

  // create live out trackers
    if (vecInfo.isDivergentLoopExit(exitBlock)) {
      ForAllLiveouts(exitBlock, [&](PHINode & lcPhi, int slot) {
        auto & liveOut = *lcPhi.getIncomingValue(slot);

      // request a tracker for this live out value
        auto it = liveOutDescs.find(&liveOut);
        PHINode * updatePhi = nullptr;
        if (it != liveOutDescs.end()) {
          updatePhi = it->second.updatePhi;
        } else {
          TrackerDesc desc;
          desc.trackerPhi = trackerBuilder.CreatePHI(liveOut.getType(), 2, liveOut.getName() + ".track");
          vecInfo.setVectorShape(*desc.trackerPhi, exitShape);
          desc.trackerPhi->addIncoming(UndefValue::get(liveOut.getType()), loop.getLoopPreheader());
          updatePhi = latchBuilder.CreatePHI(liveOut.getType(), 2, liveOut.getName() + ".upd");
          desc.trackerPhi->addIncoming(updatePhi, pureLatch);
          vecInfo.setVectorShape(*updatePhi, exitShape);
          desc.updatePhi = updatePhi;
          liveOutDescs[&liveOut] = desc;
        }

      // if the exit was taken write the current value of @liveOut to the tracker
        updatePhi->addIncoming(&liveOut, reboundBlock);
        IF_DEBUG_DLT { errs() << "UPD PHI: " << *updatePhi <<  "\n"; }
      });
    }

#if 0
    // FIXME this should work..
    if (vecInfo.isDivergentLoopExit(exitBlock) && !reboundingNestedExit) {
      // no longer a divergent loop-exit
      vecInfo.removeDivergentLoopExit(exitBlock);
    }
#endif
  }

// create an exit cascade
   assert(pureLatch->getTerminator()->getNumSuccessors() == 1);
   auto & anyFunc = platInfo.requestRVIntrinsicFunc(RVIntrinsic::Any);

   // new joined latch exit
   BasicBlock & latchExit =  *BasicBlock::Create(anyFunc.getContext(), loopName + ".divexit", pureLatch->getParent(), pureLatch);
   if (loop.getParentLoop()) {
     loop.getParentLoop()->addBasicBlockToLoop(&latchExit, loopInfo);
   }

   // build the new uniform exit branch
   pureLatch->getTerminator()->eraseFromParent();
   {
     IRBuilder<> builder(pureLatch);
     auto * continueCond =
       builder.CreateCall(&anyFunc, ArrayRef<Value*>{liveMaskDesc.updatePhi}, loopName + ".stay");
     auto & anyBr= *builder.CreateCondBr(continueCond, &loopHeader, &latchExit);
     vecInfo.setVectorShape(anyBr, VectorShape::uni());
     vecInfo.setVectorShape(*continueCond, VectorShape::uni());
   }

// sort exits for cascading order
   // since all divergent exits have to be visited before any kill exits first emit all divergent exits then all kill exits
   // The linearizer will read the cascade and make sure that all divergent exits will be processed before going to any uniform exits
   std::vector<std::pair<BasicBlock&, BasicBlock&>> exitStack;
   DenseSet<const BasicBlock*> seenExits;
   for (auto & edge : loopExitEdges) {
     auto & exitingBlock = *const_cast<BasicBlock*>(edge.first);
     auto & exitBlock = *const_cast<BasicBlock*>(edge.second);

     if (!vecInfo.isKillExit(exitBlock)) exitStack.emplace_back(exitingBlock, exitBlock);
   }
   for (auto & edge : loopExitEdges) {
     auto & exitingBlock = *const_cast<BasicBlock*>(edge.first);
     auto & exitBlock = *const_cast<BasicBlock*>(edge.second);
     if (vecInfo.isKillExit(exitBlock)) exitStack.emplace_back(exitingBlock, exitBlock);
   }
   BasicBlock & lastExit = exitStack[exitStack.size() - 1].second;

   IRBuilder<> exitBuilder(&latchExit);

   // divergence shape
   const VectorShape exitShape = VectorShape::varying();

// materialize the exit cascade
   // materialize all exits in the stack after the loop
   struct LiveOutPhis {
     PHINode * trackedPhi[2];
     enum Type {
       KILL = 0,
       DIVERGENT = 1
     };
     LiveOutPhis() { trackedPhi[0] = trackedPhi[1] = nullptr; }
   };

   DenseMap<const Value*, LiveOutPhis> liveOutPhis; // LCSSA phis for liveOuts
   for (auto exitEdge : exitStack) {
     // predecessor block of the exit block
     BasicBlock * exitPredBlock = exitBuilder.GetInsertBlock();

     auto & exitingBlock = exitEdge.first;
     auto & exitBlock = exitEdge.second;

     IF_DEBUG_DLT {
       errs()
         << "Attaching exit: " << exitBlock.getName() << "\n"
         << "\tkill exit: " << vecInfo.isKillExit(exitBlock) << "\n";
     }

     bool killExit = vecInfo.isKillExit(exitBlock);
     VectorShape branchShape = killExit ? VectorShape::uni() : exitShape;

  // set the block predicate (if we are seeing this exitBlock for the first time)
     assert(!seenExits.count(&exitBlock) && "todo implement support for exit blocks with multiple exiting incoming edges");

   // create a exitmask LCSSA phi and set it as the predicate of the exitBlock
     PHINode * exitMaskPhi = nullptr;
     {
       IRBuilder<> divExitBuilder(&latchExit);
       if (!latchExit.empty()) divExitBuilder.SetInsertPoint(&*latchExit.begin());
       exitMaskPhi = divExitBuilder.CreatePHI(boolTy, 1, exitBlock.getName().str() + ".xlcssa");
       vecInfo.setVectorShape(*exitMaskPhi, exitShape);
       maskEx.setBlockMask(exitBlock, *exitMaskPhi);
       vecInfo.setPredicate(exitBlock, *exitMaskPhi);
     }

     exitMaskPhi->addIncoming(exitDescs[&exitBlock].updatePhi, pureLatch);

     // re-connect the exit to the CFG
     BranchInst * exitBr = nullptr;

     if (&lastExit != &exitBlock) {
       // this advances the ir builder to the next block
       exitBr = & CreateIf(exitBuilder, *exitMaskPhi, exitBlock);
       auto & falseBlock = *exitBuilder.GetInsertBlock();
       // repair loopInfo
       if (loop.getParentLoop()) {
         loop.getParentLoop()->addBasicBlockToLoop(&falseBlock, loopInfo);
       }

     } else {
       // mask was set explicitely
       assert(!exitBuilder.GetInsertBlock()->getTerminator() && "not in loop exit normal form");
       exitBr = exitBuilder.CreateBr(&exitBlock);
       branchShape = VectorShape::uni(); // single successor
     }

     // set an appropriate mask for this exit
     vecInfo.setVectorShape(*exitBr, branchShape);

  // fix up live out uses in exits
     IRBuilder<> divExitBuilder(&latchExit);
     if (!latchExit.empty()) divExitBuilder.SetInsertPoint(&*latchExit.begin());

     auto it = exitBlock.begin();
     for (; it != exitBlock.end() && isa<PHINode>(it); ) {
       PHINode & lcPhi = cast<PHINode>(*it++);
       int slot = lcPhi.getBasicBlockIndex(&exitingBlock);
       assert(slot >= 0);

     // check if there already is an LCSSAA in divExit for this @liveOutVal
       Value & liveOutVal = *lcPhi.getIncomingValue(slot);
       auto itKnown = liveOutPhis.find(&liveOutVal);
       int exitType = killExit ? LiveOutPhis::KILL : LiveOutPhis::DIVERGENT;

       PHINode * exitPhi = nullptr;
       LiveOutPhis loPhis;

       if (itKnown != liveOutPhis.end() && itKnown->second.trackedPhi[exitType]) {
         exitPhi = itKnown->second.trackedPhi[exitType];
       } else {
         LiveOutPhis loPhis = liveOutPhis[&liveOutVal];

         // create an LCSSA phi
         exitPhi = divExitBuilder.CreatePHI(lcPhi.getType(), 1, liveOutVal.getName() + ".lcssa");
         vecInfo.setVectorShape(*exitPhi, exitShape);
         auto * transformedLiveOut = vecInfo.isKillExit(exitBlock)  ? &liveOutVal : liveOutDescs[&liveOutVal].updatePhi;
         exitPhi->addIncoming(transformedLiveOut, pureLatch);
         loPhis.trackedPhi[exitType] = exitPhi;

         // cache for reuse
         liveOutPhis[&liveOutVal] = loPhis;
       }

       if (lcPhi.getNumIncomingValues() == 1) {
         lcPhi.replaceAllUsesWith(exitPhi);
         lcPhi.eraseFromParent();
       } else {
         abort(); // expecting strict LCSSA form atm
         lcPhi.setIncomingValue(slot, exitPhi);
         lcPhi.setIncomingBlock(slot, exitPredBlock);
       }
     }
   }

   IF_DEBUG_DLT {
     errs() << "DLT: converted loop:\n";
     vecInfo.dump();
   }

   // finally set the loop header mask to terminate maskEx
   vecInfo.setPredicate(*loop.getHeader(), *liveMaskDesc.trackerPhi);
   maskEx.setBlockMask(*loop.getHeader(), *liveMaskDesc.trackerPhi);
}

// split the latch if it is not pure (only a terminator)
BasicBlock &
TransformSession::requestPureLatch() {
  // we cache the pure latch since we make it impure again along the way
  if (pureLatch) return *pureLatch;

  IF_DEBUG_DLT { errs() << "# reqPureLatch " << loop.getName() << "\n"; }

  std::string loopName = loop.getName().str();
  auto & latchBlock = *loop.getLoopLatch();
  auto & header = *loop.getHeader();

  // create a pure latch block
  pureLatch = BasicBlock::Create(latchBlock.getContext(), latchBlock.getName() + ".pure", latchBlock.getParent(), &latchBlock);
  oldLatch = &latchBlock;

  // create a mask update in the latch
  IRBuilder<> builder(pureLatch);
  liveMaskDesc.updatePhi = builder.CreatePHI(builder.getInt1Ty(), 2, loopName + ".live.upd");
  liveMaskDesc.updatePhi->addIncoming(liveMaskDesc.trackerPhi, &latchBlock);
  vecInfo.setVectorShape(*liveMaskDesc.updatePhi, VectorShape::varying()); // TODO join of all exit shapes

  // latchPhi updates the loop mask depending on the predecessors
  // the pure latch is the dedicated predecessor of the loop header so we can safely use the live.upd mask here
  liveMaskDesc.trackerPhi->addIncoming(UndefValue::get(Type::getInt1Ty(header.getContext())), loop.getLoopPreheader());
  liveMaskDesc.trackerPhi->addIncoming(liveMaskDesc.updatePhi, pureLatch);

  // insert on the latche edge
  auto & latchBr = *cast<BranchInst>(latchBlock.getTerminator());
  if (latchBr.isConditional()) {
    bool latchOnFalse = latchBr.getSuccessor(1) == &header;
    latchBr.setSuccessor(latchOnFalse, pureLatch);
  } else {
    assert(latchBr.getSuccessor(0) == &header);
    latchBr.setSuccessor(0, pureLatch);
  }
  auto & pureLatchBr = *BranchInst::Create(&header, pureLatch);
  vecInfo.setVectorShape(pureLatchBr, VectorShape::uni());
  vecInfo.setVectorShape(*pureLatch, VectorShape::uni());

  // register with LoopInfo & LoopTracker
  loop.addBasicBlockToLoop(pureLatch, loopInfo);
  oldLatch = &latchBlock;

  // fix header phi incoming blocks (the pure latch breaks this edge)
  for (auto & inst : header) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) break;
    int latchIdx = phi->getBasicBlockIndex(oldLatch);
    if (latchIdx >= 0) {
      phi->setIncomingBlock(latchIdx, pureLatch);
    }
  }

  assert((loop.getLoopLatch() == pureLatch) && "latch replacement didn't work");
  return *pureLatch;
}

void
DivLoopTrans::addLoopInitMasks(llvm::Loop & loop) {
  for (auto * childLoop : loop) {
    addLoopInitMasks(*childLoop);
  }

  auto it = sessions.find(&loop);
  if (it == sessions.end()) return; // uniform loop
  auto * loopSession = it->second;

// request entry edge
  // this loop has a live mask -> request its live in value
  auto & loopPreHead = *loop.getLoopPreheader();
  auto & preHeadTerm = *loopPreHead.getTerminator();
  auto & loopHead = *loop.getHeader();
  IndexSet headerIndices;
  maskEx.getPredecessorEdges(preHeadTerm, loopHead, headerIndices);
  auto & initMask = maskEx.requestJoinedEdgeMask(preHeadTerm, headerIndices);

  // attach the missing preheader input to the live mask
  auto & liveMaskPhi = cast<PHINode>(*maskEx.getBlockMask(loopHead));
  int initIdx = liveMaskPhi.getBasicBlockIndex(&loopPreHead);
  assert(initIdx >= 0 && "loop pre-header has changed!");
  liveMaskPhi.setIncomingValue(initIdx, &initMask);

  auto * divExit = loop.getExitBlock();
  assert(divExit);
  maskEx.setBlockMask(*divExit, initMask);
  vecInfo.setPredicate(*divExit, initMask);

// lower latch updates to selects
  loopSession->lowerTrackerUpdates();
}

DivLoopTrans::DivLoopTrans(PlatformInfo & _platInfo, VectorizationInfo & _vecInfo, MaskExpander & _maskEx, FunctionAnalysisManager &FAM)
: platInfo(_platInfo)
, vecInfo(_vecInfo)
, maskEx(_maskEx)
, FAM(FAM)
, loopInfo(*FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction()))
, boolTy(Type::getInt1Ty(vecInfo.getContext()))
, numDivergentLoops(0)
, numKillExits(0)
{}


DivLoopTrans::~DivLoopTrans() {}

bool
DivLoopTrans::transformDivergentLoopControl(Loop & loop) {
  bool hasDivergentLoops = false;

  // make this loop uniform (all remaining divergent loops are properly nested)
  if (vecInfo.isDivergentLoop(loop)) {
    ++numDivergentLoops;
    hasDivergentLoops = true;

    auto * loopSession = new TransformSession(loop, loopInfo, vecInfo, platInfo, maskEx);
    loopSession->transformLoop();
    sessions[&loop] = loopSession;

    // mark loop as uniform
    vecInfo.removeDivergentLoop(loop);
  }

  for (auto * childLoop : loop) {
    hasDivergentLoops |= transformDivergentLoopControl(*childLoop);
  }

  IF_DEBUG_DLT { errs() << "# vecInfo after liveout repair:\n"; vecInfo.dump(); }

  return hasDivergentLoops;
}

void
DivLoopTrans::transformDivergentLoops() {
  IF_DEBUG_DLT { errs() << "-- divLoopTrans log --\n"; }

  // create tracker/update phis and make all loops uniform
  bool hasDivergentLoops = false;
  IF_DEBUG_DLT { errs() << "# 1. transforming control in divergent loops:\n"; }
  for (auto * loop : loopInfo) {
    hasDivergentLoops |= transformDivergentLoopControl(*loop);
  }
  if (!hasDivergentLoops) {
     IF_DEBUG_DLT { errs() << "-- no divergent loops. EOF divLoopTrans --\n"; }
     return;
  }

  // checkpoint:
  FAM.invalidate<DominatorTreeAnalysis>(vecInfo.getScalarFunction());
  FAM.invalidate<PostDominatorTreeAnalysis>(vecInfo.getScalarFunction());
  IF_DEBUG_DLT {
    Dump(vecInfo.getScalarFunction());
    auto &LI = *FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction());
    LI.print(errs());
    LI.verify(FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction())); // must not recompute
  }

  // request initial loop live masks
  IF_DEBUG_DLT { errs() << "# 2. requesting initial loop live masks:\n"; }
  for (auto * loop : loopInfo) {
    addLoopInitMasks(*loop);
  }

  IF_DEBUG_DLT {
    errs() << "-- divLoopTrans finished. VecInfo::\n";
    vecInfo.dump();
    errs() << "-- Verifying (non-dom anticipated):\n";
    verifyFunction(vecInfo.getScalarFunction(), &errs());
    // assert(!errorFound);
    errs() << "-- EOF divLoopTrans --\n";
  }

  if (numDivergentLoops > 0) {
    Report() << "divLoopTrans: transformed " << numDivergentLoops << " loops with " << numKillExits << " kill exits.\n";
  }

// cleanup
  for (auto it : sessions) {
    delete  it.second;
  }
  sessions.clear();
}


} // namespace rv

