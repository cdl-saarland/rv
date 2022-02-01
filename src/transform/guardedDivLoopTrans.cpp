//===- rv/transform/guardedDivLoopTrans.cpp - make divergent loops uniform --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/transform/guardedDivLoopTrans.h"

#include "rv/PlatformInfo.h"
#include "utils/rvTools.h"

#include "rvConfig.h"
#include "rv/rvDebug.h"
#include "report.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/IRBuilder.h>

#include <llvm/Transforms/Utils/SSAUpdater.h>
#include <llvm/IR/Verifier.h>

#if 1
#define IF_DEBUG_DLT IF_DEBUG
#else
#define IF_DEBUG_DLT if (true)
#endif

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

void
GuardedTransformSession::finalizeLiveOutTracker(GuardedTrackerDesc & desc) {
  if (!desc.trackerPhi || !desc.updatePhi) return;

  auto & trackerPhi = *desc.trackerPhi;
  auto & updPhi = *desc.updatePhi;

  assert(!getShadowInput(updPhi) && "already has a shadow input!");

  // trackerPhi should be piped through on disabled lanes.
  setShadowInput(updPhi, trackerPhi);

  // preserve the tracker also on active lanes if the thread stays in the loop.
  AttachMissingInputs(updPhi, trackerPhi);
}

void
GuardedTransformSession::finalizeLiveOutTrackers() {
  // FIXME repair SSA (non-dominating defs)
  finalizeLiveOutTracker(liveMaskDesc);

  for (auto & itUpdate : liveOutDescs) {
    finalizeLiveOutTracker(itUpdate.second);
  }
  for (auto & itUpdate : exitDescs) {
    finalizeLiveOutTracker(itUpdate.second);
  }
}

const GuardedTrackerDesc &
GuardedTransformSession::getGuardedTrackerDesc(const llvm::Value& val) const {
  auto it = liveOutDescs.find(&val);
  assert(it != liveOutDescs.end());
  return it->second;
}

GuardedTrackerDesc &
GuardedTransformSession::requestGuardedTrackerDesc(const llvm::Value& val) {
  auto it = liveOutDescs.find(&val);
  if (it != liveOutDescs.end()) return it->second;
  return liveOutDescs.insert(std::make_pair(&val, GuardedTrackerDesc())).first->second;
}

void
GuardedTransformSession::addInputForHeaderCarryPhis(llvm::BasicBlock& SrcBlock) {
  assert(pureLatch && "pure latch not yet established!");
  // Attach undef inputs in the pure latch for this new incoming edge
  for (PHINode * HeaderCarryPhi : PureDomPhis) {
    HeaderCarryPhi->addIncoming(UndefValue::get(HeaderCarryPhi->getType()), &SrcBlock);
  }
}

void
GuardedTransformSession::transformLoop() {
  IF_DEBUG_DLT { errs() << "dlt: Transforming loop " << loop.getName() << "\n:"; loop.print(errs()); }

  assert(vecInfo.isDivergentLoop(loop) && "trying to convert a non-divergent loop");

// creates cascading phi nodes to track loop live outs
  SmallVector<Loop::Edge, 4> loopExitEdges;
  loop.getExitEdges(loopExitEdges);

  auto & loopHeader = *loop.getHeader();
  auto & anyFunc = platInfo.requestRVIntrinsicFunc(RVIntrinsic::Any);

// divExit will be the only exit from the transformed loop,
   BasicBlock & fusedExit =  *BasicBlock::Create(anyFunc.getContext(), loopName + ".divexit", loopHeader.getParent(), &loopHeader);
   if (loop.getParentLoop()) {
     loop.getParentLoop()->addBasicBlockToLoop(&fusedExit, loopInfo);
   }

  const VectorShape loopControlShape = VectorShape::varying(); // FIXME TensorRV stub

// insert phi_live (active live threads in the loop)
  IRBuilder<> trackerBuilder(&loopHeader, loopHeader.getFirstInsertionPt());
  auto * boolTy = trackerBuilder.getInt1Ty();

// create the loop live mask (predicate of the loop header)
  liveMaskDesc.trackerPhi = trackerBuilder.CreatePHI(boolTy, 2, loopName + ".live");
  vecInfo.setVectorShape(*liveMaskDesc.trackerPhi, loopControlShape);
  liveMaskDesc.trackerPhi->addIncoming(ConstantInt::getTrue(trackerBuilder.getContext()), loop.getLoopPreheader());
  setShadowInput(*liveMaskDesc.trackerPhi, *ConstantInt::getFalse(trackerBuilder.getContext()));


// create !any(phi_live) loop exit and offset the original loop header (guarded by phi_live)
   //
   // loopHeader (br !any(live), divExitBlock, testHead)
   //     |
   // testHead (br live, offsetHead, pureLatch)
   //     |
   // offsetHead
   //    ...
   // pureLatch
   //
   {
     auto ItFirstInst = loopHeader.getFirstNonPHIOrDbgOrLifetime();

     // the original loop header (without the phis)
     this->offsetHead = loopHeader.splitBasicBlock(ItFirstInst->getIterator());
     this->offsetHead->setName(loopName + ".offset");
     loopHeader.getTerminator()->eraseFromParent();
     loop.addBasicBlockToLoop(offsetHead, loopInfo);

     // an immediate block to encode make loop execution conditional on phi_live
     this->testHead = BasicBlock::Create(offsetHead->getContext(), loopName + ".test", loopHeader.getParent(), offsetHead);
     loop.addBasicBlockToLoop(testHead, loopInfo);
   }

  // create a dedicated latch block for the loop (also live tracking infrastructure)
  requestPureLatch(); // creates maskUpdatePHi and pureLatch

   {
     IRBuilder<> testBuilder(testHead);
     auto & liveBr = *testBuilder.CreateCondBr(liveMaskDesc.trackerPhi, offsetHead, pureLatch);
     vecInfo.setVectorShape(liveBr, VectorShape::varying());
   }

   // discard loop carry if reaching pureLatch from offsetHead (thread has
   // dropped out already)
   addInputForHeaderCarryPhis(*testHead);

   // insert an all-false test on phi_live to exit the loop from the new header
   {
     IRBuilder<> headerBuilder(&loopHeader);
     auto * continueCond =
       headerBuilder.CreateCall(&anyFunc, liveMaskDesc.trackerPhi, loopName + ".exec");
     auto & anyBr = *headerBuilder.CreateCondBr(continueCond, testHead, &fusedExit);
     vecInfo.setVectorShape(anyBr, VectorShape::uni());
     vecInfo.setVectorShape(*continueCond, VectorShape::uni());
   }
   trackerBuilder.SetInsertPoint(&loopHeader, loopHeader.getFirstInsertionPt()); // reset tracker builder to a sane insertion point

  IF_DEBUG_DLT { errs() << "after header offsetting. "; vecInfo.dump(); }

  IRBuilder<> latchBuilder(pureLatch, pureLatch->getFirstInsertionPt());

// create tracking infrastructure for the loop exits
  assert(liveOutDescs.empty() && "start indexing live out values");
  for (auto & edge : loopExitEdges) {
    const VectorShape exitShape = VectorShape::varying();
    auto * exitingBlock = const_cast<BasicBlock*>(edge.first);
    std::string exitingName;
    if (exitingBlock == &loopHeader) {
      // header exiting branches have been deferred to the offset header
      exitingBlock = offsetHead;
      exitingName = exitingBlock->getName().str();
    } else {
      exitingName = exitingBlock->getName().str();
    }
    auto & exitBlock = *const_cast<BasicBlock*>(edge.second);
    auto exitName = exitBlock.getName().str();

  // track live out threads for this exit
    GuardedTrackerDesc exitDesc;
    exitDesc.trackerPhi = trackerBuilder.CreatePHI(boolTy, 2, exitName + ".xtrack");
    vecInfo.setVectorShape(*exitDesc.trackerPhi, exitShape);
    exitDesc.updatePhi = latchBuilder.CreatePHI(boolTy, 2, exitName + ".xupd");
    vecInfo.setVectorShape(*exitDesc.updatePhi, exitShape);
    exitDescs[&exitBlock] = exitDesc;
    exitDesc.trackerPhi->addIncoming(latchBuilder.getFalse(), loop.getLoopPreheader());
    exitDesc.trackerPhi->addIncoming(exitDesc.updatePhi, pureLatch);

    // if exiting from a nested loop we will need to create LCSSA phis in the rebound block
    Loop * leftLoop = loopInfo.getLoopFor(exitingBlock);
    bool reboundingNestedExit = leftLoop != &loop;

    // collect continue block, control context
    auto & exitingBr = *cast<BranchInst>(exitingBlock->getTerminator());
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
    BasicBlock * reboundBlock = exitingBlock;
    if (reboundingNestedExit || (oldLatch == exitingBlock)) {
      reboundBlock = BasicBlock::Create(exitingBlock->getContext(), exitName + ".rebound", exitingBlock->getParent(), pureLatch);
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

    // loop carried defs from rebound block are 'undef'
    addInputForHeaderCarryPhis(*reboundBlock);

    // mask out this thread in the live mask when the exit is taken
    liveMaskDesc.updatePhi->addIncoming(ConstantInt::getFalse(exitingBlock->getContext()), reboundBlock);
    // set the exit flag if the exit is taken
    exitDesc.updatePhi->addIncoming(ConstantInt::getTrue(exitingBlock->getContext()), reboundBlock);

  // maintain LCSSA phis in reboundBlock
    if (reboundingNestedExit) {
      IRBuilder<> rebBuilder(reboundBlock, reboundBlock->begin());
      ForAllLiveouts(exitBlock, [&](PHINode & lcPhi, int slot) {
          auto & liveOut = *lcPhi.getIncomingValue(slot);
          auto & nestedPhi = *rebBuilder.CreatePHI(liveOut.getType(), 1, liveOut.getName() + ".lcssa");
          vecInfo.setVectorShape(nestedPhi, vecInfo.getVectorShape(lcPhi));
          nestedPhi.addIncoming(&liveOut, exitingBlock);
          lcPhi.setIncomingValue(slot, &nestedPhi);
      });
    }

  // create live out trackers
    IF_DEBUG_DLT { errs() << "Creating live-out trackers for exit (" << exitName <<").\n"; }
    const bool IsDivExit = vecInfo.isDivergentLoopExit(exitBlock);
    ForAllLiveouts(exitBlock, [&](PHINode & lcPhi, int slot) {
      auto & liveOut = *lcPhi.getIncomingValue(slot);
      auto & trackerDesc = requestGuardedTrackerDesc(liveOut);

      IF_DEBUG_DLT { errs() << "Live out " << liveOut.getName() << "\n"; }

      if (IsDivExit) {
        // divergent live-outs need a tracker/update setup
        if (!trackerDesc.updatePhi) {
          assert(!trackerDesc.trackerPhi && "tracker/update infrastructure already initialized?");
          trackerDesc.trackerPhi = trackerBuilder.CreatePHI(liveOut.getType(), 2, liveOut.getName() + ".track");
          vecInfo.setVectorShape(*trackerDesc.trackerPhi, exitShape);
          trackerDesc.trackerPhi->addIncoming(UndefValue::get(liveOut.getType()), loop.getLoopPreheader());
          trackerDesc.updatePhi = latchBuilder.CreatePHI(liveOut.getType(), 2, liveOut.getName() + ".upd");
          trackerDesc.trackerPhi->addIncoming(trackerDesc.updatePhi, pureLatch);
          vecInfo.setVectorShape(*trackerDesc.updatePhi, exitShape);
        }

      // if the exit was taken write the current value of @liveOut to the tracker
        trackerDesc.updatePhi->addIncoming(&liveOut, reboundBlock);

      } else {
        // Kill-exit liveouts only need a dominating def at the loop header (wrapPhi)
        if (trackerDesc.wrapPhi) { return; }

        trackerDesc.wrapPhi = trackerBuilder.CreatePHI(liveOut.getType(), 2, liveOut.getName() + ".wrap");
        trackerDesc.wrapPhi->addIncoming(UndefValue::get(liveOut.getType()), loop.getLoopPreheader());
        trackerDesc.wrapPhi->addIncoming(&liveOut, pureLatch);
        auto loShape = vecInfo.getVectorShape(liveOut);
        vecInfo.setVectorShape(*trackerDesc.wrapPhi, loShape);
      }
    });
  }

  IF_DEBUG_DLT { errs() << "after live out tracking. "; vecInfo.dump(); }

// create an exit cascade
   assert(pureLatch->getTerminator()->getNumSuccessors() == 1);
   // build the new uniform exit branch
   pureLatch->getTerminator()->eraseFromParent();
   {
     IRBuilder<> latchBuilder(pureLatch);
     latchBuilder.CreateBr(&loopHeader);
   }

// sort exits for cascading order
   // since all divergent exits have to be visited before any kill exits first emit all divergent exits then all kill exits
   // The linearizer will read the cascade and make sure that all divergent exits will be processed before going to any uniform exits
   std::vector<std::pair<BasicBlock&, BasicBlock&>> exitStack;
   DenseSet<const BasicBlock*> seenExits;
   for (auto & edge : loopExitEdges) {
     auto & exitingBlock = *remapExitingBlock(const_cast<BasicBlock*>(edge.first));
     auto & exitBlock = *const_cast<BasicBlock*>(edge.second);

     if (!vecInfo.isKillExit(exitBlock)) exitStack.emplace_back(exitingBlock, exitBlock);
   }
   for (auto & edge : loopExitEdges) {
     auto & exitingBlock = *remapExitingBlock(const_cast<BasicBlock*>(edge.first));
     auto & exitBlock = *const_cast<BasicBlock*>(edge.second);
     if (vecInfo.isKillExit(exitBlock)) {
       ++numKillExits; // stats
       exitStack.emplace_back(exitingBlock, exitBlock);
     } else {
       ++numDivExits;
     }
   }
   BasicBlock & lastExit = exitStack[exitStack.size() - 1].second;

   IRBuilder<> exitBuilder(&fusedExit);

   // divergence shape
   const VectorShape exitShape = VectorShape::varying();

// materialize the exit cascade
   // materialize all exits in the stack after the loop
   struct LiveOutPhis {
     PHINode * trackedPhi[2]; // one slot for each exit type
     enum Type {
       KILL = 0,
       DIVERGENT = 1
     };
     LiveOutPhis() { trackedPhi[0] = trackedPhi[1] = nullptr; }
   };

   IF_DEBUG_DLT vecInfo.dump();

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
       IRBuilder<> divExitBuilder(&fusedExit);
       if (!fusedExit.empty()) divExitBuilder.SetInsertPoint(&*fusedExit.begin());
       exitMaskPhi = divExitBuilder.CreatePHI(boolTy, 1, exitBlock.getName().str() + ".xlcssa");
       vecInfo.setVectorShape(*exitMaskPhi, exitShape);
     }

     exitMaskPhi->addIncoming(exitDescs[&exitBlock].trackerPhi, &loopHeader);

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
     IRBuilder<> divExitBuilder(&fusedExit);
     if (!fusedExit.empty()) divExitBuilder.SetInsertPoint(&*fusedExit.begin());

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
         // we already have an LCSSA phi for this live-out value and exit type (divergent or kill)
         exitPhi = itKnown->second.trackedPhi[exitType];
       } else {
         LiveOutPhis loPhis = liveOutPhis[&liveOutVal];

         // create an LCSSA phi
         exitPhi = divExitBuilder.CreatePHI(lcPhi.getType(), 1, liveOutVal.getName() + ".lcssa");
         vecInfo.setVectorShape(*exitPhi, exitShape);
         auto & trackerDesc = getGuardedTrackerDesc(liveOutVal);
         auto * liveOutDef = killExit ? trackerDesc.wrapPhi : trackerDesc.trackerPhi;
         exitPhi->addIncoming(liveOutDef, &loopHeader);
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
     errs() << "dlt: loop after conversion:\n";
     loop.print(errs());
     errs() << "dlt: vecInfo after conversion of " << loop.getName() << "\b";
     vecInfo.dump();
   }
}

// split the latch if it is not pure (only a terminator)
BasicBlock &
GuardedTransformSession::requestPureLatch() {
  // we cache the pure latch since we make it impure again along the way
  if (pureLatch) return *pureLatch;

  assert(liveMaskDesc.trackerPhi && "need to materialize live tracker first!");
  IF_DEBUG_DLT { errs() << "# reqPureLatch " << loop.getName() << "\n"; }

  auto & latchBlock = *loop.getLoopLatch();
  auto & header = *loop.getHeader();

  // create a pure latch block
  pureLatch = BasicBlock::Create(latchBlock.getContext(), loopName + ".pure", latchBlock.getParent(), &latchBlock);
  oldLatch = &latchBlock;

  // create a mask update in the latch
  IRBuilder<> PureLatchBuilder(pureLatch);
  liveMaskDesc.updatePhi = PureLatchBuilder.CreatePHI(PureLatchBuilder.getInt1Ty(), 2, loopName + ".live.upd");
  liveMaskDesc.updatePhi->addIncoming(liveMaskDesc.trackerPhi, oldLatch);
  liveMaskDesc.updatePhi->addIncoming(liveMaskDesc.trackerPhi, testHead);
  vecInfo.setVectorShape(*liveMaskDesc.updatePhi, VectorShape::varying()); // TODO join of all exit shapes

  // latchPhi updates the loop mask depending on the predecessors
  // the pure latch is the dedicated predecessor of the loop header so we can safely use the live.upd mask here
  liveMaskDesc.trackerPhi->addIncoming(liveMaskDesc.updatePhi, pureLatch);

  // Fix the header phi nodes
  // - later CFG transforms may make some loop-carried defs non-dominating (insert phi nodes in the latch now).
  // - fix header phi incoming blocks (the pure latch breaks this edge)
  for (auto & inst : header) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) break;
    int latchIdx = phi->getBasicBlockIndex(oldLatch);
    if (latchIdx < 0) continue; // not a phi node of the original loop.

    // update incoming block
    phi->setIncomingBlock(latchIdx, pureLatch);

    // create a dominating definition
    // (reaches if loop was not left in this iteration)
    auto *InVal = phi->getIncomingValue(latchIdx);
    auto *InInst = dyn_cast<Instruction>(InVal);
    if (!InInst) continue;

    auto *PLPhi = PureLatchBuilder.CreatePHI(InInst->getType(), 2, phi->getName() + ".pure.dom");
    vecInfo.setVectorShape(*PLPhi, vecInfo.getVectorShape(*phi));
    PLPhi->addIncoming(InVal, oldLatch);
    PureDomPhis.push_back(PLPhi); // memorize for later attachment of 'undef' inputs for other incoming edges
    phi->setIncomingValue(latchIdx, PLPhi);
  }

  // finalize control flow
  // insert on the latch edge
  auto & latchBr = *cast<BranchInst>(latchBlock.getTerminator());
  if (latchBr.isConditional()) {
    bool latchOnFalse = latchBr.getSuccessor(1) == &header;
    latchBr.setSuccessor(latchOnFalse, pureLatch);
  } else {
    assert(latchBr.getSuccessor(0) == &header);
    latchBr.setSuccessor(0, pureLatch);
  }
  auto & pureLatchBr = *PureLatchBuilder.CreateBr(&header);
  vecInfo.setVectorShape(pureLatchBr, VectorShape::uni());

  // register with LoopInfo & LoopTracker
  loop.addBasicBlockToLoop(pureLatch, loopInfo);


  assert((loop.getLoopLatch() == pureLatch) && "latch replacement didn't work");
  return *pureLatch;
}

void
GuardedDivLoopTrans::addLoopInitMasks(llvm::Loop & loop) {
  // FIXME use mask futures instead
  for (auto * childLoop : loop) {
    addLoopInitMasks(*childLoop);
  }

  auto it = sessions.find(&loop);
  if (it == sessions.end()) return; // uniform loop
  auto * loopSession = it->second;

// add defaulting definitions on tracker phis
  loopSession->finalizeLiveOutTrackers();
}

GuardedDivLoopTrans::GuardedDivLoopTrans(PlatformInfo & _platInfo, VectorizationInfo & _vecInfo, llvm::FunctionAnalysisManager &FAM)
: platInfo(_platInfo)
, vecInfo(_vecInfo)
, FAM(FAM)
, boolTy(Type::getInt1Ty(vecInfo.getContext()))
, numUniformLoops(0)
, numDivergentLoops(0)
, numKillExits(0)
, numDivExits(0)
{}


GuardedDivLoopTrans::~GuardedDivLoopTrans() {}

bool
GuardedDivLoopTrans::transformDivergentLoopControl(LoopInfo & LI, Loop & loop) {
  bool hasDivergentLoops = false;

  // make this loop uniform (all remaining divergent loops are properly nested)
  if (vecInfo.isDivergentLoop(loop)) {
    IF_DEBUG_DLT {
      errs() << "dlt: Transforming divergent loop: " << loop.getName() << "\n";
    }

    assert(vecInfo.inRegion(*loop.getHeader()) && "loop outside the Region marked as divergent");
    ++numDivergentLoops;
    hasDivergentLoops = true;

    auto * loopSession = new GuardedTransformSession(loop, LI, vecInfo, platInfo);
    loopSession->transformLoop();
    numKillExits += loopSession->numKillExits; // accumulate global stats
    numDivExits += loopSession->numDivExits; // accumulate global stats
    sessions[&loop] = loopSession;

    // mark loop as uniform
    vecInfo.removeDivergentLoop(loop);
  } else if (vecInfo.inRegion(*loop.getHeader())) {
    ++numUniformLoops;
  }

  for (auto * childLoop : loop) {
    hasDivergentLoops |= transformDivergentLoopControl(LI, *childLoop);
  }

  IF_DEBUG_DLT { errs() << "# vecInfo after liveout repair:\n"; vecInfo.dump(); }

  return hasDivergentLoops;
}

void
GuardedDivLoopTrans::transformDivergentLoops() {
  IF_DEBUG_DLT { errs() << "-- divLoopTrans log --\n"; }
  auto &LI = *FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction());

  // verify incoming analysis sructures
  IF_DEBUG_DLT {
    errs() << "dlt: Verifying LI:\n";
    auto & DT = FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction());
    LI.verify(DT);
    errs() << "dlt: LI OK: LI before DLT:\n";
    LI.print(errs());
    errs() << "dlt: Beginning transform..\n";
  }

  // create tracker/update phis and make all loops uniform
  bool hasDivergentLoops = false;
  IF_DEBUG_DLT { errs() << "# 1. transforming control in divergent loops:\n"; }
  for (auto * loop : LI) {
    hasDivergentLoops |= transformDivergentLoopControl(LI, *loop);
  }

  if (!hasDivergentLoops) {
     IF_DEBUG_DLT { errs() << "-- no divergent loops. EOF divLoopTrans --\n"; }

  } else {
    // checkpoint:
    auto PA = PreservedAnalyses::all();
    PA.abandon<DominatorTreeAnalysis>();
    PA.abandon<PostDominatorTreeAnalysis>();
    FAM.invalidate(vecInfo.getScalarFunction(), PA);
    IF_DEBUG_DLT {
      Dump(vecInfo.getScalarFunction());
      LI.print(errs());
      LI.verify(FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction())); // must not recompute
    }

    // request initial loop live masks
    IF_DEBUG_DLT { errs() << "# 2. requesting initial loop live masks:\n"; }
    for (auto * loop : LI) {
      addLoopInitMasks(*loop);
    }

    IF_DEBUG_DLT {
      errs() << "-- divLoopTrans finished. VecInfo::\n";
      vecInfo.dump();
      errs() << "-- Verifying :\n";
      if (verifyFunction(vecInfo.getScalarFunction(), &errs())) {
        Error() << " dlt broke the function!\n";
      }
      // assert(!errorFound);
      errs() << "-- EOF divLoopTrans --\n";
    }
  }

  Report() << "divLoopTrans:\n\t" << numUniformLoops << " uniform loops.\n";

  if (numDivergentLoops > 0) {
    ReportContinue()
        << "\t" << numDivergentLoops << " loops transformed,\n"
        << "\t" << numDivExits << " divergent exits,\n"
        << "\t" << numKillExits << " kill exits.\n";
  }

// cleanup
  for (auto it : sessions) {
    delete  it.second;
  }
  sessions.clear();
}


} // namespace rv
