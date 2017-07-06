//===- divLoopTrans.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// Divergent loop transformation
//
// This transformation eliminates divergent loop in the region


#include "rv/PlatformInfo.h"
#include "rv/transform/divLoopTrans.h"
#include "rv/transform/maskExpander.h"

#include "rvConfig.h"
#include "report.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"

#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "llvm/IR/Verifier.h"

#include "llvm/IR/Metadata.h"

#define IF_DEBUG_DLT IF_DEBUG

using namespace llvm;


// TODO factor to cfg utility module
static void
InsertAtFront(BasicBlock & block, Instruction & inst) {
  block.getInstList().insert(block.begin(), &inst);
}

namespace rv {

// TODO use one live value tracker for all loops (higher tracker re-use)
class LiveValueTracker {
  VectorizationInfo & vecInfo;
  DominatorTree & domTree;
  LoopInfo & loopInfo;
  IntegerType * boolTy;

public:
  // maps live out values to chains of tracker phis (inner-most phi of chain)
  std::set<PHINode*> trackerPhis;
  DenseMap<Instruction*, PHINode*> liveOutPhis;
  SmallPtrSet<PHINode*, 16> latchUpdatePhis;

  // returns whether this phi is part of a tracker cascade
  bool
  isTrackerPhi(Instruction & inst) const {
    auto * phi = dyn_cast<PHINode>(&inst);
    return phi ? trackerPhis.count(phi) : false;
  }

  // create an updater for @tracker at @updateLoopTracker's loop level
  PHINode &
  requestLatchUpdater(LoopTracker & updateLoopTracker, PHINode & tracker) {
    const int initIdx = GetPreHeaderTrackerIndex();
    PHINode * currTracker = &tracker;
    std::string name = tracker.getName().str();
    auto & updateLoop = updateLoopTracker.loop;

    // traverse tracker chain until update level
    while (currTracker && (&updateLoop != loopInfo.getLoopFor(currTracker->getParent()))) {
      currTracker = dyn_cast<PHINode>(currTracker->getIncomingValue(initIdx));
    }

    assert(currTracker && "value not tracked @updateLoop level");

    const int latchInputIdx = GetLatchTrackerIndex();
    auto * latchVal = currTracker->getIncomingValue(latchInputIdx);
    auto * latchPhi = dyn_cast<PHINode>(latchVal);

    // return if this is a known latch updater
    if (latchUpdatePhis.count(latchPhi)) return *latchPhi;

    auto & liveOutUpdate = *PHINode::Create(currTracker->getType(), 2, name + ".upd", &*updateLoopTracker.pureLatch->begin());
    vecInfo.setVectorShape(liveOutUpdate, VectorShape::varying()); // TODO use control shape
    // liveOutUpdate.addIncoming(currTracker, updateLoopTracker.oldLatch);
    currTracker->setIncomingValue(latchInputIdx, &liveOutUpdate);



    latchUpdatePhis.insert(&liveOutUpdate);
    return liveOutUpdate;
  }

  // return the incoming index of the exitblock
  int getLoopBlockIndex(PHINode & lcPhi, Loop & hostLoop) {
    for (uint i = 0; i < lcPhi.getNumIncomingValues(); ++i) {
      if (hostLoop.contains(lcPhi.getIncomingBlock(i))) return i;
    }
    return -1;
  }

  // return the successor index that leaves the loop
  int getLoopExitIndex(Instruction & inst, Loop & hostLoop) {
    auto & branch = cast<BranchInst>(inst);
    if (hostLoop.contains(branch.getSuccessor(0))) return 1;
    else if (hostLoop.contains(branch.getSuccessor(1))) return 0;
    else abort();
  }

  static int GetPreHeaderTrackerIndex() { return 0; }
  static int GetLatchTrackerIndex() { return 1; }

public:
  LiveValueTracker(VectorizationInfo & _vecInfo, DominatorTree & _domTree, LoopInfo & _loopInfo)
  : vecInfo(_vecInfo), domTree(_domTree), loopInfo(_loopInfo), boolTy(Type::getInt1Ty(vecInfo.getContext()))
  {}

  // inserts a tracker PHI into the loop headers surrounding @defInst up to @hostLoop
  // returns the tracker update valid at the latch block
  // will set the outermost init value to undef (or @initVal, if given)
  PHINode &
  requestTracker(Instruction & defInst, BasicBlock & exiting, Loop & hostLoop, Type & type, llvm::StringRef prefix, llvm::Value * customInitVal = nullptr) {
    Loop * defLoop = loopInfo.getLoopFor(&exiting);

    assert(defLoop && "defInst it not live out from any loop");

  // check if already have a tracker for this value
    auto it = liveOutPhis.find(&defInst);
    if (it != liveOutPhis.end()) {
      auto & phi = *it->second;
      return phi;
    }

  // create a PHI chain from @defInst up to this loop
    auto * trackedLoop = defLoop;
    PHINode * nestedTracker = nullptr;

    PHINode * innerTrackerPhi = nullptr;

    auto * initVal = customInitVal ? customInitVal : UndefValue::get(&type);

  // create a tracker PHI for every loop crossing the exit edge
    while (
        trackedLoop &&
        trackedLoop->getLoopDepth() >= hostLoop.getLoopDepth()
    ) {
      auto & trackedLoopHeader = *trackedLoop->getHeader();
      auto & trackedPreHeader = *trackedLoop->getLoopPreheader();

    // create a tracker phi in every surrounding loop of @defInst
      auto * phi = PHINode::Create(defInst.getType(), 2, prefix + ".track", &*trackedLoopHeader.getFirstInsertionPt()); // TODO vector shape
      trackerPhis.insert(phi);
      vecInfo.setVectorShape(*phi, VectorShape::varying());

      // remember inner-most tracker Phi
      if (!innerTrackerPhi) innerTrackerPhi = phi;

    // preheader input: tracker state of outer phi
      // attach tracker input to nested tracker PHI
      if (nestedTracker) {
        nestedTracker->setIncomingValue(GetPreHeaderTrackerIndex(), phi);
      }

    // preheader input (undef)
      phi->addIncoming(initVal, &trackedPreHeader);

    // latch input: self-loop or tracker state from (inner) nestedPhi
      if (nestedTracker) {
         phi->addIncoming(nestedTracker, trackedLoop->getLoopLatch()); // take the nested value on the latch
      } else {
         phi->addIncoming(phi, trackedLoop->getLoopLatch()); // create a self loop
      }
      IF_DEBUG_DLT { errs() << "\t* trackerPHI (w/o liveIn update): " << *phi << "\n"; }

    // next outer loop
      nestedTracker = phi;
      trackedLoop = trackedLoop->getParentLoop();
    }

    IF_DEBUG_DLT { errs() << "\t- outer-most tracker " << *nestedTracker << "\n"; }
    IF_DEBUG_DLT { errs() << "\t- inner-most tracker " << *innerTrackerPhi << "\n"; }

  // attach trackerPHI inputs
    liveOutPhis[&defInst] = innerTrackerPhi;
    return *innerTrackerPhi;
  }

  static uint
  GetExitIndex(BasicBlock & exiting, Loop & loop) {
    auto & term = *exiting.getTerminator();
    for (uint i = 0; i < term.getNumSuccessors(); ++i) {
      if (!loop.contains(term.getSuccessor(i))) {
        return i;
      }
    }
    abort();
  }

  // integrate @updateInst into the tracker phi cascade (@tracker is innermost phi)
  void
  addTrackerUpdate(PHINode & tracker, Instruction & updateInst) {
    auto trackerShape = vecInfo.getVectorShape(updateInst);

  // promote the partial def to all surrounding loops
    Value * currentLiveInDef = &tracker;
    Instruction * currentPartialDef = &updateInst;
    auto * defBlock = currentPartialDef->getParent();
    Loop * currentLoop = loopInfo.getLoopFor(tracker.getParent());

    assert((loopInfo.getLoopFor(updateInst.getParent()) == currentLoop) && "updating on wrong loop level");

    // promote exit update to latch
    IF_DEBUG_DLT { errs() << "# addTrackerUpdate " << updateInst << " for update " << updateInst.getName() << "\n"; }
    while (isa<PHINode>(currentLiveInDef)) {
      auto & currPhi = *cast<PHINode>(currentLiveInDef);
      assert(currentLoop == loopInfo.getLoopFor(currPhi.getParent()) && "curr header PHI and curr loop out of sync");

      // we need to promote the live out tracker to its user outside of thsi loop
      // However we have two definitions for this value: the tracker PHI and its update operation
      // Hence, we need to repair SSA form on the way down to the user
      auto & currLoopHeader = *currPhi.getParent();
      auto & currLatchBlock = *currentLoop->getLoopLatch();

      IF_DEBUG_DLT { errs() << "\t- partial def: " << currentPartialDef->getName() << " to latch " << currLatchBlock.getName() << " of tracker PHI " << currPhi.getName() << "\n"; }

      // update SSA down to the latch to get a dominating definition
      if (!domTree.dominates(defBlock, &currLatchBlock)) {
        std::string phiName = currentLiveInDef->getName().str() + ".prom";

        // prepare ssa repair
        SmallVector<PHINode*, 8> phiVec;
        SSAUpdater ssaUpdater(&phiVec);
        ssaUpdater.Initialize(tracker.getType(), phiName);

        ssaUpdater.AddAvailableValue(&currLoopHeader, &currPhi);
        IF_DEBUG_DLT { errs() << "\t\t def " << currPhi.getName() << " @ " << currLoopHeader.getName() << "\n"; }
        ssaUpdater.AddAvailableValue(defBlock, currentPartialDef);
        IF_DEBUG_DLT { errs() << "\t\t def " << currentPartialDef->getName() << " @ " << defBlock->getName() << "\n"; }
        auto * promotedUpdate = ssaUpdater.GetValueAtEndOfBlock(&currLatchBlock);
        assert(isa<Instruction>(promotedUpdate));
        currentPartialDef = cast<Instruction>(promotedUpdate);

         for (auto * phi : phiVec) {
           vecInfo.setVectorShape(*phi, trackerShape);
         }
      }

      // the definition is dominating at the latch
      defBlock = &currLatchBlock;

      IF_DEBUG_DLT { errs() << "\tsetting update of PHI " << currPhi << " to promoted def " << *currentPartialDef << "\n"; }
      currPhi.setIncomingValue(GetLatchTrackerIndex(), currentPartialDef);

    // advance to next surrounding loop
      currentLiveInDef = currPhi.getIncomingValue(GetPreHeaderTrackerIndex());
      currentLoop = currentLoop->getParentLoop();
    }
  }

  // the last update to @tracker
  Value &
  getLastTrackerState(PHINode & tracker) {
  // windup to outer most tracker PHI
    auto * lastPhi = &tracker;
    Value * nextPreHeaderInput = lastPhi;
    while (isa<PHINode>(nextPreHeaderInput)) {
      lastPhi = cast<PHINode>(nextPreHeaderInput);
      nextPreHeaderInput = lastPhi->getIncomingValue(GetPreHeaderTrackerIndex());

    }
  // latch input (outer most update) of outer most tracker PHI
    return *lastPhi->getIncomingValue(GetLatchTrackerIndex());
  }

  PHINode &
  getTrackerAtLevel(PHINode & tracker, Loop & loop) {
  // windup to outer most tracker PHI
    auto * lastPhi = &tracker;
    Value * nextPreHeaderInput = lastPhi;
    while (isa<PHINode>(nextPreHeaderInput)) {
      auto * phiLoop = loopInfo.getLoopFor(lastPhi->getParent());
      if (!phiLoop || (phiLoop->getLoopDepth() <= loop.getLoopDepth())) {
        // we hit the requested loop level
        break;
      }

      lastPhi = cast<PHINode>(nextPreHeaderInput);
      nextPreHeaderInput = lastPhi->getIncomingValue(GetPreHeaderTrackerIndex());
    }

    assert(lastPhi && "no tracker phi at this loop level!");
    return *lastPhi;
  }

  // the last update to @tracker (in loop @loop)
  Value &
  getLastTrackerStateAtLevel(PHINode & tracker, Loop & loop) {
    // PHINode at this loop level
    PHINode & levelTracker = getTrackerAtLevel(tracker, loop);

    // latch input is live out tracker state
    return *levelTracker.getIncomingValue(GetLatchTrackerIndex());
  }

  // get the last tracker state for this live out value (which must be a loop carried instruction)
  Value & getTrackerStateForLiveOut(Instruction & liveOutInst) {
    auto it = liveOutPhis.find(&liveOutInst);
    assert(it != liveOutPhis.end() && "not a tracked value!");
    auto &tracker = *it->second;
    return getLastTrackerState(tracker);
  }

  PHINode *
  getTrackerForLiveOut(Instruction & liveOutInst) {
    auto it = liveOutPhis.find(&liveOutInst);
    if (it == liveOutPhis.end()) return nullptr;
    return it->second;
  }

  BasicBlock & getExitingBlock(BasicBlock & exitBlock, Loop & hostLoop) {
    for (auto * pred : predecessors(&exitBlock)) {
      if (hostLoop.contains(pred)) return *pred;
    }
    abort();
  }

  // adds all live out values on loop-exits to @exitBlock
  // FIXME this currently assumes that all out-of-loop uses pass through LCSSA Phis. However, uses by all out-of-loop instructions are set to use the tracker value instead.
};

static BranchInst&
CreateIf(IRBuilder<> & builder, Value & Cond, BasicBlock & IfTrue) {
  std::string name = Cond.getName().str() + ".false";
  auto & falseBlock = *BasicBlock::Create(builder.getContext(), name, IfTrue.getParent(), &IfTrue);
  auto & falseBranch = *builder.CreateCondBr(&Cond, &IfTrue, &falseBlock);
  builder.SetInsertPoint(&falseBlock);
  return falseBranch;
}

void
DivLoopTrans::trackPreservedLiveOuts(BasicBlock & exitBlock) {
  assert(vecInfo.isKillExit(exitBlock));

  auto itBegin = exitBlock.begin(), itEnd = exitBlock.end();
  for (auto it = itBegin; isa<PHINode>(*it) && it != itEnd; ++it) {
    auto & lcPhi = cast<PHINode>(*it);
    // if (lin.isRepairPhi(lcPhi)) continue; // not a PHI node of the original program
    assert(lcPhi.getNumIncomingValues() == 1 && "neither a late repair PHI nor a LCSSA PHI");
    addKillPhi(lcPhi);
  }
}

void
DivLoopTrans::trackLiveOuts(Loop & hostLoop, BasicBlock & exitingBlock, BasicBlock & exitBlock, BasicBlock & reboundBlock, LiveValueTracker & liveOutTracker) {
  assert(!vecInfo.isKillExit(exitBlock));

  IF_DEBUG_DLT {
    errs() << "# tracking live outs in exitBlock: \n" << exitBlock.getName() << "\n";
  }
  auto & leftLoop = *loopInfo.getLoopFor(&exitingBlock);
  auto & leftLoopTracker = requestLoopTracker(leftLoop); // FIXME the live mask has no registered tracker

  auto itBegin = exitBlock.begin(), itEnd = exitBlock.end();
  for (auto it = itBegin; isa<PHINode>(*it) && it != itEnd; ++it) {
    auto & lcPhi = cast<PHINode>(*it);
    // if (lin.isRepairPhi(lcPhi)) continue; // not a PHI node of the original program
    assert(lcPhi.getNumIncomingValues() == 1 && "neither a late repair PHI nor a LCSSA PHI");

  // do not track non-live carried values
    const int loopIncomingId = 0; // LCSSA
    assert(&exitingBlock == lcPhi.getIncomingBlock(loopIncomingId));

    auto * inInst = dyn_cast<Instruction>(lcPhi.getIncomingValue(loopIncomingId));
    if (!inInst) continue; // non loop-carried

    std::string name = inInst->getName().str();

  // fold the data flow through from exiting->exit through all crossing loops
    auto & tracker = liveOutTracker.requestTracker(*inInst, exitingBlock, hostLoop, *inInst->getType(), inInst->getName().str());

    // register liveout with updater phi
    auto & liveOutUpdate = liveOutTracker.requestLatchUpdater(leftLoopTracker, tracker);
    liveOutUpdate.addIncoming(inInst, &reboundBlock); // add the only proper update (all others will default to the tracker)

    // remember unfinished update (latch value edge is missing)
    leftLoopTracker.latchValueUpdates[inInst] = LoopTracker::ValueUpdate{&tracker, &liveOutUpdate};
  }
}

void
DivLoopTrans::fixDivergentLoopUpdates(llvm::Loop & loop, LiveValueTracker & liveOutTracker) {
  for (auto * childLoop : loop) {
    fixDivergentLoopUpdates(*childLoop, liveOutTracker);
  }

  // promote latch updates (SSA repair)
  // At this point, all exiting edges from this loop have been control converted
  if (vecInfo.isDivergentLoop(&loop)) {
    fixLatchUpdates(loop, liveOutTracker);
  }
}

void
DivLoopTrans::fixDivergentLiveOutUses(llvm::Loop & loop, LiveValueTracker & liveOutTracker) {
  for (auto * childLoop : loop) {
    fixDivergentLiveOutUses(*childLoop, liveOutTracker);
  }

  // promote latch updates (SSA repair)
  // At this point, all exiting edges from this loop have been control converted
  if (vecInfo.isDivergentLoop(&loop)) {
    fixLiveOutUses(loop, liveOutTracker);
  }

  // done: all control flow uniform now
  vecInfo.setLoopDivergence(loop, true);
}

Instruction&
DivLoopTrans::implementPhiUpdate(LoopTracker::ValueUpdate & valUpd) {
  assert(isa<PHINode>(valUpd.latchUpdate) && "already lowered this one??");
  auto & trackerPhi = *valUpd.valueTracker;
  auto & updPhi = cast<PHINode>(*valUpd.latchUpdate);
  auto & BB = *updPhi.getParent();

  auto shape = vecInfo.getVectorShape(trackerPhi);
  IRBuilder<> builder(updPhi.getParent(), updPhi.getIterator());

  Instruction * accu = &trackerPhi;
  for (BasicBlock * pred : predecessors(&BB)) {
    int i = updPhi.getBasicBlockIndex(pred);
    if (i < 0) continue; // implicit default edge

    auto * inBlock = updPhi.getIncomingBlock(i);
    auto * inVal = updPhi.getIncomingValue(i);
    if (inVal == &trackerPhi) continue; // explicit (redundant) default edge

    auto & edgeMask = maskEx.requestEdgeMask(*inBlock, BB);
    std::string name = updPhi.getName();

    // pick this incoming value if the edge is taken
    accu = cast<Instruction>(builder.CreateSelect(&edgeMask, inVal, accu, name));
    vecInfo.setVectorShape(*accu, shape);
  }

  // select cascade implements this phi
  updPhi.replaceAllUsesWith(accu);
  vecInfo.dropVectorShape(updPhi);
  updPhi.eraseFromParent();

  return *accu;
}

// after ALL divergent loops have been control converted fix the latch updates and exit conditions
void
DivLoopTrans::fixLatchUpdates(Loop & loop, LiveValueTracker & liveOutTracker) {
  IF_DEBUG_DLT { errs() << "# fixLatchUpdates " << loop.getName() << "\n"; }

  // loop control handles
  LoopTracker & loopTracker = getLoopTracker(loop);
  auto * divExitBlock = loop.getUniqueExitBlock();
  auto * exitingBlock = loop.getExitingBlock();
  assert(exitingBlock == loopTracker.pureLatch);
  assert(divExitBlock && "control transform should leave a single exit");

// repair LCSSA incoming block
   IF_DEBUG_DLT { errs() << "Patching incoming blocks in " << divExitBlock->getName() << "\n"; }
   for (auto & inst : *divExitBlock) {
     auto * lcPhi = dyn_cast<PHINode>(&inst);
     if (!lcPhi) break;

     // all lcPhis receive their incoming value from the pure latch
     lcPhi->setIncomingBlock(0, exitingBlock);
   }

// Lower live value updates to selects
  // lower update phis to explicit selects
  // (latchUpdate phis have implicit incoming undef edges, these will break SSAUpdater in addTrackerUpdate)
  for (auto & itValueUpd : loopTracker.latchValueUpdates) {
    LoopTracker::ValueUpdate & valueUpd = itValueUpd.second;
    // replace the update phi with a proper blend cascade
    valueUpd.latchUpdate = &implementPhiUpdate(valueUpd); // there will only be one update phi per loop level (in the pure latch with incoming edges for all divergent exits)
  }

// Implement the live mask update
  auto & liveUpdatePhi = *loopTracker.maskUpdatePhi;

  // all threads that survive to this point are live
  auto & survivorMask = maskEx.requestEdgeMask(*loopTracker.oldLatch, *loopTracker.pureLatch);
  survivorMask.setName(liveUpdatePhi.getName());

  IF_DEBUG_DLT errs() << "LIVE UPDATE FOR " << loop.getName() << "\n\told PHI: " << liveUpdatePhi << "\n\treplacement: " << survivorMask << "\n";
  liveUpdatePhi.replaceAllUsesWith(&survivorMask);
  vecInfo.dropVectorShape(liveUpdatePhi);
  liveUpdatePhi.eraseFromParent();
}

void
DivLoopTrans::fixLiveOutUses(Loop & loop, LiveValueTracker & liveOutTracker) {
  IF_DEBUG_DLT { errs() << "# fixLiveOutUses  " << loop.getName() << "\n"; }

  // loop control handles
  LoopTracker & loopTracker = getLoopTracker(loop);
  auto * divExitBlock = loop.getUniqueExitBlock();
  auto * exitingBlock = loop.getExitingBlock();
  assert(exitingBlock == loopTracker.pureLatch);
  assert(divExitBlock && "control transform should leave a single exit");


// from this point on the region must not contain PHI nodes that have inconsistent predecessor lists with their parent blocks
  // Otw SSAUpdater will break! (llvm FindPredecessorBlocks)

// promote live value updates to the latch blocks
  // attach explicit live out updates to latches
  for (auto & itValueUpd : loopTracker.latchValueUpdates) {
    LoopTracker::ValueUpdate & valueUpd = itValueUpd.second;
    // chain in the update instructions
    liveOutTracker.addTrackerUpdate(*valueUpd.valueTracker, *valueUpd.latchUpdate);
  }


// fix live outs in exit blocks
   // repair live out uses
   IF_DEBUG_DLT { errs() << "Patching live out values in " << divExitBlock->getName() << "\n"; }
   for (auto & inst : *divExitBlock) {
     auto * lcPhi = dyn_cast<PHINode>(&inst);
     if (!lcPhi) break;

     // all lcPhis receive their incoming value from the pure latch
     // lcPhi->setIncomingBlock(0, exitingBlock); // doing this early to have consistent PHI nodes for SSAUpdater (addTrackerUpdate)
     IF_DEBUG_DLT { errs() << "- lcPhi : " << *lcPhi << "\n"; }

     auto & liveOut = *lcPhi->getIncomingValue(0);
     if (!isa<Instruction>(liveOut)) {
        IF_DEBUG_DLT { errs() << "\t non-inst live out.\n"; }
        continue;
     };

     auto & liveOutInst = cast<Instruction>(liveOut);
     if (isKillPhi(*lcPhi)) {
       IF_DEBUG_DLT errs() << "\tkill exit. skip.\n";
       continue;
     }

     // otw replace old live out with tracker live out state
     auto * tracker = liveOutTracker.getTrackerForLiveOut(liveOutInst);

     if (liveOutTracker.isTrackerPhi(liveOutInst)) {
       // exit trackers insert their header tracker phi directly in LCSSA phis
       IF_DEBUG_DLT errs() << "\t direct tracker use in LCSSA phi -> promote\n";
       tracker = &cast<PHINode>(liveOutInst);
     } else {
       // regular tracked live outs have an associated tracker
       assert(tracker && "divergent exit live out was not tracked!");
       IF_DEBUG_DLT errs() << "\t use of live out in LCSSA phi -> promote\n";
     }

     // get the live out value at this loop level
     auto & liveOutUpd = liveOutTracker.getLastTrackerStateAtLevel(*tracker, loop);

     lcPhi->setIncomingValue(0, &liveOutUpd);
   }

   IF_DEBUG_DLT vecInfo.dump();
}

void
DivLoopTrans::convertToLatchExitLoop(Loop & loop, LiveValueTracker & liveOutTracker) {
  IF_DEBUG_DLT { errs() << "# convToLatchExitLoop " << loop.getName() << "\n"; }

  assert(vecInfo.isDivergentLoop(&loop) && "trying to convert a non-divergent loop");

// request a live mask for this loop
  auto & loopTracker = requestLoopTracker(loop);

// creates cascading phi nodes to track loop live outs
  SmallVector<Loop::Edge, 4> loopExitEdges;
  loop.getExitEdges(loopExitEdges);

// fold exits
  for (auto & edge : loopExitEdges) {
    auto exitShape = VectorShape::varying(); // TODO use the actual control shape

    // Note: exitingBlock may be in a child loop
    auto & exitingBlock = *const_cast<BasicBlock*>(edge.first);
    auto & exitBlock = *const_cast<BasicBlock*>(edge.second);
    Loop & leftLoop = *loopInfo.getLoopFor(&exitingBlock);

    // collect continue block, control context
    auto exitingName = exitingBlock.getName().str();
    auto exitName = exitBlock.getName().str();

    auto & exitingBr = *cast<BranchInst>(exitingBlock.getTerminator());
    bool exitOnFalse = exitingBr.getSuccessor(1) == &exitBlock;

// Set-up mask tracking infrastructure for this edge
    // we will need a pure latch for the left loop
    auto & leftLoopTracker = requestLoopTracker(leftLoop);

    // create live masks for all loops this edge bridges
    for (
        Loop * crossedLoop = leftLoop.getParentLoop();
        crossedLoop && (crossedLoop->getLoopDepth() >= loop.getLoopDepth());
        crossedLoop = crossedLoop->getParentLoop())
    {
      requestLoopTracker(*crossedLoop);
    }

    // create tracker phis for this exit (init value is false)
    // auto & exitCond = *cast<Instruction>(exitingBr.getCondition());
    auto * falseConst = ConstantInt::getFalse(exitingBr.getContext());
    auto * exitVal = cast<Instruction>(exitingBr.getCondition()); // FIXME there is no natural live out instruction available so we abuse the exiting condition
    PHINode & innerExitTracker = liveOutTracker.requestTracker(*exitVal, exitingBlock, loop, *falseConst->getType(), exitBlock.getName().str() + ".xmask", falseConst);

    // make sure we have a pure latch block in exiting loop
    auto & pureLatchBlock = requestPureLatch(leftLoopTracker);
    auto & latchMaskUpdate = *leftLoopTracker.maskUpdatePhi; // created on LiveOutTracker::requestPureLatch

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
    if (leftLoopTracker.oldLatch == &exitingBlock) {
      reboundBlock = BasicBlock::Create(exitingBlock.getContext(), exitName + ".rebound", exitingBlock.getParent(), &pureLatchBlock);
      leftLoop.addBasicBlockToLoop(reboundBlock, loopInfo);
      exitingBr.setSuccessor(exitOnFalse, reboundBlock);
      auto & reboundBr = *BranchInst::Create(&pureLatchBlock, reboundBlock);
      vecInfo.setVectorShape(reboundBr, VectorShape::uni());
    } else {
      exitingBr.setSuccessor(exitOnFalse, &pureLatchBlock);
    }

    // mask out this thread in the live mask when the exit is taken
    latchMaskUpdate.addIncoming(ConstantInt::getFalse(exitingBlock.getContext()), reboundBlock);

  // create an exit mask tracker
    // if this iteration leaves the loop (edge exiting->rebound taken) update the mask tracker
    auto & exitLatchPhi = leftLoopTracker.requestExitUpdate(exitBlock, innerExitTracker);
    vecInfo.setVectorShape(exitLatchPhi, exitShape);
    // if the exit was taken, set the exit mask to true and the live mask to false
    exitLatchPhi.addIncoming(ConstantInt::getTrue(exitingBlock.getContext()), reboundBlock);

  // process live out values (LCSSA based)
    if (vecInfo.isKillExit(exitBlock)) {
      // do not track LCSSA phis
      trackPreservedLiveOuts(exitBlock);
      ++numKillExits;
    } else {
      // create live value trackers & updaters for all live outs on this exit
      // kill exits transfer all control outside of the loop and so they do not need any trackers themselves
      trackLiveOuts(loop, exitingBlock, exitBlock, *reboundBlock, liveOutTracker);
    }

    IF_DEBUG_DLT {
      errs()
        << "- exitLatchPhi: " << exitLatchPhi << "\n"
        <<  "- latchMaskUpdate: " << latchMaskUpdate << "\n";
    }
  }

// create a dedicated, uniform exit branch
   auto & latchBlock = requestPureLatch(loopTracker);
   assert(latchBlock.getTerminator()->getNumSuccessors() == 1);

   auto & loopHeader = *loop.getHeader();
   std::string loopName = loop.getName().str();
   auto * anyFunc = platInfo.requestMaskReductionFunc("rv_any");

   // new joined latch exit
   auto & latchExit =  *BasicBlock::Create(latchBlock.getContext(), loopName + ".divexit", latchBlock.getParent(), &latchBlock);
   if (loop.getParentLoop()) {
     loop.getParentLoop()->addBasicBlockToLoop(&latchExit, loopInfo);
   }

   // build the new uniform exit branch
   latchBlock.getTerminator()->eraseFromParent();
   {
     IRBuilder<> builder(&latchBlock);
     auto * continueCond =
       builder.CreateCall(anyFunc, ArrayRef<Value*>{loopTracker.maskUpdatePhi}, loopName + ".cont");
     auto & anyBr= *builder.CreateCondBr(continueCond, &loopHeader, &latchExit);
     vecInfo.setVectorShape(anyBr, VectorShape::uni());
     vecInfo.setVectorShape(*continueCond, VectorShape::uni());
   }

// sort exits for cascading order
   // since all divergent exits have to be visited before any kill exits first emit all divergent exits then all kill exits
   // The linearizer will read the cascade and make sure that all divergent exits will be processed before going to any uniform exits
   std::vector<std::pair<BasicBlock&, BasicBlock&>> exitStack;
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
   IRBuilder<> builder(&latchExit);

// FIXME set proper masks on the exit cascade
// materialize the exit cascade
   // unify live outs as we go
   std::map<Instruction*, PHINode*> knownLiveOuts;

   // materialize all exits in the stack after the loop
   for (auto exitEdge : exitStack) {
     auto & exitingBlock = exitEdge.first;
     auto & exitBlock = exitEdge.second;

     IF_DEBUG_DLT {
       errs()
         << "Attaching exit: " << exitBlock.getName() << "\n"
         << "\tkill exit: " << vecInfo.isKillExit(exitBlock) << "\n";
     }

     bool killExit = vecInfo.isKillExit(exitBlock);
     VectorShape branchShape = killExit ? VectorShape::uni() : VectorShape::varying();

     // set the mask for this exit (so maskExpander will take that and not expand into the loop)
     PHINode * exitMaskPhi = nullptr;
     if (latchExit.empty()) { // TODO fix LLVM
       exitMaskPhi = PHINode::Create(boolTy, 1, exitBlock.getName().str() + ".lcssa", &latchExit);
     } else {
       exitMaskPhi = PHINode::Create(boolTy, 1, exitBlock.getName().str() + ".lcssa", &*latchExit.begin());
     }
     vecInfo.setVectorShape(*exitMaskPhi, VectorShape::varying());

     // last exit update in the outermost left loop
     Loop & leftLoop = *loopInfo.getLoopFor(&exitingBlock);
     LoopTracker & leftLoopTracker = getLoopTracker(leftLoop);
     LoopTracker::ValueUpdate & exitDesc = leftLoopTracker.getExitUpdate(exitBlock);

     // temporarliy use the exit mask tracker (will be replaced by actual update in fixLatchUpdates)
     exitMaskPhi->addIncoming(exitDesc.valueTracker, &exitBlock);

     // set the right exit mask for this block
     maskEx.setBlockMask(exitBlock, *exitMaskPhi);
     vecInfo.setPredicate(exitBlock, *exitMaskPhi); // FIXME cleanup

     // re-connect the exit to the CFG
     BranchInst * exitBr = nullptr;
     if (&lastExit != &exitBlock) {
       // this advances the ir builder to the next block
       exitBr = & CreateIf(builder, *exitMaskPhi, exitBlock);
       auto & falseBlock = *builder.GetInsertBlock();
       // repair loopInfo
       if (loop.getParentLoop()) {
         loop.getParentLoop()->addBasicBlockToLoop(&falseBlock, loopInfo);
       }

     } else {
       exitBr = builder.CreateBr(&exitBlock);
       branchShape = VectorShape::uni(); // single successor
     }
     IF_DEBUG_DLT { errs() << "- restored exit branch: " << *exitBr << "\n"; }

     // set an appropriate mask for this exit
     vecInfo.setVectorShape(*exitBr, branchShape);

     // repair LCSSA
     for (auto it = exitBlock.begin(); isa<PHINode>(it);) {
       PHINode & lcPhi = cast<PHINode>(*it++);
       auto * liveOut = lcPhi.getIncomingValue(0);
       auto * liveOutInst = dyn_cast<Instruction>(liveOut);

       // try to unify LCSSA Phis
#if 0
#warning "LCSSA unification disabled"
#else
       if (liveOutInst) {
         auto itKnown = knownLiveOuts.find(liveOutInst);
         if (itKnown != knownLiveOuts.end()) {
           auto &existingPhi = *itKnown->second;
           lcPhi.replaceAllUsesWith(&existingPhi);
           lcPhi.eraseFromParent();
           continue;
         }
       }
#endif

       // Otw, hoist this LCSSA phi
       lcPhi.removeFromParent();
       lcPhi.setIncomingBlock(0, loopTracker.pureLatch);
       InsertAtFront(latchExit, lcPhi);
       knownLiveOuts[liveOutInst] = &lcPhi;
     }
   }

   IF_DEBUG_DLT {
     errs() << "\n- Region with exit branches -\n";
     vecInfo.dump();
   }

   // finally set the loop header mask to terminate maskEx
   vecInfo.setPredicate(*loop.getHeader(), loopTracker.loopMaskPhi);
   maskEx.setBlockMask(*loop.getHeader(), loopTracker.loopMaskPhi); // FIXME
}


PHINode &
LoopTracker::requestExitUpdate(BasicBlock & exit, PHINode & exitTracker) {
  auto it = latchExitUpdates.find(&exit);
  if (it != latchExitUpdates.end()) return cast<PHINode>(*it->second.latchUpdate);

  IF_DEBUG_DLT { errs() << "# reqExitLatchPhi " << loop.getName() << ", exit " << exit.getName() << "\n"; }

  std::string exitUpdName = exit.getName().str() + ".xmask.upd";
  auto & exitUpdatePhi = *PHINode::Create(Type::getInt1Ty(exit.getContext()), 2, exitUpdName, &*pureLatch->begin()); // TODO set vector shape
  latchExitUpdates[&exit] = ValueUpdate{&exitTracker, &exitUpdatePhi};

  // re use the value tracking infrastructure here
  latchValueUpdates[&exitTracker] = LoopTracker::ValueUpdate{&exitTracker, &exitUpdatePhi};

  exitUpdatePhi.addIncoming(&exitTracker, oldLatch);
  return exitUpdatePhi;
}

LoopTracker::ValueUpdate&
LoopTracker::getExitUpdate(BasicBlock & exit) {
  return latchExitUpdates[&exit];
}

LoopTracker &
DivLoopTrans::getLoopTracker(Loop & loop) {
  auto it = loopTrackers.find(&loop);
  assert (it != loopTrackers.end());
  return *it->second;
}

LoopTracker &
DivLoopTrans::requestLoopTracker(Loop & loop) {
  auto it = loopTrackers.find(&loop);
  if (it != loopTrackers.end()) return *it->second;

  // Create a tracker phi in the loop header
  std::string maskPhiName = loop.getName().str();
  auto & loopMaskPhi = *PHINode::Create(boolTy, 2, maskPhiName + ".live", &*loop.getHeader()->begin());
  vecInfo.setVectorShape(loopMaskPhi, VectorShape::varying()); // TODO use control shape

  auto * tracker = new LoopTracker(loop, loopMaskPhi);

  loopTrackers[&loop] = tracker;
  return *tracker;
}

// split the latch if it is not pure (only a terminator)
BasicBlock &
DivLoopTrans::requestPureLatch(LoopTracker & loopTracker) {
  // we cache the pure latch since we make it impure again along the way
  if (loopTracker.pureLatch) return *loopTracker.pureLatch;

  IF_DEBUG_DLT { errs() << "# reqPureLatch " << loopTracker.loop.getName() << "\n"; }

  auto & loop = loopTracker.loop;
  std::string loopName = loop.getName().str();
  auto & latchBlock = *loop.getLoopLatch();
  auto & header = *loop.getHeader();

  // create a pure latch block
  auto & pureLatchBlock = *BasicBlock::Create(latchBlock.getContext(), latchBlock.getName() + ".pure", latchBlock.getParent(), &latchBlock);

  // create a mask update in the latch
  auto & latchPhi = *PHINode::Create(boolTy, 2, loopName + ".live.upd", &pureLatchBlock);
  loopTracker.maskUpdatePhi = &latchPhi;
  latchPhi.addIncoming(&loopTracker.loopMaskPhi, &latchBlock);
  vecInfo.setVectorShape(latchPhi, VectorShape::varying()); // TODO use control divergence shape

  // latchPhi updates the loop mask depending on the predecessors
  // the pure latch is the dedicated predecessor of the loop header so we can safely use the live.upd mask here
  loopTracker.loopMaskPhi.addIncoming(UndefValue::get(Type::getInt1Ty(header.getContext())), loop.getLoopPreheader());
  loopTracker.loopMaskPhi.addIncoming(&latchPhi, &pureLatchBlock);

  // insert on the latche edge
  auto & latchBr = *cast<BranchInst>(latchBlock.getTerminator());
  if (latchBr.isConditional()) {
    bool latchOnFalse = latchBr.getSuccessor(1) == &header;
    latchBr.setSuccessor(latchOnFalse, &pureLatchBlock);
  } else {
    assert(latchBr.getSuccessor(0) == &header);
    latchBr.setSuccessor(0, &pureLatchBlock);
  }
  auto & pureLatchBr = *BranchInst::Create(&header, &pureLatchBlock);
  vecInfo.setVectorShape(pureLatchBr, VectorShape::uni());

  // broken edge to loop header -> fix incoming blocks
#if 0
  // TODO remove should be redundant with latch control transform
  for (auto & inst : header) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) break;
    int latchIdx = phi->getBasicBlockIndex(&latchBlock);
    if (latchIdx < 0) continue;
    phi->setIncomingBlock(latchIdx, &pureLatchBlock);
  }
#endif

  // register with LoopInfo & LoopTracker
  loop.addBasicBlockToLoop(&pureLatchBlock, loopInfo);
  loopTracker.pureLatch = &pureLatchBlock;
  loopTracker.oldLatch = &latchBlock;

  // fix header phi incoming blocks (the pure latch breaks this edge)
  for (auto & inst : header) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) break;
    int latchIdx = phi->getBasicBlockIndex(loopTracker.oldLatch);
    if (latchIdx >= 0) {
      phi->setIncomingBlock(latchIdx, &pureLatchBlock);
    }
  }

  assert((loop.getLoopLatch() == &pureLatchBlock) && "latch replacement didn't work");
  return pureLatchBlock;
}

void
DivLoopTrans::addLoopInitMasks(llvm::Loop & loop) {
  for (auto * childLoop : loop) {
    addLoopInitMasks(*childLoop);
  }

  auto it = loopTrackers.find(&loop);
  if (it == loopTrackers.end()) {
    return;
  }

  auto * loopTracker = it->second;

  // this loop has a live mask -> request its live in value
  auto & loopPreHead = *loop.getLoopPreheader();
  auto & preHeadTerm = *loopPreHead.getTerminator();
  auto & loopHead = *loop.getHeader();
  IndexSet headerIndices;
  maskEx.getPredecessorEdges(preHeadTerm, loopHead, headerIndices);
  auto & initMask = maskEx.requestJoinedEdgeMask(preHeadTerm, headerIndices);

  // attach the missing preheader input to the live mask
  int initIdx = loopTracker->loopMaskPhi.getBasicBlockIndex(&loopPreHead);
  assert(initIdx >= 0 && "loop pre-header has changed!");

  loopTracker->loopMaskPhi.setIncomingValue(initIdx, &initMask);

  auto * divExit = loop.getExitBlock();
  assert(divExit);
  maskEx.setBlockMask(*divExit, initMask);
  vecInfo.setPredicate(*divExit, initMask); // FIXME cleanup
}


void
DivLoopTrans::addDefaultInputsToLatchUpdates() {
  for (auto it : loopTrackers) {
    auto & loopTracker = *it.second;
    if (!loopTracker.pureLatch) continue;

    auto & pureLatch = *loopTracker.pureLatch;

    for (auto & inst : pureLatch) {
      if (!isa<PHINode>(inst)) continue;
      auto & phi = cast<PHINode>(inst);

      auto & defValue = *phi.getIncomingValueForBlock(loopTracker.oldLatch);

      for (auto * predBlock : predecessors(&pureLatch)) {
        int predIdx =  phi.getBasicBlockIndex(predBlock);
        if (predIdx >= 0) continue;
        phi.addIncoming(&defValue, predBlock); // Otw, let this path default
      }

      IF_DEBUG_DLT {
        for (size_t i = 0; i < phi.getNumIncomingValues(); ++i) {
          auto * inBlock = phi.getIncomingBlock(i);
          bool found = std::any_of(pred_begin(&pureLatch), pred_end(&pureLatch), [=](BasicBlock * predBlock){ return inBlock == predBlock; } );
          assert(found);
        }
      }
    }
  }
}



DivLoopTrans::DivLoopTrans(PlatformInfo & _platInfo, VectorizationInfo & _vecInfo, MaskExpander & _maskEx, llvm::DominatorTree & _domTree, llvm::LoopInfo & _loopInfo)
: platInfo(_platInfo)
, vecInfo(_vecInfo)
, maskEx(_maskEx)
, domTree(_domTree)
, loopInfo(_loopInfo)
, boolTy(Type::getInt1Ty(vecInfo.getContext()))
, numDivergentLoops(0)
, numKillExits(0)
{}


DivLoopTrans::~DivLoopTrans() {
  for (auto it : loopTrackers) {
    delete it.second;
  }
}

bool
DivLoopTrans::transformDivergentLoopControl(Loop & loop, LiveValueTracker & liveOutTracker) {
  // transform control for this divergent loop
  bool hasDivergentLoops = false;
  if (vecInfo.isDivergentLoop(&loop)) {
    ++numDivergentLoops;
    hasDivergentLoops |= true;
    convertToLatchExitLoop(loop, liveOutTracker);
  }

  for (auto * childLoop : loop) {
    hasDivergentLoops |= transformDivergentLoopControl(*childLoop, liveOutTracker);
  }
  return hasDivergentLoops;
}

void
DivLoopTrans::transformDivergentLoops() {
  IF_DEBUG_DLT { errs() << "-- divLoopTrans log --\n"; }

  LiveValueTracker liveOutTracker(vecInfo, domTree, loopInfo);

  // create tracker/update phis and make all loops uniform
  bool hasDivergentLoops = false;
  IF_DEBUG_DLT { errs() << "# 1. transforming control in divergent loops:\n"; }
  for (auto * loop : loopInfo) {
    hasDivergentLoops |= transformDivergentLoopControl(*loop, liveOutTracker);
  }
  if (!hasDivergentLoops) {
     IF_DEBUG_DLT { errs() << "-- no divergent loops. EOF divLoopTrans --\n"; }
     return;
  }
  IF_DEBUG_DLT { errs() << "# vecInfo after control transform:\n"; vecInfo.dump(); }

  // checkpoint:
  domTree.recalculate(vecInfo.getScalarFunction());
  IF_DEBUG_DLT {
    vecInfo.getScalarFunction().dump();
    loopInfo.print(errs());
    loopInfo.verify(domTree); // must not recompute
  }

  // repair value flow in tracker/latch updates and live out uses
  IF_DEBUG_DLT { errs() << "# 2. materialize value tracker updates:\n"; }
  for (auto * loop : loopInfo) {
    fixDivergentLoopUpdates(*loop, liveOutTracker);
  }
  IF_DEBUG_DLT { errs() << "# vecInfo after value tracker materialization:\n"; vecInfo.dump(); }

  // repair live out uses
  IF_DEBUG_DLT { errs() << "# 3. repairing live out value flow:\n"; }
  for (auto * loop : loopInfo) {
    fixDivergentLiveOutUses(*loop, liveOutTracker);
  }
  IF_DEBUG_DLT { errs() << "# vecInfo after liveout repair:\n"; vecInfo.dump(); }

  // request initial loop live masks
  IF_DEBUG_DLT { errs() << "# 4. requesting initial loop live masks:\n"; }
  for (auto * loop : loopInfo) {
    addLoopInitMasks(*loop);
  }

  // TODO we must not do this or we cannot use SSAUpdater
  // add missing default input to latch updates
  // IF_DEBUG_DLT { errs() << "# 4. adding default inputs to latch update phis:\n"; }
  // addDefaultInputsToLatchUpdates();

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
}


} // namespace rv

