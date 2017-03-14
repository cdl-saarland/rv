//===- Linearizer.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon
//

#include "rv/transform/Linearizer.h"

#include "rv/region/Region.h"
#include "rv/vectorizationInfo.h"

#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/ADT/PostOrderIterator.h>

#include <cassert>
#include <climits>
#include <set>

#include "rvConfig.h"

#if 1
#define IF_DEBUG_LIN IF_DEBUG
#else
#define IF_DEBUG_LIN if (false)
#endif

#if 0
#define IF_DEBUG_DTFIX IF_DEBUG
#else
#define IF_DEBUG_DTFIX if (false)
#endif

namespace rv {

void
Linearizer::addToBlockIndex(BasicBlock & block) {
  assert(relays.size() < INT_MAX);
  int id = relays.size();
  blockIndex[&block] = id;
  relays.push_back(RelayNode(block, id));
}

using namespace rv;
using namespace llvm;

void
Linearizer::buildBlockIndex() {
  relays.reserve(func.getBasicBlockList().size());

  // FIXME this will diverge for non-canonical (LoopInfo) loops
  std::vector<BasicBlock*> stack;
  std::set<Loop*> pushedLoops;

  for (auto & block : func) {
    // seek unprocessed blocks
    if (!inRegion(block)) continue; // FIXME we need a Region::blocks-in-the-region iterator
    if (blockIndex.count(&block)) continue; // already indexed this block
    stack.push_back(&block);

    // process blocks
    while (!stack.empty()) {
      BasicBlock * block = stack.back();
      if (blockIndex.count(block)) {
        stack.pop_back();
        continue; // already indexed this block
      }

      auto * loop = li.getLoopFor(block);

      // we are seeing this loop for the first time
      // drop this block
      // push first the latch and than all predecessors of the header on top
      if (loop && pushedLoops.insert(loop).second) {
        stack.pop_back(); // forget how we entered this loop

        auto & latch = *loop->getLoopLatch();
        stack.push_back(&latch);

        // push all header predecessors on top of the latch
        for (auto * pred : predecessors(loop->getHeader())) {
          if (!inRegion(*pred)) continue;

          // do not descend into the latch
          if (loop->contains(pred)) continue;

          // Otw, check if dependencies are satifised
          if (!blockIndex.count(pred)) {
            stack.push_back(pred);
          }
        }

        // start processing the loop
        continue;
      }

      // filter out all dependences to loop-carried blocks if we are looking at the loop header
      Loop * filterLoop = nullptr;
      if (loop && loop->getHeader() == block) {
        filterLoop = loop;
      }

      bool allDone = true;

      for (auto * pred : predecessors(block)) {
        if (!inRegion(*pred)) continue;

        // do not descend into the latch
        if (filterLoop && filterLoop->contains(pred)) continue;

        // Otw, check if dependencies are satifised
        if (!blockIndex.count(pred)) {
          stack.push_back(pred);
          allDone = false;
        }
      }

      // all dependences satisfied -> assign topo index
      if (allDone) {
        // assign an id
        stack.pop_back();
        assert(!blockIndex.count(block));
        addToBlockIndex(*block);

        // if we are re-vising the loop header all dependences outside of the loop have been scheduled
        // now its time to schedule the remainder of the loop before any other outside block
        if (filterLoop) {
          auto * loopLatch = filterLoop->getLoopLatch();
          assert(loopLatch && "loop does not have a latch");
          if (!blockIndex.count(loopLatch)) {
            stack.push_back(loopLatch);
          }
        }
      }
    }
  }

}

Value&
Linearizer::promoteDefToBlock(BasicBlock & block, SmallVector<Value*, 16> & defs, Value & defaultDef, int defBlockId, int blockId, VectorShape instShape) {
  Value * localDef = nullptr;
  PHINode * localPhi = nullptr;

  auto * type = defaultDef.getType();

  auto itBegin = pred_begin(&block), itEnd = pred_end(&block);
  for (auto it = itBegin; it != itEnd; ++it) {
    auto * predBlock = *it;

    Value * inVal = nullptr;

    if (!hasIndex(*predBlock)) {
      // TODO ad hoc stepping through blendBlocks
      inVal = &promoteDefToBlock(*predBlock, defs, defaultDef, defBlockId, blockId, instShape);

    } else {
      int predIndex = getIndex(*predBlock);

      // turn incoming value into an explicit value (nullptr -> Undef)
      if (predIndex < defBlockId) {
        // predecessor not in span -> undef
        inVal = &defaultDef;

      } else if (predIndex >= blockId) {
        continue; // reaching backedge -> ignore

      } else {
        // predecessor in span with def
        int reachingDefId = predIndex - defBlockId;
        auto * reachingDef = defs[reachingDefId];
        if (!reachingDef) {
          // reaching undef within block range
          inVal = &defaultDef;
        } else {
          inVal = reachingDef;
        }
      }
    }

    // first reaching def OR reaching def is the same
    if (!localDef || localDef == inVal) {
      localDef = inVal;
      continue;
    }

    // Otw, we need a phi node
    if (!localPhi) {
      localPhi = PHINode::Create(type, 0, "", &*block.getFirstInsertionPt());
      vecInfo.setVectorShape(*localPhi, instShape);
      for (auto itPassedPred = itBegin; itPassedPred != it; ++itPassedPred) {
        localPhi->addIncoming(localDef, *itPassedPred);
      }
      IF_DEBUG_LIN { errs() << "\t | partial def PHI @ " << blockId << ", " << block.getName() << " : " << *localPhi << "\n"; }
      localDef = localPhi;
    }

    // attach the incoming value
    localPhi->addIncoming(inVal, predBlock);
  }

  // register as final definition at this point
  IF_DEBUG_LIN { errs() << "\t- localDef @ " << (blockId) << " " << *localDef << "\n"; }

  return *localDef;
}

Value &
Linearizer::promoteDefinitionExt(SmallVector<Value*, 16> & defs, Value & inst, Value & defaultDef, int defBlockId, int destBlockId) {

  assert(defBlockId <= destBlockId);

  if (defBlockId == destBlockId) return inst;

  const int span = destBlockId - defBlockId;

  IF_DEBUG_LIN { errs() << "\t* promoting value " << inst << " from def block " << defBlockId << " to " << destBlockId << "\n"; }

  assert(defs.size() > span);

  defs[0] = &inst;

  auto instShape = vecInfo.getVectorShape(inst);

  for (int i = 1; i < span + 1; ++i) {
    int blockId = defBlockId + i;

    auto & block = getBlock(blockId);

    defs[i] = &promoteDefToBlock(block, defs, defaultDef, defBlockId, blockId, instShape);
  }

  IF_DEBUG_LIN { errs() << "\tdefs[" << span << "] " << *defs[span] << "\n"; }
  return *defs[span];
}

Value &
Linearizer::promoteDefinition(Value & inst, Value & defaultDef, int defBlockId, int destBlockId) {
  IF_DEBUG_LIN { errs() << "\t* promoting value " << inst << " from def block " << defBlockId << " to " << destBlockId << "\n"; }

  assert(defBlockId <= destBlockId);

  if (defBlockId == destBlockId) return inst;

  const int span = destBlockId - defBlockId;

  SmallVector<Value*, 16> defs(span + 1, nullptr);

  return promoteDefinitionExt(defs, inst, defaultDef, defBlockId, destBlockId);
}

Value &
Linearizer::promoteDefinition(Value & inst, Value & defaultDef, int defBlockId, BasicBlock & userBlock) {
  if (!isa<Instruction>(inst)) return inst;

  if (hasIndex(userBlock)) {
    return promoteDefinition(inst, defaultDef, defBlockId, getIndex(userBlock));

  } else {
    int maxPredId = defBlockId;
    for (auto * predBlock : predecessors(&userBlock)) {
      assert(hasIndex(*predBlock) && "predecessor of blendBlock must be indexed block");
      maxPredId = std::max<>(maxPredId, getIndex(*predBlock));
    }

    const int span = maxPredId - defBlockId;
    SmallVector<Value*, 16> defs(span + 1, nullptr);

    // short cut: predecessor is def block
    if (span == 0) {
      return inst;
    }

    IF_DEBUG_LIN { errs() << "\t* promoting value " << inst << " from def block " << defBlockId << " to block " << userBlock.getName() << ", range " << span << " max pred " << maxPredId << "\n"; }

  // promote the definition to all predecessors of @userBlock
    promoteDefinitionExt(defs, inst, defaultDef, defBlockId, maxPredId);

  // materialize a definition in @userBlock
    return promoteDefToBlock(userBlock, defs, defaultDef, defBlockId, maxPredId + 1, vecInfo.getVectorShape(inst));
  }
}

void
Linearizer::verifyLoopIndex(Loop & loop) {
  for (auto * childLoop : loop) {
    verifyLoopIndex(*childLoop);
  }

  int startId = getNumBlocks(), endId = 0;

  for (auto * block : loop.blocks()) {
    startId = std::min<>(getIndex(*block), startId);
    endId = std::max<>(getIndex(*block), endId);
  }

  IF_DEBUG_LIN {
    errs() << "Loop index range of " << loop.getHeader()->getName() << " from "  << startId << " to " << endId << "\n";
  }

  // there are no blocks in the range that are not part of the loop
  for (int i = startId; i <= endId; ++i) {
    assert(loop.contains(&getBlock(i)) && "non-loop block in topo range of loop");
  }

  // the header has @startId, the latch as @endId
  assert(startId == getIndex(*loop.getHeader()));
  assert(endId == getIndex(*loop.getLoopLatch()));
}

void
Linearizer::verifyBlockIndex() {
  for (auto * loop : li) {
    verifyLoopIndex(*loop);
  }
}

bool
Linearizer::needsFolding(TerminatorInst & termInst) {
  assert(!isa<SwitchInst>(termInst) && "switches unsupported at the moment");

  if (isa<ReturnInst>(termInst) || isa<UnreachableInst>(termInst)) return false;

// Only conditional branches are subject to divergence
  auto & branch = cast<BranchInst>(termInst);
  if (!branch.isConditional()) return false;

// the branch condition is immediately divergent
  if (!vecInfo.getVectorShape(branch).isUniform()) return true;

  return false;
}

Function *
Linearizer::requestReductionFunc(llvm::Module & mod, const std::string & name) {
  auto * redFunc = mod.getFunction(name);
  if (redFunc) return redFunc;
  auto & context = mod.getContext();
  auto * boolTy = Type::getInt1Ty(context);
  auto * funcTy = FunctionType::get(boolTy, boolTy, false);
  redFunc = Function::Create(funcTy, GlobalValue::ExternalLinkage, name, &mod);
  redFunc->setDoesNotAccessMemory();
  redFunc->setDoesNotThrow();
  redFunc->setConvergent();
  redFunc->setDoesNotRecurse();
  return redFunc; // TODO add SIMD mapping
}

Instruction &
Linearizer::createReduction(Value & pred, const std::string & name, BasicBlock & atEnd) {
  auto * redFunc = requestReductionFunc(*atEnd.getParent()->getParent(), name);
  auto * call = CallInst::Create(redFunc, &pred, "reduce", &atEnd);
  vecInfo.setVectorShape(*call, VectorShape::uni());
  return *call;
}

void
Linearizer::dropLoopExit(BasicBlock & block, Loop & loop) {
  auto & term = *block.getTerminator();
  assert(loop.contains(&block) && "can not drop loop exit edge from block that is not in loop");
  assert(term.getNumSuccessors() > 1 && "these must be an edge to drop here");

// find a successor within this loop
  BasicBlock * uniqueLoopSucc = nullptr;
  for (uint i = 0; i < term.getNumSuccessors(); ++i) {
    auto * succ = term.getSuccessor(i);
    if (!uniqueLoopSucc && loop.contains(succ)) {
      uniqueLoopSucc = succ;
      break;
    }
  }

  assert(uniqueLoopSucc && "could not find successor within loop");
// send all loop exiting edges to that successor inside the loop
  // replace this node with a single successor node
  auto * loopBranch = BranchInst::Create(uniqueLoopSucc, &term);
  term.eraseFromParent();
  vecInfo.dropVectorShape(term);
  vecInfo.setVectorShape(*loopBranch, VectorShape::uni());
}

static void
InsertAtFront(BasicBlock & block, Instruction & inst) {
  block.getInstList().insert(block.begin(), &inst);
}

class LiveValueTracker {
  Linearizer & lin;
  VectorizationInfo & vecInfo;
  MaskAnalysis & ma;
  LoopInfo & li;
  Loop & loop;
  BasicBlock & preHeader;

  // maps loop live-out values to their tracking PHI nodes
  // the phi node @second keeps track of the computed value of @first when each thread left the loop
  DenseMap<Instruction*, PHINode*> liveOutPhis;

  // return the incoming index of the exitblock
  int getLoopBlockIndex(PHINode & lcPhi) {
    for (uint i = 0; i < lcPhi.getNumIncomingValues(); ++i) {
      if (loop.contains(lcPhi.getIncomingBlock(i))) return i;
    }
    return -1;
  }

  // return the successor index that leaves the loop
  int getLoopExitIndex(Instruction & inst) {
    auto & branch = cast<BranchInst>(inst);
    if (loop.contains(branch.getSuccessor(0))) return 1;
    else if (loop.contains(branch.getSuccessor(1))) return 0;
    else abort();
  }

  static int GetPreHeaderTrackerIndex() { return 0; }
  static int GetLatchTrackerIndex() { return 1; }
public:
  LiveValueTracker(Linearizer & _lin, Loop & _loop, BasicBlock & _preHeader)
  : lin(_lin), vecInfo(lin.vecInfo), ma(lin.maskAnalysis), li(lin.li), loop(_loop), preHeader(_preHeader)
  {}

  // inserts a tracker PHI into the loop headers surrounding @defInst
  // returns the tracker update valid at the latch block
  PHINode &
  requestTracker(Instruction & inst, BasicBlock & exiting, Instruction & defInst) {
    auto it = liveOutPhis.find(&inst);
    if (it != liveOutPhis.end()) {
      auto & phi = *it->second;
      return phi;
    }

  // create a PHI chain from @defInst up to this loop
    Loop * defLoop = li.getLoopFor(&exiting);
    auto * trackedLoop = defLoop;
    PHINode * nestedTracker = nullptr;

    PHINode * innerTrackerPhi = nullptr;

    auto * undef = UndefValue::get(defInst.getType());

  // create a tracker PHI for loop crossing the exit edge
    while (
        trackedLoop &&
        trackedLoop->getLoopDepth() >= loop.getLoopDepth()
    ) {
      auto * trackedLoopHeader = trackedLoop->getHeader();
      auto * trackedPreHeader = trackedLoop == &loop ? &preHeader : trackedLoop->getLoopPreheader();

    // create a tracker phi in every surrounding loop of @defInst
      auto * phi = PHINode::Create(defInst.getType(), 2, "track_" + defInst.getName(), &*trackedLoopHeader->getFirstInsertionPt());
      vecInfo.setVectorShape(*phi, VectorShape::varying());

      // remember inner-most tracker Phi
      if (!innerTrackerPhi) innerTrackerPhi = phi;

    // preheader input: tracker state of outer phi
      // attach tracker input to nested tracker PHI
      if (nestedTracker) {
        nestedTracker->setIncomingValue(GetPreHeaderTrackerIndex(), phi);
      }

    // preheader input (undef)
      phi->addIncoming(undef, trackedPreHeader);

    // latch input: self-loop or tracker state from (inner) nestedPhi
      if (nestedTracker) {
         phi->addIncoming(nestedTracker, trackedLoop->getLoopLatch()); // take the nested value on the latch
      } else {
         phi->addIncoming(phi, trackedLoop->getLoopLatch()); // create a self loop
      }
      IF_DEBUG_LIN { errs() << "\t* trackerPHI (w/o liveIn update): " << *phi << "\n"; }

    // next outer loop
      nestedTracker = phi;
      trackedLoop = trackedLoop->getParentLoop();
    }

    IF_DEBUG_LIN { errs() << "\t- outer-most tracker " << *nestedTracker << "\n"; }
    IF_DEBUG_LIN { errs() << "\t- inner-most tracker " << *innerTrackerPhi << "\n"; }

  // attach trackerPHI inputs
    liveOutPhis[&inst] = innerTrackerPhi;
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

  // return the mask predicate of the loop exit
  Value&
  getLoopExitMask(BasicBlock & exiting, Loop & loop) {
    int exitIndex = GetExitIndex(exiting, loop);

    auto & context = exiting.getContext();
    Value * blockMask = ma.getEntryMask(exiting);

    IRBuilder<> builder(exiting.getTerminator());

    auto & branch = *cast<BranchInst>(exiting.getTerminator());

    Value * exitCondition = branch.getCondition();
    auto exitShape = lin.vecInfo.getVectorShape(*exitCondition);
    if (exitIndex != 0) {
      exitCondition = builder.CreateXor(branch.getCondition(), ConstantInt::get(Type::getInt1Ty(context), -1));
      vecInfo.setVectorShape(*exitCondition, exitShape);
    }

    auto * exitingMask = builder.CreateAnd(exitCondition, blockMask);
    vecInfo.setVectorShape(*exitingMask, exitShape);
    return *exitingMask;
  }

  // updates @tracker in block @src with @val, if the exit predicate is true
  // this inserts a select instruction in the latch that blends in @val into @tracker if the exit is taken
  // FIXME this will only work if the exit predicate and the live-out instruction dominate the latchBlock
  void
  addTrackerUpdate(PHINode & tracker, BasicBlock & exiting, BasicBlock & exit, Instruction & val) {
  // sanitize: the exit edge leaves from inside the current @loop to a block outside of the loop
    assert(loop.contains(&exiting));
    assert(!loop.contains(&exit));

  // last tracker state
    auto * lastTrackerState = tracker.getIncomingValue(GetLatchTrackerIndex());

  // get exit predicate
    auto & exitMask = getLoopExitMask(exiting, loop); // should do the trick if this atually was the edge predicate..
    // auto & exitMask = *lin.getLoopExitMask(exiting, exit);

    IF_DEBUG_LIN { errs() << "\t-- loop exit mask " << exitMask << "\n"; }
  // materialize the update
    IRBuilder<> builder(&exiting, exiting.getTerminator()->getIterator()); // exit mask needs to be defined in @exiting
    int lastDefIndex = lin.getIndex(exiting);
    auto * updateInst = cast<Instruction>(builder.CreateSelect(&exitMask, &val, lastTrackerState, "update_" + val.getName()));
    vecInfo.setVectorShape(*updateInst, VectorShape::varying());

  // promote the partial def to all surrounding loops
    Value * currentLiveInDef = &tracker;
    Instruction * currentPartialDef = updateInst;
    Loop * currentLoop = li.getLoopFor(tracker.getParent());

    IF_DEBUG_LIN { errs() << "\ttracker promotion " << *updateInst << " for exit " << exiting.getName() << " to " << exit.getName() << "\n"; }
    while (isa<PHINode>(currentLiveInDef)) {
      auto & currPhi = *cast<PHINode>(currentLiveInDef);
      IF_DEBUG_LIN { errs() << "\t- partial def: " << currentPartialDef->getName() << " to latch of tracker PHI " << currPhi.getName() << "\n"; }

      assert(currentLoop == li.getLoopFor(currPhi.getParent()) && "curr header PHI and curr loop out of sync");
      int currLatchIndex = lin.getIndex(*currentLoop->getLoopLatch());

      Instruction * promotedUpdate = nullptr;

      // we need to promote the live out tracker to its user outside of thsi loop
      // However we have two definitions for this value: the tracker PHI and its update operation
      // Hence, we need to repair SSA form on the way down to the user
      auto * currLoopHeader = currPhi.getParent();
      auto * innerLatchBlock = &lin.getBlock(lastDefIndex);

      if (currLoopHeader != innerLatchBlock) {
        // we need a dominating definition for the latch of THIS loop
        auto & repairPhi = lin.createRepairPhi(val, *currentLoop->getLoopLatch());

        // if the latch of the NESTED loop was executed we should see the tracker update in THIS loop
        repairPhi.addIncoming(currentPartialDef, &lin.getBlock(lastDefIndex)); // we add this first to signal that this is the prefered definition
        // if the latch of the NESTED loop was not executed, we should see the same old tracker state
        repairPhi.addIncoming(&currPhi, currPhi.getParent()); // add this last to signal that this is the fallback definition

        promotedUpdate = &repairPhi;
      } else {
        promotedUpdate = currentPartialDef;
      }

      IF_DEBUG_LIN { errs() << "\tsetting update of PHI " << currPhi << " to promoted def " << promotedUpdate << "\n"; }
      currPhi.setIncomingValue(GetLatchTrackerIndex(), promotedUpdate);

    // advance to next surrounding loop
      currentLiveInDef = currPhi.getIncomingValue(GetPreHeaderTrackerIndex());
      currentPartialDef = promotedUpdate;
      currentLoop = currentLoop->getParentLoop();
      lastDefIndex = currLatchIndex; // skip over to the eventual unique loop exit
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

  // get the last tracker state for this live out value (which must be a loop carried instruction)
  Value & getTrackerStateForLiveOut(Instruction & liveOutInst) {
    auto it = liveOutPhis.find(&liveOutInst);
    assert(it != liveOutPhis.end() && "not a tracked value!");
    auto &tracker = *it->second;
    return getLastTrackerState(tracker);
  }

  BasicBlock & getExitingBlock(BasicBlock & exitBlock) {
    for (auto * pred : predecessors(&exitBlock)) {
      if (loop.contains(pred)) return *pred;
    }
    abort();
  }

  // adds all live out values on loop-exits to @exitBlock
  // FIXME this currently assumes that all out-of-loop uses pass through LCSSA Phis. However, uses by all out-of-loop instructions are set to use the tracker value instead.
  void
  trackLiveOuts(BasicBlock & exitBlock) {
    auto & exitingBlock = getExitingBlock(exitBlock);

  // if this branch always finishes the loop off
#if 1
    // bool finalExit = false;
    if (!vecInfo.isMandatory(&exitBlock)) {
      // finalExit = true;
      // this exit kills the loop so we do not need to track any values for it
      IF_DEBUG_LIN errs() << "kill exit " << exitBlock.getName() << " skipping..\n";
      return;
    }
#endif

    assert(!loop.contains(&exitBlock));
    auto itBegin = exitBlock.begin(), itEnd = exitBlock.end();
    for (auto it = itBegin; isa<PHINode>(*it) && it != itEnd; ++it) {
      auto & lcPhi = cast<PHINode>(*it);
      if (lin.isRepairPhi(lcPhi)) continue; // not a PHI node of the original program
      assert(lcPhi.getNumIncomingValues() == 1 && "neither a late repair PHI nor a LCSSA PHI");

    // do not track non-live carried values
      int loopIncomingId = getLoopBlockIndex(lcPhi);
      assert(loopIncomingId >= 0 && "not an LCSSA node");
      assert(&exitingBlock == lcPhi.getIncomingBlock(loopIncomingId));

      auto * inInst = dyn_cast<Instruction>(lcPhi.getIncomingValue(loopIncomingId));
      if (!inInst || !loop.contains(inInst->getParent())) continue; // live out value not loop carried

    // fold the data flow through from exiting->exit through all crossing loops
      auto & tracker = requestTracker(lcPhi, exitingBlock, *inInst);
      // update the tracker with @inInst whenever the exit edge is taken
      addTrackerUpdate(tracker, exitingBlock, exitBlock, *inInst);

  // replace outside uses with tracker
      // if this exit branch kills the loop
      auto & liveOut = getTrackerStateForLiveOut(lcPhi);
      lcPhi.setIncomingValue(loopIncomingId, &liveOut);
    }
  }
};

static
BasicBlock &
GetExitingBlock(Loop & loop, BasicBlock & exitBlock) {
  for (auto * pred : predecessors(&exitBlock)) {
    if (loop.contains(pred)) return *pred;
  }
  abort();
}

Linearizer::RelayNode &
Linearizer::convertToSingleExitLoop(Loop & loop, RelayNode * exitRelay) {
  // TODO rename convertToLatchExitLoop

// look-aheader for the prehader (TODO this is a hack)
  auto & relay = *getRelay(getIndex(*loop.getHeader()));
  auto & preHeader = **pred_begin(relay.block);

// replaces live-out values by explicit tracker PHIs and updates
  LiveValueTracker liveOutTracker(*this, loop, preHeader);

// query the live mask on the latch
  auto & latch = *loop.getLoopLatch();
  auto latchIndex = getIndex(latch);
  assert(latchIndex >= 0);
  auto & header = *loop.getHeader();
  assert(getIndex(header) >= 0);

// create a relay for the single exit block that this loop will have after the conversion
  // while at it create tracker PHIS and updates to them for all live-out values
  SmallVector<BasicBlock*, 3> loopExitBlocks;
  loop.getExitBlocks(loopExitBlocks);

  auto * loopExitRelay = exitRelay;
  for (auto * exitBlock : loopExitBlocks) {
    auto exitId = getIndex(*exitBlock);
    // all exit blocks must be visited after the loop

    loopExitRelay = &addTargetToRelay(loopExitRelay, exitId);
    // track all values that live across this exit edge

    auto & exitingBlock = GetExitingBlock(loop, *exitBlock);
    auto * innerMostExitLoop = li.getLoopFor(&exitingBlock);

    IF_DEBUG_LIN errs() << "\tProcessing loop exit from " << exitingBlock.getName() << " to " << exitBlock->getName() << " of loop with header " << innerMostExitLoop->getHeader()->getName() << "\n";
    // only consider exits of the current loop level
    liveOutTracker.trackLiveOuts(*exitBlock);
  }

// forward loop header reaching to loop exits
  mergeInReaching(*loopExitRelay, relay);

// move LCSSA nodes to exitBlockRelay
  for (auto * block : loopExitBlocks) {

    // skip over the exit we are keeping
    if (block == loopExitRelay->block) {
      continue; // already migrated LCSSA phi to loop exit relay
    }

    // check if we need to repair any LCSSA phi nodes
    // FIXME we should really do this on the final dom tree AFTER the loop body was normalized
    for (auto it = block->begin(); isa<PHINode>(it) && it != block->end(); ) {
      auto * lcPhi = &cast<PHINode>(*it);
      if (!lcPhi) break;
      if (isRepairPhi(*lcPhi)) {
        ++it; // skip this one
        continue;
      }

      // for all exiting edges
      for (uint i = 0; i < lcPhi->getNumIncomingValues(); ++i) {
        assert (loop.contains(lcPhi->getIncomingBlock(i)) && "not an LCSSA Phi node");

        auto * inst = dyn_cast<Instruction>(lcPhi->getIncomingValue(i));
        if (!inst) {
          continue; // no repair necessary as the incoming value is globally available in the function
        }

        BasicBlock * defBlock = inst->getParent();

        // branch will start from the latch
        lcPhi->setIncomingBlock(i, &latch);

        // def dominates exit block and will continue to do so after loop transform
        if (dt.dominates(defBlock, block)) {
          continue;
        }

        // def does not dominate latch
        // create a dominating def by inserting PHI nodes with incoming undefs
        int defIndex = getIndex(*defBlock);
        assert(getIndex(header) <= defIndex && defIndex <= latchIndex && "non-dominating def not in loop");

        auto & dominatingDef = promoteDefinition(*inst, *UndefValue::get(inst->getType()), defIndex, latchIndex);

        // replace incoming value with new dominating def
        lcPhi->setIncomingValue(i, &dominatingDef);
      }

      // migrate this PHI node to the loopExitRelay
      IF_DEBUG_LIN { errs() << "\t\tMigrating " << lcPhi->getName() << " from " << lcPhi->getParent()->getName() << " to " << loopExitRelay->block->getName() << "\n"; }

    // we eliminate LCSSA Phis instead of fixing their predecessor blocks
#if 1
      it++; // skip over this one
      lcPhi->replaceAllUsesWith(lcPhi->getIncomingValue(0));
      lcPhi->eraseFromParent();
#else
      lcPhi->removeFromParent();
      InsertAtFront(*loopExitRelay->block, *lcPhi);
#endif
    }
  }

// drop all loop exiting blocks
  SmallVector<BasicBlock*, 3> loopExitingBlocks;
  loop.getExitingBlocks(loopExitingBlocks);

  for (auto * exitingBlock : loopExitingBlocks) {
    // exits from inner loops will be handled by recursive invocations of processLoop
    // if (li.getLoopFor(exitingBlock) != &loop) continue;

    dropLoopExit(*exitingBlock, loop);
  }

// query exit mask (before dropping the latch which destroys the terminator)
  // Value* liveCond = maskAnalysis.getExitMask(latch, header); // maskAnalysis is invalid!
  Value* liveCond = latchMasks[&loop]; // FIXME currently using cached values

// drop old latch
  auto * latchTerm = latch.getTerminator();
  assert(latchTerm);
  assert(latchTerm->getNumSuccessors() == 1);
  vecInfo.dropVectorShape(*latchTerm);
  latchTerm->eraseFromParent();

// create a new if-all-threads-have-left exit branch cond == rv_any(<loop live mask>)
  auto * anyThreadLiveCond = &createReduction(*liveCond, "rv_any", latch);
  IF_DEBUG_LIN { errs() << "- trip condition " << *anyThreadLiveCond << "\n"; }
  BranchInst* branch = BranchInst::Create(&header, loopExitRelay->block, anyThreadLiveCond, &latch);

// mark loop and its latch exit as non-divergent
  vecInfo.setVectorShape(*branch, VectorShape::uni());
  vecInfo.setLoopDivergence(loop, false);

  return *loopExitRelay;
}

bool
Linearizer::needsFolding(PHINode & phi) {
  // this implementation exploits the fact that edges only disappear completely by relaying
  // e.g. if a edge persists we may assume that it always implies the old predicate

  auto & block = *phi.getParent();

  // this is the case if there are predecessors that are unknown to the PHI
  SmallPtrSet<BasicBlock*, 4> predSet;

  for (auto * inBlock : predecessors(&block)) {
    auto blockId = phi.getBasicBlockIndex(inBlock);
    if (blockId < 0) { return true; }
    predSet.insert(inBlock);
  }

  // or incoming blocks in the PHI node are no longer predecessors
  for (uint i = 0; i < phi.getNumIncomingValues(); ++i) {
    if (!predSet.count(phi.getIncomingBlock(i))) { return true; }
  }

  // Phi should still work
  return false;
}


template<class T>
DomTreeNodeBase<BasicBlock> *
FindIDom(const T & inBlocks, DominatorTree & dt) {
  BasicBlock * commonDomBlock = nullptr;
  for (auto * predBlock : inBlocks) {
    if (!commonDomBlock) { commonDomBlock = predBlock; }
    else { commonDomBlock = dt.findNearestCommonDominator(commonDomBlock, predBlock); }

    IF_DEBUG_DTFIX { errs() << "\t\t\t: dom with " << predBlock->getName() << " is " << commonDomBlock->getName() << "\n"; }

    assert(commonDomBlock && "domtree repair: did not reach a common dom node!");
  }

// domtree update: least common dominator of all incoming branches
  return dt.getNode(commonDomBlock);
}

struct SuperInput {
  SmallVector<BasicBlock*, 4> inBlocks; // original predecessors that reach this remaining predecessor
  BasicBlock * predBlock; // remaining predecessor in linear CFG
  BasicBlock * blendBlock; // block used for select materialization

  SuperInput(SmallVector<BasicBlock*, 4> && _inBlocks, BasicBlock & _predBlock)
  : inBlocks(_inBlocks)
  , predBlock(&_predBlock)
  , blendBlock(nullptr)
  {
    assert(!inBlocks.empty());
  }

  SuperInput()
  : inBlocks()
  , predBlock(nullptr)
  , blendBlock(nullptr)
  {}

  DomTreeNodeBase<BasicBlock>*
  materializeControl(BasicBlock & phiBlock, DominatorTree & dt, LoopInfo & loopInfo, Region * region) {
    if (!blendBlock) return dt.getNode(inBlocks[0]);

  // re-wire old predecessor to blend block
    predBlock->getTerminator()->replaceUsesOfWith(&phiBlock, blendBlock);

  // branch to phi Block
    BranchInst::Create(&phiBlock, blendBlock);

  // add block to region
    if (region) region->add(*blendBlock);

  // update domtree
    DomTreeNodeBase<BasicBlock>* idom = FindIDom<>(predecessors(blendBlock), dt);
    return dt.addNewBlock(blendBlock, idom->getBlock());

  // TODO update loop info
  }
};


/// \brief create a super input value for this phi node
Value *
Linearizer::createSuperInput(PHINode & phi, SuperInput & superInput) {
  auto * falseMask = ConstantInt::getFalse(phi.getContext());

  auto & blocks = superInput.inBlocks;

// make sure we have a dominating definition of the first incoming value available
  auto * defaultValue = phi.getIncomingValueForBlock(blocks[0]);

  // early exit: there is only one predecessor: no phis, no blend blocks -> return that value right away
  if (blocks.size() <= 1) return defaultValue; // FIXME we still need a dominating definition

// we will need blending: create a block for that to take place
  if (!superInput.blendBlock) superInput.blendBlock = BasicBlock::Create(phi.getContext(), "super", phi.getParent()->getParent(), phi.getParent());

  // make sure the default definition is dominating
  // FIXME also do this for the single predecessor case if inVal does not dominate it
  Value * blendedVal = defaultValue;
  if (isa<Instruction>(defaultValue)) {
    auto & defFuture = createRepairPhi(*defaultValue, *superInput.blendBlock);
    defFuture.addIncoming(defaultValue, blocks[0]);
    defFuture.addIncoming(UndefValue::get(defaultValue->getType()), superInput.blendBlock);
    blendedVal = &defFuture;
  }

// Start buildling cascasding selects for all remaining incoming values
  IRBuilder<> builder(superInput.blendBlock);

  auto & phiBlock = *phi.getParent();

  auto phiShape = vecInfo.getVectorShape(phi);
  for (int i = 1; i < blocks.size(); ++i) {
    auto * inBlock = blocks[i];
    auto * inVal = phi.getIncomingValueForBlock(inBlock);

    auto * edgeMask = getEdgeMask(*inBlock, phiBlock);

  // make sure the mask predicate is available at this point
  // TODO use caching
    if (isa<Instruction>(edgeMask)) {
      auto & maskFuture = createRepairPhi(*edgeMask, *superInput.blendBlock);
      maskFuture.addIncoming(edgeMask, inBlock);
      maskFuture.addIncoming(falseMask, superInput.blendBlock);
      edgeMask = &maskFuture;
    }

  // promote the incoming value
    if (isa<Instruction>(inVal)) {
      auto & inValFuture = createRepairPhi(*inVal, *superInput.blendBlock);
      inValFuture.addIncoming(inVal, inBlock);
      inValFuture.addIncoming(UndefValue::get(inVal->getType()), superInput.blendBlock);
      inVal = &inValFuture;
    }

  // don't blend undefs
    if (isa<UndefValue>(inVal)) continue; // no need to blend in undef
    if (isa<UndefValue>(blendedVal)) { blendedVal = inVal; continue; }

    blendedVal = builder.CreateSelect(edgeMask, inVal, blendedVal);
    vecInfo.setVectorShape(*blendedVal, phiShape);
  }

  return blendedVal;
}

void
Linearizer::foldPhis(BasicBlock & block) {
// FIXME first shot implementation (highly optimizeable)


// find first non repair PHI
  BasicBlock::iterator itPhi = block.begin();
  for (
      BasicBlock::iterator it = itPhi;
      it != block.end() && isa<PHINode>(*it) && isRepairPhi(cast<PHINode>(*it));
      ++it)
  {
    itPhi = it;
  }

  // no non-repair PHI found
  if (itPhi == block.end() || !isa<PHINode>(*itPhi)) {
    return;
  }

// no PHis, no folding
  auto & phi = cast<PHINode>(*itPhi);

  // only phi found is a repair phi
  if (isRepairPhi(phi)) return;

// check if PHIs need to be folded at all
  if (!needsFolding(phi)) return;

  // TODO fast path for num preds == 1

  IF_DEBUG_LIN { errs() << "\t- folding PHIs in " << block.getName() << "\n"; }

// identify all incoming values that stay immediate predecessors of this block




// after folding multiple immediate predecessors may carry phi inputs in superposition (the former select blocks all re-route through the remaining predecessors)

  // merges new predecessors to all incoming definitions that are in super position
  std::map<BasicBlock*, SuperInput> selectBlockMap;

  // blocks in this set do not need a blend block
  SmallPtrSet<BasicBlock*, 4> seenPreds;
  SmallPtrSet<BasicBlock*, 4> seenInputs;

  for (auto * predBlock : predecessors(&block)) {
    if (!seenPreds.insert(predBlock).second) continue;

    // assert(preservedInputBlocks.count(predBlock) && "assuming that new preds are a subset of old preds");

    IF_DEBUG_LIN { errs() << "\t   inspecting pred " << predBlock->getName() << "\n"; }

    assert(hasIndex(*predBlock));
    auto & predReachingBlocks = getReachingBlocks(getIndex(*predBlock));

    // all inputs that are incoming on this edge after folding
    SmallVector<BasicBlock*, 4> superposedInBlocks;

    for (int i = 0; i < phi.getNumIncomingValues(); ++i) {
      auto * inBlock = phi.getIncomingBlock(i);

      // this incoming block remains an immediate predecessor so its value can only be live in on that block
      // if (preservedInputBlocks.count(inBlock) && preservedInputBlocks[inBlock] != i) {
      //   continue;
      // }

      // otw, this value needs blending on any dominated input

      if (predBlock == inBlock || predReachingBlocks.count(inBlock)) {
        IF_DEBUG_LIN { errs() <<  "\t      - reaching in block " << inBlock->getName() << "\n";  }
        superposedInBlocks.push_back(inBlock);
        seenInputs.insert(inBlock);
      }
    }

    assert(superposedInBlocks.size() >= 1 && "no dominating def available on this select block?!");
    IF_DEBUG_LIN errs() << "phi: superposed incoming value for new inbound block " << predBlock->getName() << " : " << superposedInBlocks.size() << "\n";
    selectBlockMap[predBlock] = SuperInput(std::move(superposedInBlocks), *predBlock);
  }

  assert((phi.getNumIncomingValues() == seenInputs.size()) && "block reachability not promoted down to phi");
  // TODO can abort here if selectBlockMap.empty()
  assert(!selectBlockMap.empty());

// phi -> select based on getEdgeMask(start, dest)
  auto itStart = block.begin(), itEnd = block.end();
  for (auto it = itStart; it != itEnd; ) {
    auto * phi = dyn_cast<PHINode>(&*it++);
    if (!phi) break;
    if (phi->getNumIncomingValues() == 1) continue; // LCSSA
    if (isRepairPhi(*phi)) continue; // only a placeholder for defered SSA repair

    IRBuilder<> builder(&block, block.getFirstInsertionPt());


    auto & flatPhi = *PHINode::Create(phi->getType(), 6, phi->getName(), phi);
    SmallPtrSet<const BasicBlock*, 4>  seenPreds;
    for (auto * predBlock : predecessors(&block)) {
      if (!seenPreds.insert(predBlock).second) continue;

      auto itSuperInput = selectBlockMap.find(predBlock);

      assert(itSuperInput != selectBlockMap.end());

      // folded iput
      auto * superInVal = createSuperInput(*phi, itSuperInput->second);
      auto & superInput = itSuperInput->second;

      auto * selectBlock = superInput.blendBlock ? superInput.blendBlock : predBlock;
      flatPhi.addIncoming(superInVal, selectBlock);
    }

    Value * replacement = nullptr;
    if (flatPhi.getNumIncomingValues() == 1) {
      replacement = flatPhi.getIncomingValue(0);
      flatPhi.eraseFromParent();
    } else {
      vecInfo.setVectorShape(flatPhi, VectorShape::varying()); // TODO infer from operands
      replacement = &flatPhi;
    }

    phi->replaceAllUsesWith(replacement);
    phi->eraseFromParent();
  }

// embed future blend blocks into control
  for (auto it : selectBlockMap) {
    it.second.materializeControl(block, dt, li, region);
    if (it.second.blendBlock) {
      vecInfo.setVectorShape(*it.second.blendBlock->getTerminator(), VectorShape::uni());
    }
  }

  // update idom
  dt.getNode(&block)->setIDom(FindIDom<>(predecessors(&block), dt));
}

int
Linearizer::processLoop(int headId, Loop * loop) {
  auto & loopHead = getBlock(headId);
  assert(loop && "not actually part of a loop");
  assert(loop->getHeader() == &loopHead && "not actually the header of the loop");

  IF_DEBUG_LIN {
    errs() << "processLoop : header " << loopHead.getName() << " ";
    dumpRelayChain(getIndex(loopHead));
    errs() << "\n";
  }

  auto & latch = *loop->getLoopLatch();
  int latchIndex = getIndex(latch);
  int loopHeadIndex = getIndex(loopHead);

  // inherited relays from the pre-header edge: all targets except loop header
  RelayNode * headRelay = getRelay(headId);

  // assert(headRelay && "could not find relay for loop header");

  if (vecInfo.isDivergentLoop(loop)) {
    RelayNode * exitRelay = nullptr;
    if (headRelay) {
      exitRelay = headRelay -> next;
    }

    // convert loop into a non-divergent form
    convertToSingleExitLoop(*loop, exitRelay);

  } else if (headRelay) {
    // forward header reaching blocks to loop exits
    SmallVector<BasicBlock*, 4> exitBlocks;
    loop->getExitBlocks(exitBlocks);
    for (auto * exitBlock : exitBlocks) {
      mergeInReaching(getRelayUnchecked(getIndex(*exitBlock)), *headRelay);
    }
  }

  // emit all blocks within the loop (except the latch)
  int latchNodeId = processRange(loopHeadIndex, latchIndex, loop);

  // FIXME repair SSA in the loop here, AFTER loop conversion
  // repairLiveOutSSA({(val, defStart)}, destId)

  // now emit the latch (without descending into its successors)
  emitBlock(latchIndex);
  foldPhis(latch);

  // emit loop header again to re-wire the latch to the header
  emitBlock(loopHeadIndex);

  // attach undef inputs for all preheader edges to @loopHead
  addUndefInputs(loopHead);
  IF_DEBUG_LIN { errs() << "-- processLoop finished --\n"; }

  return latchNodeId + 1; // continue after the latch
}

void
Linearizer::addUndefInputs(llvm::BasicBlock & block) {
  auto itBegin = block.begin(), itEnd = block.end();
  for (auto it = itBegin; isa<PHINode>(*it) && it != itEnd; ++it) {
    auto & phi = cast<PHINode>(*it);
    for (auto * predBlock : predecessors(&block)) {
      auto blockId = phi.getBasicBlockIndex(predBlock);
      if (blockId >= 0) continue;

      phi.addIncoming(UndefValue::get(phi.getType()), predBlock);
    }
  }
}


// forwards branches to the relay target of @targetId to the actual @targetId block
// any scheduleHeads pointing to @target will be advanced to the next block on their itinerary
// @return the relay node representing all blocks that have to be executed after this one, if any
Linearizer::RelayNode *
Linearizer::emitBlock(int targetId) {
  auto & target = getBlock(targetId);
  IF_DEBUG_LIN {
    errs() << "\temit : " << target.getName() << "\n";
  }

// advance all relays for @target
  BasicBlock * relayBlock;
  auto * advancedRelay = advanceScheduleHead(targetId, relayBlock);

// if there is no relay for this head we are done
  if (!relayBlock) {
    return nullptr;
  }

// make all predecessors of @relayBlock branch to @target instead
  auto itStart = relayBlock->use_begin(), itEnd = relayBlock->use_end();

  // dom node of emitted target block
  auto * targetDom = dt.getNode(&target);
  assert(targetDom);

  IF_DEBUG_DTFIX errs() << "\t\tDTFIX: searching idom for " << target.getName() << "\n";

  for (auto itUse = itStart; itUse != itEnd; ) {
    Use & use = *(itUse++);

    int i = use.getOperandNo();
    auto & branch = *cast<BranchInst>(use.getUser());
    IF_DEBUG_LIN { errs() << "\t\tlinking " << branch << " opIdx " << i << "\n"; }

    // forward branches from relay to target
    branch.setOperand(i, &target);
    IF_DEBUG_LIN { errs() << "\t\t-> linked " << branch << " opIdx " << i << "\n"; }
  }

// search for a new idom
  auto * nextCommonDom = FindIDom<>(predecessors(&target), dt);

// domtree update: least common dominator of all incoming branches
  IF_DEBUG_DTFIX { errs() << "DT before dom change:";dt.print(errs()); }
  targetDom->setIDom(nextCommonDom);
  IF_DEBUG_DTFIX { errs() << "DT after dom change:";dt.print(errs()); }

// if there are any instructions stuck in @relayBlock move them to target now
  for (auto it = relayBlock->begin(); it != relayBlock->end() && !isa<TerminatorInst>(*it); it = relayBlock->begin()) {
    it->removeFromParent();
    InsertAtFront(target, *it);
  }

// dump remaining uses for debugging purposes
  IF_DEBUG_LIN {
    for (auto & use : relayBlock->uses()) {
      auto * userInst = dyn_cast<Instruction>(use.getUser());
      if (userInst) {
        errs() << "UserInst : " << *use.getUser() << " in block " << *userInst->getParent() << "\n";
        assert(!userInst);
      } else {
        errs() << "USe : " << *use.getUser() << "\n";
      }
    }
  }

  // free up the relayBlock
  relayBlock->eraseFromParent();

  // remaining exits after this block
  return advancedRelay;
}


// process the branch our loop at this block and return the next blockId
int
Linearizer::processBlock(int headId, Loop * parentLoop) {
  // pending blocks at this point
  auto & head = getBlock(headId);

  IF_DEBUG_LIN { errs() << "processBlock "; dumpRelayChain(headId); errs() << "\n"; }

// descend into loop, if any
  auto * loop = li.getLoopFor(&head);
  if (loop != parentLoop) {
    return processLoop(headId, loop);
  }

  // all dependencies satisfied -> emit this block
  auto * advancedExitRelay = emitBlock(headId);

  // convert phis to selectsw
  foldPhis(head);

  // materialize all relays
  processBranch(head, advancedExitRelay, parentLoop);

  return headId + 1;
}

int
Linearizer::processRange(int startId, int endId, Loop * parentLoop) {
  for (auto i = startId; i < endId;) {
    assert(!parentLoop || parentLoop->contains(&getBlock(i)));
    i = processBlock(i, parentLoop);
  }

  return endId;
}

bool
Linearizer::containsOriginalPhis(BasicBlock & block) {
  for (auto & inst : block) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) return false;
    if (!isRepairPhi(*phi)) return true;
  }
  return false;
}

void
Linearizer::mergeInReaching(RelayNode & dest, RelayNode & source) {
  if (&dest == &source) return;
  for (auto * bb: source.reachingBlocks) {
    dest.addReachingBlock(*bb);
  }
}


void
Linearizer::processBranch(BasicBlock & head, RelayNode * exitRelay, Loop * parentLoop) {
  IF_DEBUG_LIN {
    errs() << "  processBranch : " << *head.getTerminator() << " of block " << head.getName() << "\n";
  }

  auto & term = *head.getTerminator();

  if (term.getNumSuccessors() == 0) {
    IF_DEBUG_LIN { errs() << "\t control sink.\n"; }
    return;
  }

  auto * branch = dyn_cast<BranchInst>(&term);

  auto & headRelay = getRelayUnchecked(getIndex(head));

// Unconditional branch case
  if (!branch->isConditional()) {
    auto & nextBlock = *branch->getSuccessor(0);

    auto & relay = addTargetToRelay(exitRelay, getIndex(nextBlock));

    // if the branch target feeds a phi and the edge is relayed -> track reachability
    if (containsOriginalPhis(nextBlock)) {
       relay.addReachingBlock(head);
    }

    IF_DEBUG_LIN {
      errs() << "\tunconditional. merged with " << nextBlock.getName() << " "; dumpRelayChain(relay.id); errs() << "\n";
    }

    mergeInReaching(relay, headRelay);
    branch->setSuccessor(0,  relay.block);
    return;
  }

// whether this branch must be eliminated from the CFG
  assert(branch && "can only fold conditional BranchInsts (for now)");
  bool mustFoldBranch = needsFolding(*branch);

// order successors by global topologic order
  uint firstSuccIdx = 0;
  uint secondSuccIdx = 1;

  if (getIndex(*branch->getSuccessor(firstSuccIdx)) > getIndex(*branch->getSuccessor(secondSuccIdx))) {
    std::swap<>(firstSuccIdx, secondSuccIdx);
  }
  BasicBlock * firstBlock = branch->getSuccessor(firstSuccIdx);
  int firstId = getIndex(*firstBlock);
  BasicBlock * secondBlock = branch->getSuccessor(secondSuccIdx);
  int secondId = getIndex(*secondBlock);
  assert(firstId > 0 && secondId > 0 && "branch leaves the region!");

  IF_DEBUG_LIN {
    if (mustFoldBranch) {  errs() << "\tneeds folding. first is " << firstBlock->getName() << " at " << firstId << " , second is " << secondBlock->getName() << " at " << secondId << "\n"; }
  }

// process the first successor
// if this branch is folded then @secondBlock is a must-have after @firstBlock
  RelayNode * firstRelay = &addTargetToRelay(exitRelay, firstId);

  // the branch to secondBlock is relayed -> remember we came from head
  if (mustFoldBranch && containsOriginalPhis(*secondBlock)) {
    firstRelay->addReachingBlock(head);
  }

  if (mustFoldBranch) {
    firstRelay = &addTargetToRelay(firstRelay, secondId);
    branch->setSuccessor(secondSuccIdx, firstRelay->block);
  }


// relay the first branch to its relay block
  branch->setSuccessor(firstSuccIdx, firstRelay->block);

// whatever reaches head reaches the first successor
  mergeInReaching(*firstRelay, headRelay);

// domtree repair
  // if there is no relay node for B then A will dominate B after the transformation
  // this is because in that case all paths do B have to go through A first
  if (dt.dominates(&head, secondBlock) && !getRelay(secondId)) {
    auto * secondDom = dt.getNode(secondBlock);
    auto * firstDom = dt.getNode(firstBlock);
    assert(firstDom);

    IF_DEBUG_DTFIX { errs() << "DT before dom change:"; dt.print(errs()); }
    IF_DEBUG_DTFIX { errs() << "DTFIX: " << secondBlock->getName() << " idom is " << firstBlock->getName() << " by dominance\n"; }
    secondDom->setIDom(firstDom);
    IF_DEBUG_DTFIX { errs() << "DT after dom change:";dt.print(errs()); }
  }

// process the second successor
  auto & secondRelay = addTargetToRelay(exitRelay, secondId);

  mergeInReaching(secondRelay, headRelay);

  // auto & secondRelay = requestRelay(secondMustHaves);
  if (containsOriginalPhis(*secondBlock)) {
    secondRelay.addReachingBlock(head);
  }

  if (!mustFoldBranch) {
    branch->setSuccessor(secondSuccIdx, secondRelay.block);

  } else {
    secondRelay.addReachingBlock(*firstBlock);
  }

// mark branch as non-divergent
  vecInfo.setVectorShape(*branch, VectorShape::uni());
}

void
Linearizer::run() {
  IF_DEBUG_LIN {
    errs() << "-- LoopInfo --\n";
    li.print(errs());
  }

// initialize with a global topologic enumeration
  buildBlockIndex();

// verify the integrity of the block index
  verifyBlockIndex();

// early exit on trivial cases
  if (getNumBlocks() <= 1) return;

// FIXME currently maskAnslysis is invalidated as a result of linearization.
  // We cache the latch masks locally before touching the function as we need those to make divergent loops uniform
  cacheMasks();

// dump divergent branches / loops
  IF_DEBUG_LIN {
    dt.print(errs());

    errs() << "-- LIN: divergent loops/brances in the region --";
    for (int i = 0; i < getNumBlocks(); ++i) {
      auto & block = getBlock(i);
      auto * loop = li.getLoopFor(&block);

      errs() << "\n" << i << " : " << block.getName() << " , ";

      if (loop && loop->getHeader() == &block) {
        if (vecInfo.isDivergentLoop(loop)) {
           errs() << "div-loop header: " << block.getName();

           auto & latch = *loop->getLoopLatch();
           auto * latchMask = maskAnalysis.getExitMask(latch, block);
           errs() << "\t latch mask " << *latchMask << "\n";

        }
      }
      if (needsFolding(*block.getTerminator())) {
         errs() << "Fold : " << *block.getTerminator();
      }
    }
  }

// fold divergent branches and convert divergent loops to fix point iteration form
  linearizeControl();

// simplify branches
  cleanup();

  dt.recalculate(func);

// repair SSA form on the linearized CFG
  resolveRepairPhis();

// repair SSA (def/use chains that were broken by chain merging)
  fixSSA();

// verify control integrity
  IF_DEBUG_LIN verify();
}

void
Linearizer::linearizeControl() {
  IF_DEBUG_LIN {  errs() << "\n-- LIN: linearization log --\n"; }

  int lastId = processRange(0, getNumBlocks(), nullptr);
  (void) lastId;

  assert(lastId  == getNumBlocks());

  IF_DEBUG_LIN {  errs() << "\n-- LIN: linearization finished --\n"; }
}

PHINode &
Linearizer::createRepairPhi(Value & val, IRBuilder<> & builder) {
  PHINode * repairPhi = builder.CreatePHI(val.getType(), 2, "repairPhi_" + val.getName());

  VectorShape resShape = VectorShape::uni();
  if (vecInfo.hasKnownShape(val)) {
    resShape = vecInfo.getVectorShape(val);
  }

  vecInfo.setVectorShape(*repairPhi, resShape);
  repairPhis.insert(repairPhi);
  return *repairPhi;
}

PHINode &
Linearizer::createRepairPhi(Value & val, BasicBlock & destBlock) {
  if (destBlock.empty()) {
    IRBuilder<> builder(&destBlock);
    return createRepairPhi(val, builder);
  } else {
    IRBuilder<> builder(&destBlock, destBlock.getFirstInsertionPt());
    return createRepairPhi(val, builder);
  }
}

void
Linearizer::resolveRepairPhis() {
  IF_DEBUG_LIN { errs() << "-- resolving repair PHIs --\n"; }

  for (auto * repairPHI : repairPhis) {
    assert(repairPHI->getNumIncomingValues() == 2);
    auto * innerBlock = repairPHI->getIncomingBlock(0);
    auto * innerVal = repairPHI->getIncomingValue(0);
    auto * outerVal = repairPHI->getIncomingValue(1);

    int startIndex = getIndex(*innerBlock);

    auto & userBlock = *repairPHI->getParent();

    IF_DEBUG_LIN { errs() << " repair " << *repairPHI << " on range " << startIndex << " to " << userBlock.getName() << "\n"; }
    auto & promotedDef = promoteDefinition(*innerVal, *outerVal, startIndex, userBlock);
    repairPHI->replaceAllUsesWith(&promotedDef);
    vecInfo.dropVectorShape(*repairPHI);
    repairPHI->eraseFromParent();
  }

  repairPhis.clear();
}

void
Linearizer::verify() {
  IF_DEBUG_LIN { errs() << "\n-- LIN: verify linearization --\n"; func.dump(); }

  for (int i = 0; i < getNumBlocks(); ++i) {
    auto * block = &getBlock(i);
    auto * loop = li.getLoopFor(block);

    if (!loop) {
      assert(!needsFolding(*block->getTerminator()));

    } else if (loop && loop->getHeader() == block) {
      assert(!vecInfo.isDivergentLoop(loop));
    }
  }

  // check whether the on-the-fly domTree repair worked
  dt.verifyDomTree();

  // generic verification passes
  llvm::verifyFunction(func, &errs());
}

void
Linearizer::cacheMasks(){
  for (int i = 0; i < getNumBlocks(); ++i) {
    auto & block = getBlock(i);
    auto * loop = li.getLoopFor(&block);


  // cache loop related masks
    if (loop && loop->getHeader() == &block) {
      if (!vecInfo.isDivergentLoop(loop)) continue;

    // cache latch masks
      auto & latch = *loop->getLoopLatch();
      latchMasks[loop] = maskAnalysis.getExitMask(latch, block);

      SmallVector<BasicBlock*, 6> exitBlocks;
      loop->getExitBlocks(exitBlocks);

    // cache loop exit masks
      for (auto * exitBlock : exitBlocks) {
        auto & exiting = GetExitingBlock(*loop, *exitBlock);

        auto * actualLoopExitMask = maskAnalysis.getActualLoopExitMask(exiting);
        setLoopExitMask(exiting, *exitBlock, actualLoopExitMask);
      }
    }

// cache branch masks
   auto & term = *block.getTerminator();
   for (int i = 0; i < term.getNumSuccessors(); ++i) {
     auto * succBlock = term.getSuccessor(i);
     auto * edgeMask = maskAnalysis.getExitMask(block, *succBlock);
     if (edgeMask) setEdgeMask(block, *succBlock, edgeMask);
   }
  }
}

void
Linearizer::cleanup() {
// simplify terminators
  // linearization can lead to terminators of the form "br i1 cond %blockA %blockA"
  for (auto & block : func) {
    auto * term = block.getTerminator();
    if (!term || term->getNumSuccessors() <= 1) continue; // already as simple as it gets

    bool allSame = true;
    BasicBlock * singleSucc = nullptr;
    for (uint i = 0; i < term->getNumSuccessors(); ++i) {
      if (!singleSucc) {
        singleSucc = term->getSuccessor(i);
      } else if (singleSucc != term->getSuccessor(i)) {
        allSame = false;
        break;
      }
    }

    if (allSame) {
      auto * simpleBranch = BranchInst::Create(singleSucc, term);
      vecInfo.setVectorShape(*simpleBranch, VectorShape::uni());
      vecInfo.dropVectorShape(*term);
      term->eraseFromParent();
    }
  }
}

int
Linearizer::getLeastIndex(const BasicBlock & block) const {
  if (hasIndex(block)) return getIndex(block);
  int leastIndex = -1;

  for (auto * predBlock : predecessors(&block)) {
    if (hasIndex(*predBlock)) {
      leastIndex = std::max<>(leastIndex, getIndex(*predBlock));
    }
  }

  assert (leastIndex > -1 && "has no indexed predecessors!");

  return leastIndex;
}

void
Linearizer::fixSSA() {
  for (auto & block : func) {
    if (!inRegion(block)) continue;

    DenseMap<Instruction*, Value*> promotionCache;

    for (auto & inst : block) {

      auto * phi = dyn_cast<PHINode>(&inst);

      // phi def/use repair
      if (phi) {
        for (int inIdx = 0; inIdx < phi->getNumIncomingValues(); ++inIdx) {
          auto * inBlock = phi->getIncomingBlock(inIdx);
          auto * inVal = phi->getIncomingValue(inIdx);

          auto * inInst = dyn_cast<Instruction>(inVal);
          if (!inInst) continue;

          auto & defBlock = *inInst->getParent();

          if (dt.dominates(&defBlock, inBlock)) continue;

          assert(hasIndex(defBlock));

          int defIndex = getLeastIndex(defBlock);

          auto & fixedDef = promoteDefinition(*inInst, *UndefValue::get(inInst->getType()), defIndex, *inBlock);

          phi->setIncomingValue(inIdx, &fixedDef);

        }
        continue;
      }

      // non-phi def/use repair
      for (int opIdx = 0; opIdx < inst.getNumOperands(); ++opIdx) {
        auto * opInst = dyn_cast<Instruction>(inst.getOperand(opIdx));
        if (!opInst) continue;

        auto & defParent = *opInst->getParent();

      // check if this chain was broken
        if (dt.dominates(&defParent, &block)) continue;

        int defBlockIdx = getLeastIndex(defParent);

      // do we have a cached definition available?
        Value * fixedDef = nullptr;
        auto itCachedDef = promotionCache.find(opInst);
        if (itCachedDef != promotionCache.end()) {
          fixedDef = itCachedDef->second;
        } else {
          // if not, promote the definition down to this use
          auto & promotedDef = promoteDefinition(*opInst, *UndefValue::get(opInst->getType()), defBlockIdx, block);
          promotionCache[opInst] = &promotedDef;
          fixedDef = &promotedDef;
        }

        inst.setOperand(opIdx, fixedDef);
      }
    }

    // promote predicates
    auto * blockPred = vecInfo.getPredicate(block);
    if (!blockPred) continue;
    auto * predInst = dyn_cast<Instruction>(blockPred);
    if (!predInst) continue;

    auto & predDefBlock = *predInst->getParent();
    if (dt.dominates(&predDefBlock, &block)) continue;

    int defBlockIdx = getLeastIndex(predDefBlock);

    auto * boolTy = Type::getInt1Ty(predInst->getContext());
    auto & promotedDef = promoteDefinition(*predInst, *Constant::getNullValue(boolTy), defBlockIdx, block);

    vecInfo.setPredicate(block, promotedDef);
  }
}


} // namespace rv
