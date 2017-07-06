#ifndef RV_TRANSFORM_DIVLOOPTRANS_H
#define RV_TRANSFORM_DIVLOOPTRANS_H


#include "rv/vectorizationInfo.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/SmallSet.h"



namespace llvm {
  class ConstantInt;
  class IntegerType;
  class LoopInfo;
  class DominatorTree;
}

namespace rv {

using PHIVec = llvm::SmallVector<llvm::PHINode*, 16>;
using PHISet = llvm::SmallPtrSet<llvm::PHINode*, 16>;
using BlockSet = llvm::SmallPtrSet<llvm::BasicBlock*, 4>;

class PlatformInfo;
class MaskExpander;
class LiveValueTracker;

// Divergence tracker for a loop
struct LoopTracker {
  llvm::Loop & loop;
  // already needed if the loop itself is non-divergent but is crossed by divergent loop exits
  llvm::PHINode & loopMaskPhi; // loop mask PHi

  // these two are only needed if there is a divergent exit directly from this loop
  llvm::PHINode * maskUpdatePhi; // mask update in the pure latch (if any)
  llvm::BasicBlock * pureLatch; // nullptr if the latch is not pure (yet)
  llvm::BasicBlock * oldLatch; // if pureLatch, then oldLatch is the unique predecessor to pureLatch

  // value updates
  struct ValueUpdate {
    llvm::PHINode * valueTracker; // value tracker
    llvm::Instruction * latchUpdate; // latch update on this loop level (phi or fixLiveOutUse)

#if 0
    ValueUpdate(PHINode & _valueTracker, PHINode & _latchUpdate)
    : valueTracker(_valueTracker)
    . latchUpdate(_latchUpdate)
    {}
#endif
  };

  // maps exits to exit mask updates
  llvm::DenseMap<llvm::BasicBlock*,  ValueUpdate> latchExitUpdates;
  llvm::DenseMap<llvm::Instruction*, ValueUpdate> latchValueUpdates;

  LoopTracker(llvm::Loop & _loop, llvm::PHINode & _loopMaskPhi)
  : loop(_loop)
  , loopMaskPhi(_loopMaskPhi)
  , maskUpdatePhi(nullptr)
  , pureLatch(nullptr)
  , oldLatch(nullptr)
  , latchExitUpdates()
  {}

  LoopTracker(LoopTracker & o)
  : loop(o.loop)
  , loopMaskPhi(o.loopMaskPhi)
  , maskUpdatePhi(o.maskUpdatePhi)
  , pureLatch(o.pureLatch)
  , oldLatch(o.oldLatch)
  , latchExitUpdates(o.latchExitUpdates)
  {}

  // generates a phi node in the pure latch that tracks whether @exit was taken in this loop iteration
  llvm::PHINode & requestExitUpdate(llvm::BasicBlock & exit, llvm::PHINode & exitTracker);

  ValueUpdate & getExitUpdate(llvm::BasicBlock & exit);
};



// actual transformation
class DivLoopTrans {
  PlatformInfo & platInfo;
  VectorizationInfo & vecInfo;
  MaskExpander & maskEx;
  llvm::DominatorTree & domTree;
  llvm::LoopInfo & loopInfo;

  llvm::IntegerType * boolTy;

  // loop mask scaffolding
  llvm::DenseMap<llvm::Loop*, LoopTracker*> loopTrackers;

  // splits the latch to make it empty except for an unconditional header branch (pure latch)
  llvm::BasicBlock & requestPureLatch(LoopTracker & loopTracker);

  // generates a live mask for this loop
  LoopTracker & requestLoopTracker(llvm::Loop & loop);
  LoopTracker & getLoopTracker(llvm::Loop & loop); // asserting getter

  // collect all divergent exits of this loop and send them through a dedicated latch exit

  // keep track of kill exits while the loop is transformed
  PHISet killPhis;
  void addKillPhi(llvm::PHINode & killExitLCSSAPhi) { killPhis.insert(&killExitLCSSAPhi); }
  bool isKillPhi(llvm::PHINode & lcssaPhi) const { return killPhis.count(&lcssaPhi); }

// Control phase
  // return true, if any loops were transformed
  bool transformDivergentLoopControl(llvm::Loop & loop, LiveValueTracker & liveOutTracker);

  // this finalizes the control conversion on @loop
  void convertToLatchExitLoop(llvm::Loop & loop, LiveValueTracker & liveOutTracker);

  // do not track these live outs (kill exit live outs)
  void trackPreservedLiveOuts(llvm::BasicBlock & exitBlock);

  // create trackers/updates for liveouts across to this exit
  void trackLiveOuts(llvm::Loop &hostLoop, llvm::BasicBlock & exitignBlock, llvm::BasicBlock & exitBlock, llvm::BasicBlock & reboundBlock, LiveValueTracker & liveOutTracker);

// Latch update phase
  // after ALL divergent loops have been control converted fix the latch updates and exit conditions
  void fixDivergentLoopUpdates(llvm::Loop & loop, LiveValueTracker & liveOutTracker);
  void fixLatchUpdates(llvm::Loop & loop, LiveValueTracker & liveOutTracker);

// Latch update phase
  // after ALL divergent loops have been control converted fix the latch updates and exit conditions
  void fixDivergentLiveOutUses(llvm::Loop & loop, LiveValueTracker & liveOutTracker);
  void fixLiveOutUses(llvm::Loop & loop, LiveValueTracker & liveOutTracker);

// finalization phase
  // descend into all of @loop's loops and attach an input mask to the loop live mask phi
  void addLoopInitMasks(llvm::Loop & loop);

  // adds missing incoming values to all latch update phis
  void addDefaultInputsToLatchUpdates();

  // replace this value update phi with a proper blend cascade
  llvm::Instruction& implementPhiUpdate(LoopTracker::ValueUpdate & valUpd);
public:
  DivLoopTrans(PlatformInfo & _platInfo, VectorizationInfo & _vecInfo, MaskExpander & _maskEx, llvm::DominatorTree & _domTree, llvm::LoopInfo & _loopInfo);
  ~DivLoopTrans();

  // makes all divergent loops in the region uniform
  void transformDivergentLoops();

  // stats
  size_t numDivergentLoops;
  size_t numKillExits;
};

}

#endif // RV_TRANSFORM_DIVLOOPTRANS_H
