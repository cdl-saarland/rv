//===- rv/transform/guardedDivLoopTrans.h - make divergent loops uniform --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//


#ifndef RV_TRANSFORM_GUARDEDDIVLOOPTRANS_H
#define RV_TRANSFORM_GUARDEDDIVLOOPTRANS_H


#include "rv/vectorizationInfo.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Instructions.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/IR/PassManager.h"


namespace llvm {
  class ConstantInt;
  class IntegerType;
  class LoopInfo;
  class DominatorTree;
  class PHINode;
  class LoopInfo;
}

namespace rv {

using PHIVec = llvm::SmallVector<llvm::PHINode*, 16>;
using PHISet = llvm::SmallPtrSet<llvm::PHINode*, 16>;
using BlockSet = llvm::SmallPtrSet<llvm::BasicBlock*, 4>;

class PlatformInfo;
class LiveValueTracker;


struct GuardedTrackerDesc {
  llvm::PHINode * wrapPhi;    // kill exit live-out wrapper (@header)
  llvm::PHINode * trackerPhi; // divergent live out tracker (@header)
  llvm::PHINode * updatePhi;  // divergent live out updater (@pureLatch)

  GuardedTrackerDesc()
  : wrapPhi(nullptr)
  , trackerPhi(nullptr)
  , updatePhi(nullptr)
  {}
};

// Divergence tracker for a loop
struct GuardedTransformSession {
  llvm::Loop & loop;
  std::string loopName;
  llvm::LoopInfo & loopInfo;
  VectorizationInfo & vecInfo;
  PlatformInfo & platInfo;

  llvm::BasicBlock * testHead;
  llvm::BasicBlock * offsetHead;
  llvm::BasicBlock * pureLatch; // nullptr if the latch is not pure (yet)
  llvm::BasicBlock * oldLatch; // if pureLatch, then oldLatch is the unique predecessor to pureLatch

  // phi nodes in the pureLatch to obtain a dominating definition of loop
  // carried values for the header phi nodes.
  llvm::SmallVector<llvm::PHINode*, 4> PureDomPhis;

  // register
  // \p SrcBlock as a new input to \p PureLatch in the way of adding 'undef'
  // inputs to all header carry phi nodes (PureDomPhis)
  void addInputForHeaderCarryPhis(llvm::BasicBlock& srcBlock);

  // state tracking infrastructure
  GuardedTrackerDesc liveMaskDesc;
  // maps each exit block to the exit tracker in this loop
  llvm::DenseMap<const llvm::BasicBlock*, GuardedTrackerDesc> exitDescs;
  // maps each live out to a tracker
  llvm::DenseMap<const llvm::Value*, GuardedTrackerDesc> liveOutDescs;
  GuardedTrackerDesc & requestGuardedTrackerDesc(const llvm::Value& val); // creates an emty tracker if missing
  const GuardedTrackerDesc & getGuardedTrackerDesc(const llvm::Value& val) const; // asserting getter

  llvm::BasicBlock*
  remapExitingBlock(llvm::BasicBlock * exitingBlock) {
    if (exitingBlock == loop.getHeader()) return offsetHead;
    return exitingBlock;
  }

  size_t numKillExits;
  size_t numDivExits;

  GuardedTransformSession(llvm::Loop & _loop, llvm::LoopInfo & _loopInfo, VectorizationInfo & _vecInfo, PlatformInfo & _platInfo)
  : loop(_loop)
  , loopName(loop.getName().str())
  , loopInfo(_loopInfo)
  , vecInfo(_vecInfo)
  , platInfo(_platInfo)
  , testHead(nullptr)
  , offsetHead(nullptr)
  , pureLatch(nullptr)
  , oldLatch(nullptr)
  , liveMaskDesc()
  , numKillExits(0)
  , numDivExits(0)
  {}

  // transform to a uniform loop
  // returns the canonical live mask phi
  void transformLoop();

  //
  void finalizeLiveOutTrackers();

  void finalizeLiveOutTracker(GuardedTrackerDesc & desc);

  llvm::BasicBlock & requestPureLatch();
};



// actual transformation
class GuardedDivLoopTrans {
  PlatformInfo & platInfo;
  VectorizationInfo & vecInfo;
  llvm::FunctionAnalysisManager & FAM;
  llvm::IntegerType * boolTy;
  // collect all divergent exits of this loop and send them through a dedicated latch exit

  llvm::DenseMap<const llvm::Loop*, GuardedTransformSession*> sessions;

// Control phase
  // return true, if any loops were transformed
  bool transformDivergentLoopControl(llvm::LoopInfo & LI, llvm::Loop & loop);

  // this finalizes the control conversion on @loop
  // void convertToLatchExitLoop(llvm::Loop & loop, LiveValueTracker & liveOutTracker);

// finalization phase
  // descend into all of @loop's loops and attach an input mask to the loop live mask phi
  void addLoopInitMasks(llvm::Loop & loop);

  // replace this value update phi with a proper blend cascade
public:
  GuardedDivLoopTrans(PlatformInfo & _platInfo, VectorizationInfo & _vecInfo, llvm::FunctionAnalysisManager &FAM);
  ~GuardedDivLoopTrans();

  // makes all divergent loops in the region uniform
  void transformDivergentLoops();

  // stats
  size_t numUniformLoops;
  size_t numDivergentLoops;
  size_t numKillExits;
  size_t numDivExits;
};

}

#endif // RV_TRANSFORM_GUARDEDDIVLOOPTRANS_H
