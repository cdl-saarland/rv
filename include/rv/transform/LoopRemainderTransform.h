//
// Created by tkloessner on 24.04.17.
//

#ifndef RV_LOOPREMAINDERTRANSFORM_H
#define RV_LOOPREMAINDERTRANSFORM_H

#include "llvm/Analysis/LoopInfo.h"
#include <llvm/Analysis/ScalarEvolution.h>

using namespace llvm;

namespace rv {

class LoopRemainderTransform {
public:
  LoopRemainderTransform(LoopInfo& LI, ScalarEvolution* SE, Loop* ScalarLoop)
          : LI(LI), ScalarLoop(ScalarLoop) { }

  /// Returns the newly created loop in which vectorized instructions will be inserted
  Loop* createLoopEpilogueStructure(unsigned VF);

  BasicBlock* getMiddleBlock() { return LoopMiddleBlock; }
  BasicBlock* getExitBlock() { return LoopExitBlock; }

private:
  unsigned VF;

  LoopInfo& LI;
  ScalarEvolution* SE;

  Loop* ScalarLoop;
  Loop* VectorLoop = nullptr;

  BasicBlock* LoopMiddleBlock = nullptr;
  BasicBlock* LoopExitBlock = nullptr;

  SmallVector<BasicBlock*, 4> LoopBypassBlocks;

  Value* TripCount = nullptr; // total loop iteration
  Value* VectorTripCount = nullptr; // vector loop iteration value

  Value* getOrCreateTripCount(Loop* L);
  Value* getOrCreateVectorTripCount(Loop* L);
  void emitVectorLoopEnteredCheck(Loop* L, LoopInfo& LI, BasicBlock* Bypass);
  void emitMinimumIterationCountCheck(Loop* L, LoopInfo& LI, BasicBlock* Bypass);
  PHINode* createInductionVariable(Loop* L, Value* Start, Value* End, Value* Step);

  /// Fix the starting values of the remainder loop.
  /// Places new phis into the new preheader, and sets the incoming values of the
  /// remainder loop phis to these.
  void fixRemainderLoopPHIs() { assert(false && "implement"); }
};

}

#endif //RV_LOOPREMAINDERTRANSFORM_H
