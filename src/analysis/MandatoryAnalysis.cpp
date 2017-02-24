//
// Created by tkloessner on 24.02.17.
//

#include <llvm/IR/CFG.h>
#include "rv/analysis/MandatoryAnalysis.h"

namespace rv {

MandatoryAnalysis::MandatoryAnalysis(VectorizationInfo& vInfo,
                                     const LoopInfo& LoopInfo,
                                     const CDG& cdg)
        : mVecInfo(vInfo), mLoopInfo(LoopInfo), mCDG(cdg)
{
}

void MandatoryAnalysis::analyze(Function& F) {
  for (auto&& BB : F) {
    if (mVecInfo.getVectorShape(*BB.getTerminator()).isVarying()) {
      // MANDATORY case 1: successors of varying branches are mandatory
      for (const BasicBlock* succBB : successors(&BB)) {
        mVecInfo.markMandatory(succBB);
      }
      markDependentLoopExitsMandatory(&BB);
    }

    if (mVecInfo.hasKnownShape(BB) && mVecInfo.getVectorShape(BB).isVarying()) {
      // MANDATORY case 2: divergent block
      mVecInfo.markMandatory(&BB);
    }
  }

  markDivergentLoopLatchesMandatory();
}

void MandatoryAnalysis::markDivergentLoopLatchesMandatory() {
  for (const Loop* loop : mLoopInfo) {
    markLoopLatchesRecursively(loop);
  }
}

void MandatoryAnalysis::markLoopLatchesRecursively(const Loop* loop) {
  // MANDATORY case 3: latches of divergent loops are mandatory
  if (mVecInfo.isDivergentLoop(loop)) {
    mVecInfo.markMandatory(loop->getLoopLatch());
  }

  for (const Loop* sLoop : loop->getSubLoops()) {
    markLoopLatchesRecursively(sLoop);
  }
}

void MandatoryAnalysis::markDependentLoopExitsMandatory(const BasicBlock* BB) {
  Loop* loop = mLoopInfo.getLoopFor(BB);

  // No exits that can be mandatory
  if (!loop) return;

  // MANDATORY case 4: can be nicely put with just the cdg,
  //                   if an exit is control dependent on
  //                   this varying branch, then there is
  //                   one path that stays in the loop and
  //                   eventually reaches the latch, and one
  //                   that reaches the exit, as its control dependent

  SmallVector<BasicBlock*, 8> LoopExits;
  loop->getExitBlocks(LoopExits);

  const CDNode* cd_node = mCDG[BB];
  for (const CDNode* cd_succ : cd_node->succs()) {
    for (const BasicBlock* exit : LoopExits) {
      if (exit == cd_succ->getBB()) {
        mVecInfo.markMandatory(exit);
      }
    }
  }
}

}