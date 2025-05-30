//===- src/transform/loopExitCanonicalizer.cpp - exit : exiting == 1:1  --*- C++
//-*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/passes/loopExitCanonicalizer.h"

#include <stdexcept>

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Instructions.h>

#include "rvConfig.h"
#include "utils/rvTools.h"

using namespace llvm;
using namespace rv;

LoopExitCanonicalizer::LoopExitCanonicalizer(LoopInfo &loopInfo)
    : mLoopInfo(loopInfo) {}

bool LoopExitCanonicalizer::canonicalize(Function &F) {
  for (auto &L : mLoopInfo) {
    canonicalizeLoop(L);
  }

  return false;
}

void LoopExitCanonicalizer::canonicalizeLoop(Loop *loop) const {
  assert(loop);

  for (auto &SL : *loop) {
    canonicalizeLoop(SL);
  }

  SmallVector<BasicBlock *, 2> exitBlocks;
  loop->getExitBlocks(exitBlocks);

  for (const auto &exitBB : exitBlocks) {
    Loop *outerLoop = mLoopInfo.getLoopFor(exitBB);
    assert(outerLoop != loop);

    if (exitBB->getUniquePredecessor()) {
      assert(loop->isLoopExiting(exitBB->getUniquePredecessor()));
      continue;
    }

    IF_DEBUG {
      outs() << "loop exit has more than one incoming edge: '";
      outs() << exitBB->getName() << "' - canonicalizing...\n";
    }

    SmallVector<BasicBlock *, 2> exitingBlocks;
    rv::getExitingBlocks(exitBB, mLoopInfo, exitingBlocks);

    for (auto &exitingBB : exitingBlocks) {
      assert(mLoopInfo.getLoopFor(exitingBB) == loop &&
             "exiting blocks from different loop go to same exit block - not "
             "in LCSSA?");

      BasicBlock *newExitBB = createIntermediateBlock(exitingBB, exitBB);
      if (outerLoop) {
        outerLoop->addBasicBlockToLoop(newExitBB, mLoopInfo);
      }
    }
  }

  IF_DEBUG {
    exitBlocks.clear();
    loop->getExitBlocks(exitBlocks);
    for (const auto &exitBB : exitBlocks) {
      assert(exitBB->getUniquePredecessor());
    }
  }
}

BasicBlock *
LoopExitCanonicalizer::createIntermediateBlock(BasicBlock *source,
                                               BasicBlock *target) const {
  assert(source && target);
  assert(!target->getUniquePredecessor());

  BasicBlock *newTarget = BasicBlock::Create(
      target->getContext(), "loop.exit.dedicated", target->getParent(), target);

  // Adjust edge.
  replaceTarget(source, target, newTarget);

  // Create edge to from new to old target.
  BranchInst::Create(target, newTarget);

  // Adjust phis.
  adjustPhis(source, target, newTarget);

  return newTarget;
}

void LoopExitCanonicalizer::adjustPhis(BasicBlock *source, BasicBlock *target,
                                       BasicBlock *newTarget) const {
  assert(source && target && newTarget);

  for (auto &I : *target) {
    if (!isa<PHINode>(I))
      break;
    PHINode *phi = cast<PHINode>(&I);

    PHINode *newPhi = PHINode::Create(phi->getType(), 2, "lcssa.phi",
                                      newTarget->getFirstNonPHIIt());

    newPhi->addIncoming(phi->getIncomingValueForBlock(source), source);

    phi->removeIncomingValue(source, false /* DeletePhiIfEmpty */);
    phi->addIncoming(newPhi, newTarget);
  }
}

void LoopExitCanonicalizer::replaceTarget(BasicBlock *source,
                                          BasicBlock *target,
                                          BasicBlock *newTarget) const {
  assert(source && target && newTarget);

  auto *terminator = source->getTerminator();

  bool replaced = false;
  RV_UNUSED(replaced);
  for (unsigned i = 0, e = terminator->getNumSuccessors(); i < e; ++i) {
    BasicBlock *succBB = terminator->getSuccessor(i);
    if (succBB != target)
      continue;

    terminator->setSuccessor(i, newTarget);
    replaced = true;
    break; // first successor replaced..
  }

  assert(replaced);
}
