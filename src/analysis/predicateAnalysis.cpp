//===- src/analysis/predicateAnalysis.cpp - divergent (trans) cdep computation --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/analysis/predicateAnalysis.h"
#include "rv/vectorizationInfo.h"
#include "rvConfig.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/CFG.h"

#if 1
#define IF_DEBUG_PREDA IF_DEBUG
#else
#define IF_DEBUG_PREDA if (true)
#endif

using namespace llvm;
using namespace rv;

void
PredicateAnalysis::addDivergentBranch(const llvm::BasicBlock & rootBlock, llvm::ArrayRef<const llvm::BasicBlock*> & divSuccArray, BlockCallbackFunc BlockCallback) {
// infer pdbound (re-convergence point)
  const auto * rootPdNode = PDT.getNode(&rootBlock);
  const auto * pdBoundNode = rootPdNode ? rootPdNode->getIDom() : nullptr;
  const BasicBlock * pdBoundBlock = pdBoundNode ? pdBoundNode->getBlock() : nullptr;

// set the varying predicate flag for all reachable blocks up to (and excluding) the pdBoundNode.
  // divergent control will re-converge due to this branch will re-converge at pdBoundNode (if at all).
  std::vector<const BasicBlock*> taintStack;
  for (const auto * succBlock : divSuccArray) {
    taintStack.push_back(succBlock);
  }

  std::set<const BasicBlock*> taintedBlocks; // already been there
  while (!taintStack.empty()) {
    // fetch an unseen, reachable block
    const auto * reachableBlock = taintStack.back();
    taintStack.pop_back();
    if (!taintedBlocks.insert(reachableBlock).second) continue;

    if (reachableBlock == pdBoundBlock) continue;

    bool wasVarying = false;
    if (vecInfo.getVaryingPredicateFlag(*reachableBlock, wasVarying) && wasVarying) {
      // we already new this block has a varying predicate
      continue;
    }

    // Otw, signal the varying block
    vecInfo.setVaryingPredicateFlag(*reachableBlock, true);
    BlockCallback(*reachableBlock);

    // Descend into successors
    for (auto *Succ : successors(reachableBlock)) {
      if (!taintedBlocks.count(Succ)) taintStack.push_back(Succ);
    }
  }
}
