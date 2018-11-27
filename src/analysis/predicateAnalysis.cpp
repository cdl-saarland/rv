//===------------------ predicateAnalysis.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//

#include "rv/analysis/predicateAnalysis.h"
#include "rv/vectorizationInfo.h"
#include "rvConfig.h"

#include <llvm/Support/raw_ostream.h>

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
  }
}
