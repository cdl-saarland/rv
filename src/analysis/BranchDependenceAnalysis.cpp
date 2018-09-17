//===- BranchDependenceAnalysis.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon
//
//

#include "rv/analysis/BranchDependenceAnalysis.h"

#include <llvm/IR/Dominators.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/Analysis/PostDominators.h>

#include "rvConfig.h"
#include <cassert>
#include <map>

using namespace llvm;


#if 1
#define IF_DEBUG_BDA IF_DEBUG
#else
#define IF_DEBUG_BDA if (true)
#endif


namespace rv {

ConstBlockSet BranchDependenceAnalysis::emptyBlockSet;

BranchDependenceAnalysis::BranchDependenceAnalysis(Region & _region,
                           const llvm::DominatorTree & _domTree,
                           const llvm::PostDominatorTree & _postDomTree,
                           const llvm::LoopInfo & _loopInfo)
: region(_region)
, domTree(_domTree)
, postDomTree(_postDomTree)
, loopInfo(_loopInfo)
{
  IF_DEBUG_BDA {
    errs() << "Region for BDA: ";  region.getFunction().print(errs());
  }
}


BranchDependenceAnalysis::~BranchDependenceAnalysis() {
  for (auto it : cachedJoinBlocks) {
    delete it.second;
  }
}


/// \brief returns the set of blocks whose PHI nodes become divergent if @branch is divergent
const ConstBlockSet &
BranchDependenceAnalysis::join_blocks(const llvm::TerminatorInst & term) {
  IF_DEBUG_BDA { errs() << "-- BDA::join_block for " << term.getParent()->getName() << " --\n"; }
  if (term.getNumSuccessors() < 1) {
    return emptyBlockSet;
  }

  auto it = cachedJoinBlocks.find(&term);
  if (it != cachedJoinBlocks.end()) return *it->second;

  auto * joinBlocks = new ConstBlockSet;

  // immediate post dominator (no join block beyond that block)
  const auto * pdNode = postDomTree.getNode(const_cast<BasicBlock*>(term.getParent()));
  const auto * ipdNode = pdNode->getIDom();
  const auto * pdBoundBlock = ipdNode ? ipdNode->getBlock() : nullptr;

  IF_DEBUG_BDA if (pdBoundBlock) {
    errs() << "post dom bound " << pdBoundBlock->getName() << "\n";
  }
  // loop of branch (loop exits may exhibit temporal diverence)
  const auto * termLoop = loopInfo.getLoopFor(term.getParent());

  // maps blocks to last valid def
  using DefMap = std::map<const BasicBlock*, const BasicBlock*>;
  DefMap defMap;

  std::vector<DefMap::iterator> worklist;

  // loop exits
  llvm::SmallPtrSet<const BasicBlock*, 4> exitBlocks;

  // immediate successor blocks (of @term)
  SmallPtrSet<const BasicBlock*, 2> succBlocks;

  // bootstrap with branch targets
  for (const auto * succBlock : successors(term.getParent())) {
    if (!region.contains(succBlock)) continue;

    auto itPair = defMap.emplace(succBlock, succBlock);

    succBlocks.insert(succBlock);

    // immediate loop exit from @term
    if (termLoop && !termLoop->contains(succBlock)) {
      exitBlocks.insert(succBlock);
      continue;
    }

    // otw, propagate
    worklist.push_back(itPair.first);
  }

  const BasicBlock * termLoopHeader = termLoop ? termLoop->getHeader() : nullptr;

  // propagate def (collecting join blocks on the way)
  while (!worklist.empty()) {
    auto itDef = worklist.back();
    worklist.pop_back();

    const auto * block = itDef->first;
    const auto * defBlock = itDef->second;
    assert(defBlock);

    if (exitBlocks.count(block)) continue;

    IF_DEBUG_BDA { errs() << "BDA: prop " << block->getName() << " with def " << defBlock->getName() <<  ".\n"; }

    // don't step over postdom (if any)
    if (block == pdBoundBlock) continue;

    if (block == termLoopHeader) continue; // don't propagate beyond termLoopHeader or def will be overwritten

    for (const auto * succBlock : successors(block)) {
      IF_DEBUG_BDA { errs() << "BDA: successor " << succBlock->getName() << " with def " << defBlock->getName() <<  ".\n"; }
      // if (succBlock == defBlock) continue; // detect loops

      if (!region.contains(succBlock)) continue;

      // loop exit (temporal divergence)
      const auto * succLoop = loopInfo.getLoopFor(succBlock);
      if (termLoop &&
         (!succLoop || !termLoop->contains(succBlock)))
      {
        IF_DEBUG_BDA { errs() << "\t loop exit.\n"; }
        defMap.emplace(succBlock, defBlock);
        exitBlocks.insert(succBlock);
        continue;
      }

      // regular successor on same loop level
      auto itLastDef = defMap.find(succBlock);

      // first reaching def
      if (itLastDef == defMap.end()) {
        IF_DEBUG_BDA { errs() << "\t first reaching @ " << succBlock->getName() << " is " << defBlock->getName() << ".\n"; }
        auto itNext = defMap.emplace(succBlock, defBlock).first;
        worklist.push_back(itNext);
        continue;
      }

      const auto * lastSuccDef = itLastDef->second;

      // control flow join (establish new def)
      if ((lastSuccDef != defBlock) ||
          ((defBlock == succBlock) && succBlocks.count(defBlock))
      ) {
        if (joinBlocks->insert(succBlock).second) {
          IF_DEBUG_BDA { errs() << "\t join @ " << succBlock->getName() << ".\n"; }
          auto itNewDef = defMap.emplace(succBlock, succBlock).first;
          worklist.push_back(itNewDef);
        }
      }
    }
  }

  // if the ipd is inside the loop, the definition at the loop header will be the same as at the ipd (no other defs can reach)
  //
  // A // loop header
  // |
  // B // nested loop header
  // |
  // C -> X (exit from B loop) -..-> (A latch)
  // |
  // D -> back to B (B latch)
  // |
  // proper exit from both loops
  //
  // D post-dominates B as it is the only proper exit from the "A loop".
  // If C has a divergent branch, propagation will therefore stop at D.
  // That implies that B will never receive a definition.
  // But that definition can only be the same as at D (D itself in thise case) because all paths to anywhere have to pass through D.
  //
  if (termLoop && termLoop->contains(pdBoundBlock)) {
    defMap[termLoopHeader] = defMap[pdBoundBlock];
  }

  // analyze reached loop exits
  if (!exitBlocks.empty()) {
    assert(termLoop);
    const auto * headerDefBlock = defMap[termLoopHeader];
    assert(headerDefBlock && "no definition in header of carrying loop");

    for (const auto * exitBlock : exitBlocks) {
      IF_DEBUG_BDA { errs() << "BDA: (post) loop exit: " << exitBlock->getName() << "\n"; }
      assert((defMap[exitBlock] != nullptr) && "no reaching def at loop exit");
      if (defMap[exitBlock] != headerDefBlock) {
        IF_DEBUG_BDA { errs() << "\t divergent loop exit: " << exitBlock->getName() << "\n"; }
        joinBlocks->insert(exitBlock);
      }
    }
  }
  IF_DEBUG_BDA { errs() << "-- end of join_blocks --\n\n"; }

  cachedJoinBlocks[&term] = joinBlocks;
  return *joinBlocks;
}


} // namespace rv
