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

inline
void
IntersectInPlace(ConstBlockSet & x, const ConstBlockSet & y) {
  for (auto a : x) {
    if (!y.count(a)) x.erase(a);
  }
}

inline
ConstBlockSet
Intersect(const ConstBlockSet & x, const ConstBlockSet & y) {
  ConstBlockSet res;
  for (auto a : x) {
    if (y.count(a)) res.insert(a);
  }
  for (auto b : y) {
    if (x.count(b)) res.insert(b);
  }
  return res;
}

inline
void MergeIn(ConstBlockSet & m, ConstBlockSet & other) {
  for (auto b : other) m.insert(b);
}

inline
void
IntersectAndMerge(ConstBlockSet & accu, const ConstBlockSet & a, const ConstBlockSet & b) {
  for (auto x : a) {
    if (b.count(x)) {
      accu.insert(x);
    }
  }
}

inline
void
Subtract(ConstBlockSet & a, const ConstBlockSet & b) {
  for (auto y : b) {
    a.erase(y);
  }
}

inline
void
DumpSet(const ConstBlockSet & blocks) {
  errs() << "{";
  for (const auto * bb : blocks) {
    errs() << ", " << bb->getName();
  }
  errs() << "}";
}

inline
void
GetDomRegion(DomTreeNodeBase<BasicBlock> & domNode, ConstBlockSet & domRegion) {
  domRegion.insert(domNode.getBlock());
  for (auto it = domNode.begin(); it != domNode.end(); ++it) {
    GetDomRegion(**it, domRegion);
  }
}

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

  // bootstrap with branch targets
  for (const auto * succBlock : successors(term.getParent())) {
    if (!region.contains(succBlock)) continue;

    auto itPair = defMap.emplace(succBlock, succBlock);

    // immediate loop exit from @term
    const auto * succLoop = loopInfo.getLoopFor(succBlock);
    if (termLoop && (!succLoop || !termLoop->contains(succLoop->getHeader()))) {
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
         (!succLoop || !termLoop->contains(succLoop->getHeader())))
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
      if (lastSuccDef != defBlock) {
        IF_DEBUG_BDA { errs() << "\t join @ " << succBlock->getName() << ".\n"; }
        auto itNewDef = defMap.emplace(succBlock, succBlock).first;
        worklist.push_back(itNewDef);

        joinBlocks->insert(succBlock);
      }
    }
  }

  // analyze reached loop exits
  if (!exitBlocks.empty()) {
    assert(termLoop);
    auto * loopHeader = termLoop->getHeader();
    const auto * headerDefBlock = defMap[loopHeader];
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
