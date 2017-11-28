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

using namespace llvm;


#if 0
#define IF_DEBUG_BDA IF_DEBUG
#else
#define IF_DEBUG_BDA if (false)
#endif


namespace rv {

ConstBlockSet BranchDependenceAnalysis::emptySet;

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

BranchDependenceAnalysis::BranchDependenceAnalysis(Function & F,
                                                   const CDG & _cdg,
                                                   const DFG & _dfg,
                                                   const LoopInfo & _loopInfo)
: pdClosureMap()
, domClosureMap()
, effectedBlocks_old()
, effectedBlocks_new()
, cdg(_cdg)
, dfg(_dfg)
, loopInfo(_loopInfo)
{

  IF_DEBUG_BDA errs() << "-- frontiers --\n";
  for (auto & block : F) {

    ConstBlockSet pdClosure, domClosure;
    computePostDomClosure(block, pdClosure);
    computeDomClosure(block, domClosure);

    domClosure.insert(&block);
    pdClosure.insert(&block);

    IF_DEBUG_BDA {
      errs() << block.getName() << " :\n\t DFG: ";
      DumpSet(domClosure);
      errs() << "\n\t PDF: ";
      DumpSet(pdClosure);
      errs() << "\n";
    }

    pdClosureMap[&block] = pdClosure;
    domClosureMap[&block] = domClosure;
  }

#if 0 //  PDA divergence criterion
  for (auto & block : F) {
    const CDNode* cd_node = cdg[&block];
    if (!cd_node) continue;
    // Iterate over the predeccessors in the dfg, of the successors in the cdg
    for (const CDNode* cd_succ : cd_node->succs()) {
      const DFNode* df_node = dfg[cd_succ->getBB()];
      for (const DFNode* df_pred : df_node->preds()) {
        // Get the block BB that is affected by the varying branch
        const BasicBlock* const BB = df_pred->getBB();
        effectedBlocks[block.getTerminator()].insert(BB);
      }
    }
  }
#endif

  IF_DEBUG_BDA {
    errs() << "-- branch dependence analysis --\n";
    F.dump();
  }

  // maps phi blocks to divergence causing branches
  DenseMap<const BasicBlock*, ConstBlockSet> inverseMap;
// compute cd* for all blocks


  IF_DEBUG_BDA errs() << "-- branch dependence --\n";
  for (auto & z : F) {
    ConstBlockSet branchBlocks;

    for (auto itPred = pred_begin(&z), itEnd = pred_end(&z); itPred != itEnd; ++itPred) {
      auto * x = *itPred;
      auto xClosure = pdClosureMap[x];

      for (auto itOtherPred = pred_begin(&z); itOtherPred != itPred; ++itOtherPred) {
        auto * y = *itOtherPred;
        auto yClosure = pdClosureMap[y];

        IF_DEBUG_BDA { errs() << "z = " << z.getName() << " with x = " << x->getName() << " , y = " << y->getName() << "\n"; }

        // early exit on: x reaches y or y reaches x
        if (yClosure.count(x)) {
          // x is in the PDF of y
          bool added = branchBlocks.insert(x).second;
          IF_DEBUG_BDA { if (added) errs() << "\tx reaches y: add " << x->getName() << "\n"; }
        } else if (xClosure.count(y)) {
          // y is in the PDF of x
          bool added = branchBlocks.insert(y).second;
          IF_DEBUG_BDA { if (added) errs() << "\ty reaches x: add " << y->getName() << "\n"; }
        }

        // intersection (set of binary branches that are reachable from both x and y)
        auto xyPostDomClosure = Intersect(yClosure, xClosure);

        // iterate over list of candidates
        for (auto * brBlock : xyPostDomClosure) {
          if (branchBlocks.count(brBlock)) continue; // already added by early exit

          IF_DEBUG_BDA errs() << "# disjoint paths from A = " << brBlock->getName() << " to z = " << z.getName() << "\n";

          // check if there can exist a disjoint path
          bool foundDisjointPath = false;

          // for all (disjoin) pairs of leaving edges
          for (auto itSucc = succ_begin(brBlock), itEndSucc = succ_end(brBlock); !foundDisjointPath && itSucc != itEndSucc; ++itSucc) {
            auto * b = *itSucc;
            auto bClosure = domClosureMap[b];

            for (auto itOtherSucc = succ_begin(brBlock); !foundDisjointPath && itOtherSucc != itSucc; ++itOtherSucc) {
              auto * c = *itOtherSucc;
              if (b == c) continue; // multi exits to the same target (switches)

              auto cClosure = domClosureMap[c];
              auto bcDomClosure = Intersect(bClosure, cClosure);

              if (bClosure.count(&z) && cClosure.count(&z)) {
                foundDisjointPath = true;
                break;
              }
            }
          }

          // we found a disjoint path from brBlock to block
          if (foundDisjointPath) {
            IF_DEBUG_BDA errs() << "Adding " << brBlock->getName() << "\n";
            branchBlocks.insert(brBlock);
          }
        }
      }
    }

    inverseMap[&z] = branchBlocks;
  }
  IF_DEBUG_BDA errs () << "-- DONE --\n";


  IF_DEBUG_BDA {
    errs() << "-- Mapping of phi blocks to branches --\n";
    for (auto it : inverseMap) {
      errs() << it.first->getName() << " : ";
      DumpSet(it.second);
      errs() << "\n";
    }
  }

// invert result for look up table
  for (auto it : inverseMap) {
    const auto * phiBlock = it.first;
    for (auto branchBlock : it.second) {
      auto * term = branchBlock->getTerminator();
      auto * branch = dyn_cast<BranchInst>(term);
      if (branch && !branch->isConditional()) continue; // non conditional branch
      if (!branch && !isa<SwitchInst>(term)) continue; // otw, must be a switch

      IF_DEBUG_BDA { errs() << branchBlock->getName() << " inf -> " << phiBlock->getName() << "\n"; }
      effectedBlocks_old[term].insert(phiBlock);
    }
  }

// final result dump
  IF_DEBUG_BDA {
    errs() << "-- Mapping of br blocks to phi blocks --\n";
    for (auto it : effectedBlocks_old) {
      auto * brBlock = it.first->getParent();
      auto phiBlocks = it.second;
      errs() << brBlock->getName() << " : "; DumpSet(phiBlocks); errs() <<"\n";
    }
  }



  // dump PDA output for comparison
#if 0
  IF_DEBUG_PDA {
    errs() << "-- PDA output --\n";
    for (auto & block : F) {
      const CDNode* cd_node = cdg[&block];
      if (!cd_node) continue;

      bool printed = false;
      // Iterate over the predeccessors in the dfg, of the successors in the cdg
      ConstBlockSet seen;
      for (const CDNode* cd_succ : cd_node->succs()) {
        const DFNode* df_node = dfg[cd_succ->getBB()];
        for (const DFNode* df_pred : df_node->preds()) {
          // Get the block BB that is affected by the varying branch
          const BasicBlock* const BB = df_pred->getBB();
          if (!seen.insert(BB).second) continue;


          if (!printed) errs() << block.getName() << " : ";
          printed = true;

          errs() << ", " << BB->getName();
          // assert(effectedBlocks[block.getTerminator()].count(BB) && "missed a PDA block");
        }
      }
      if (printed) errs() << "\n";
    }
  }
#endif
}

/// \brief computes the iterated control dependence relation for @x
void
BranchDependenceAnalysis::computePostDomClosure(const BasicBlock & x, ConstBlockSet & closure) {
  auto * xcd = cdg[&x];
  if (!xcd) return;

  for (auto cd_succ : xcd->preds()) {
    auto * cdBlock = cd_succ->getBB();
    if (closure.insert(cdBlock).second) {
      computePostDomClosure(*cdBlock, closure);
    }
  }
}

void
BranchDependenceAnalysis::computeDomClosure(const BasicBlock & b, ConstBlockSet & closure) {
  auto * bdf = dfg[&b];
  if (!bdf) return;

  for (auto df_pred : bdf->preds()) {
    auto * dfBlock = df_pred->getBB();
    if (closure.insert(dfBlock).second) {
      computeDomClosure(*dfBlock, closure);
    }
  }
}

const ConstBlockSet&
BranchDependenceAnalysis::getEffectedBlocks(const llvm::TerminatorInst& term) const {
  auto it = effectedBlocks_new.find(&term);
  if (it != effectedBlocks_new.end()) return it->second;

  const BasicBlock* parent = term.getParent();

  // Find divergent blocks by node-disjoint paths
  // Refine the old analysis
  for (const BasicBlock* BB : getEffectedBlocks_old(term)) {
    if (DPD.divergentPaths(parent, BB)) {
      effectedBlocks_new[&term].insert(BB);
    }
  }

  // Find divergent loop exits
  if (const Loop* l = loopInfo.getLoopFor(parent)) {
    llvm::SmallVector<BasicBlock*, 4> exits;
    l->getExitBlocks(exits);

    for (const BasicBlock* exit : exits) {
      if (DPD.inducesDivergentExit(parent, exit, l)) {
        effectedBlocks_new[&term].insert(exit);
      }
    }
  }

  it = effectedBlocks_new.find(&term);
  if (it == effectedBlocks_new.end()) return emptySet;
  return it->second;
}

ConstBlockSet
BranchDependenceAnalysis::getControlDependentBlocks(const llvm::TerminatorInst& term) const {
    ConstBlockSet res;
    for (auto closure : pdClosureMap) {
        if (closure.second.count(term.getParent())) {
            res.insert(closure.first);
        }
    }
    return res;
}

} // namespace rv
