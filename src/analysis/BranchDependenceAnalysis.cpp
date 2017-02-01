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

#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>

using namespace llvm;

namespace rv {

BranchDependenceAnalysis::BranchDependenceAnalysis(llvm::Function & F, const CDG & _cdg, const DFG & _dfg)
: effectedBlocks()
, cdg(_cdg)
, dfg(_dfg)
{
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
}


} // namespace rv
