//===- src/utils/llvmDomination.h - fancy dominator queries --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVMDOMINATION_HPP_
#define LLVMDOMINATION_HPP_

#include "CommonTypes.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/Dominators.h"

namespace rv {

typedef std::vector<llvm::DomTreeNode *> DomTreeNodeVector;

/// collect all blocks that are reachable and post-dominated from @entryBlock
BlockSet getSelfDominatedBlocks(llvm::BasicBlock *entryBlock,
                                llvm::PostDominatorTree &postDomTree);

bool dominatesAll(llvm::DominatorTree &domTree, llvm::DomTreeNode *node,
                  const BlockSet &blocks);

llvm::DomTreeNode *findImmediateDominator(llvm::DominatorTree &domTree,
                                          const BlockSet &blocks);

BlockSet computeDominatedRegion(llvm::DominatorTree &domTree,
                                llvm::BasicBlock *header, BlockSet exits);
} // namespace rv

#endif /* LLVMDOMINATION_HPP_ */
