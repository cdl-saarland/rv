//===- src/utils/llvmDuplication.h - fancy IR cloning --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVMDUPLICATION_HPP_
#define LLVMDUPLICATION_HPP_

#include "CommonTypes.h"
#include <llvm/IR/Dominators.h>
#include <map>

// #include "BlockCopyTracker.h"

namespace rv {

/*
 * a wrapper for CloneBasicBlock that remaps all instructions of the clone
 */
llvm::BasicBlock *cloneBlockAndMapInstructions(llvm::BasicBlock *block,
                                               ValueMap &cloneMap);

BlockSet splitNode(llvm::BasicBlock *srcBlock,
                   llvm::DominatorTree *domTree = 0);

llvm::BasicBlock *cloneBlockForBranch(
    llvm::BasicBlock *srcBlock, llvm::BasicBlock *branchBlock,
    std::map<llvm::BasicBlock *, llvm::ValueToValueMapTy *> &unifiedCloneMap,
    llvm::DominatorTree *domTree);

/*
 * fixes instructions inside the cloned blocks and instruction using the
 * original blocks, such that @branchBlock exclusively branches to the cloned
 * Blocks
 */
void patchClonedBlocksForBranch(ValueMap &cloneMap,
                                const BlockVector &originalBlocks,
                                const BlockVector &clonedBlocks,
                                llvm::BasicBlock *branchBlock);
} // namespace rv

#endif /* LLVMDUPLICATION_HPP_ */
