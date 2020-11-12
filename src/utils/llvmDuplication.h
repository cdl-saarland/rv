//===---- utils/llvmDuplication.h - Convenient BB Cloning -----*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//


#ifndef RV_SRC_UTILS_LLVMDUPLICATION_H_
#define RV_SRC_UTILS_LLVMDUPLICATION_H_

#include <map>
#include <vector>
#include <set>

#include <llvm/IR/Dominators.h>
#include <llvm/IR/ValueMap.h>
#include <llvm/Transforms/Utils/ValueMapper.h>


// #include "BlockCopyTracker.h"

namespace rv {

typedef std::vector<llvm::BasicBlock *> BlockVector;
typedef std::set<llvm::BasicBlock *> BlockSet;

/*
 * a wrapper for CloneBasicBlock that remaps all instructions of the clone
 */
llvm::BasicBlock *cloneBlockAndMapInstructions(llvm::BasicBlock *block,
                                               llvm::ValueToValueMapTy &cloneMap);

BlockSet splitNode(llvm::BasicBlock *srcBlock,
                   llvm::DominatorTree *domTree = 0);

llvm::BasicBlock *cloneBlockForBranch(llvm::BasicBlock *srcBlock,
                                      llvm::BasicBlock *branchBlock,
                                      std::map<llvm::BasicBlock*,llvm::ValueToValueMapTy*> & unifiedCloneMap,
                                      llvm::DominatorTree *domTree);

/*
 * fixes instructions inside the cloned blocks and instruction using the
 * original blocks, such that @branchBlock exclusively branches to the cloned
 * Blocks
 */
void patchClonedBlocksForBranch(llvm::ValueToValueMapTy &cloneMap,
                                const BlockVector &originalBlocks,
                                const BlockVector &clonedBlocks,
                                llvm::BasicBlock *branchBlock);
}

#endif /* RV_SRC_UTILS_LLVMDUPLICATION_H_ */
