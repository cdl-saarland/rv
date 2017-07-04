/*
 * llvmDuplication.h
 *
 *  Created on: Jun 21, 2010
 *      Author: Simon Moll
 */

#ifndef LLVMDUPLICATION_HPP_
#define LLVMDUPLICATION_HPP_

#include <map>
#include <llvm/IR/Dominators.h>
#include "CommonTypes.h"


// #include "BlockCopyTracker.h"

namespace rv {

/*
 * a wrapper for CloneBasicBlock that remaps all instructions of the clone
 */
llvm::BasicBlock *cloneBlockAndMapInstructions(llvm::BasicBlock *block,
                                               ValueMap &cloneMap);

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
void patchClonedBlocksForBranch(ValueMap &cloneMap,
                                const BlockVector &originalBlocks,
                                const BlockVector &clonedBlocks,
                                llvm::BasicBlock *branchBlock);
}

#endif /* LLVMDUPLICATION_HPP_ */
