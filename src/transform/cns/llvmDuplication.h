/*
 * llvmDuplication.h
 *
 *  Created on: Jun 21, 2010
 *      Author: Simon Moll
 */

#ifndef LLVMDUPLICATION_HPP_
#define LLVMDUPLICATION_HPP_

#include <llvm/Analysis/LoopPass.h>
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

// splits a node for a set of branches, does ignore empty sets in predecessorSet
// and returns 0 for the cloned block of them
BlockVector splitNodeExt(llvm::BasicBlock *srcBlock,
                         BlockSetVector predecessorSet,
                         llvm::DominatorTree *domTree);

LoopSet splitLoop(llvm::LoopInfo &loopInfo, llvm::Loop *loop, llvm::Pass *pass,
                  llvm::DominatorTree *domTree = NULL);

llvm::BasicBlock *cloneBlockForBranch(llvm::BasicBlock *srcBlock,
                                      llvm::BasicBlock *branchBlock,
                                      llvm::DominatorTree *domTree = NULL);
llvm::BasicBlock *cloneBlockForBranchSet(llvm::BasicBlock *srcBlock,
                                         BlockSet branchSet,
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
void patchClonedBlocksForBranches(ValueMap &cloneMap,
                                  const BlockVector &originalBlocks,
                                  const BlockVector &clonedBlocks,
                                  BlockSet branchBlocks);

// FIXME

// llvm::Loop * cloneLoopForBranch(BlockCopyTracker & tracker,
// llvm::LPPassManager & lpm, llvm::Pass * pass, llvm::LoopInfo & loopInfo,
// llvm::Loop * loop, llvm::BasicBlock * branchBlock);  //dummy tracker argument
// (legacy code support)

// llvm::Loop * cloneLoopForBranch(llvm::LPPassManager & lpm, llvm::Pass * pass,
// llvm::LoopInfo & loopInfo, llvm::Loop * loop, llvm::BasicBlock * branchBlock,
// llvm::DominatorTree * domTree=NULL);
}

#endif /* LLVMDUPLICATION_HPP_ */
