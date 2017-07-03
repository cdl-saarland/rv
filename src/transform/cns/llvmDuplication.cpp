/*
 * llvmDuplication.cpp
 *
 *  Created on: Jun 21, 2010
 */

#include "llvmDuplication.h"
#include <llvm/Transforms/Utils/Cloning.h>

#include "llvmDomination.h"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/ValueMap.h>

using namespace llvm;

namespace rv {

BlockSet splitNode(llvm::BasicBlock *srcBlock, llvm::DominatorTree *domTree) {
  assert(srcBlock && "was NULL");

  BlockVector preds;
  for (auto * BB : predecessors(srcBlock)) {
    preds.push_back(BB);
  }

  BlockSet clones;

  if (preds.size() <= 1)
    return clones;

  if (domTree) {
    llvm::BasicBlock *first = *preds.begin();
    domTree->changeImmediateDominator(srcBlock, first);
  }

  for (BlockVector::iterator itPred = (preds.begin() + 1);
       itPred != preds.end(); ++itPred) {
    llvm::BasicBlock *predBlock = *itPred;
    llvm::BasicBlock *clonedBlock =
        cloneBlockForBranch(srcBlock, predBlock, domTree);
    clones.insert(clonedBlock);
  }

  return clones;
}

BlockVector splitNodeExt(llvm::BasicBlock *srcBlock,
                         BlockSetVector predecessorSet,
                         llvm::DominatorTree *domTree) {
  assert(srcBlock && "was NULL");

  BlockVector clones(predecessorSet.size(), 0);

  if (predecessorSet.size() <= 1)
    return clones;

  // forward to first non-empty branch set
  BlockSetVector::iterator itFirstNonEmpty = predecessorSet.begin();
  BlockVector::iterator itFirstClone = clones.begin();
  for (; itFirstNonEmpty->empty(); ++itFirstNonEmpty, ++itFirstClone) {
    if (itFirstNonEmpty == predecessorSet.end()) {
      return clones;
    }
  }

  // fix dominator information for the first entry (that does not get cloned)
  if (domTree) {
    BlockSet firstSet = *itFirstNonEmpty;
    if (firstSet.size() == 1)
      domTree->changeImmediateDominator(srcBlock, *firstSet.begin());
  }

  // iterate over all remaining branch sets
  BlockVector::iterator itClone = itFirstClone + 1;
  for (BlockSetVector::iterator itSet = itFirstNonEmpty + 1;
       itSet != predecessorSet.end(); ++itSet, ++itClone)
    if (!(*itSet).empty()) {
      *itClone = cloneBlockForBranchSet(srcBlock, *itSet, domTree);
    }

  return clones;
}

LoopSet splitLoop(llvm::LoopInfo &loopInfo, llvm::Loop *loop, llvm::Pass *pass,
                  llvm::DominatorTree *domTree) {
  assert(loop && "was NULL");
  assert(pass && "sorry, somehow llvm::CloneLoop needs as pass object");

  llvm::BasicBlock *header = loop->getHeader();

  BlockVector preds;
  for (auto * BB : predecessors(header)) preds.push_back(BB);// = getAllPredecessors(header);

  LoopSet clones;

  if (preds.size() <= 1)
    return clones;

  if (domTree) {
    llvm::BasicBlock *first = *preds.begin();
    llvm::BasicBlock *header = loop->getHeader();
    domTree->changeImmediateDominator(header, first);
  }

  loop->getHeader();

  for (BlockVector::iterator itPred = (preds.begin() + 1);
       itPred != preds.end(); ++itPred) {
    // llvm::BasicBlock * predBlock = *itPred;

    // FIXME
    // llvm::Loop * clonedLoop = cloneLoopForBranch(lpm, pass, loopInfo, loop,
    // predBlock, domTree);
    // clones.insert(clonedLoop);
  }

  return clones;
}

llvm::BasicBlock *cloneBlockAndMapInstructions(llvm::BasicBlock *block,
                                               ValueMap &cloneMap) {
  llvm::BasicBlock *clonedBlock =
      llvm::CloneBasicBlock(block, cloneMap, "", block->getParent());

  for (Instruction &inst : *clonedBlock) {
    //	llvm::errs() << "remapping inst=" << inst->getName().str() << "\n";
    RemapInstruction(&inst, cloneMap, RF_IgnoreMissingLocals);
  }

  return clonedBlock;
}

// FIXME
/*llvm::Loop * cloneLoopForBranch(BlockCopyTracker & tracker,
llvm::LPPassManager & lpm, llvm::Pass * pass, llvm::LoopInfo & loopInfo,
llvm::Loop * loop, llvm::BasicBlock * branchBlock)
{
        return cloneLoopForBranch(lpm, pass, loopInfo, loop, branchBlock);
}

llvm::Loop * cloneLoopForBranch(llvm::LPPassManager & lpm, llvm::Pass * pass,
llvm::LoopInfo & loopInfo, llvm::Loop * loop, llvm::BasicBlock * branchBlock,
llvm::DominatorTree *domTree)
{
        ValueMap cloneMap;
        llvm::Loop *clonedLoop = llvm::CloneLoop(loop, &lpm, &loopInfo,
                                           cloneMap, pass);
        if (domTree)
                domTree->addNewBlock(clonedLoop->getHeader(), branchBlock);

        patchClonedBlocksForBranch(cloneMap, loop->getBlocks(),
clonedLoop->getBlocks(), branchBlock);

        return clonedLoop;
}*/

//### fix up incoming values ###

void patchClonedBlocksForBranch(ValueMap &cloneMap,
                                const BlockVector &originalBlocks,
                                const BlockVector &clonedBlocks,
                                llvm::BasicBlock *branchBlock) {
  BlockSet branchSet;
  branchSet.insert(branchBlock);
  patchClonedBlocksForBranches(cloneMap, originalBlocks, clonedBlocks,
                               branchSet);
}

void patchClonedBlocksForBranches(ValueMap &cloneMap,
                                  const BlockVector &originalBlocks,
                                  const BlockVector &clonedBlocks,
                                  BlockSet branchBlocks) {
  for (BlockVector::const_iterator itBlock = originalBlocks.begin();
       itBlock != originalBlocks.end(); ++itBlock) {
    llvm::BasicBlock *srcBlock = *itBlock;
    llvm::BasicBlock *clonedBlock =
        llvm::cast<llvm::BasicBlock>(cloneMap[srcBlock]);

    // tracker.identifyBlocks(srcBlock, clonedBlock);

    // Fix all PHI-nodes of the cloned block
    for (llvm::BasicBlock::iterator it = srcBlock->begin();
         it != srcBlock->end() && llvm::isa<llvm::PHINode>(it);) {
      llvm::PHINode *srcPHI = llvm::cast<llvm::PHINode>(it++);

      llvm::PHINode *clonedPHI = llvm::cast<llvm::PHINode>(cloneMap[srcPHI]);

      if (branchBlocks.size() == 1) {
        llvm::BasicBlock *branchBlock = *branchBlocks.begin();

        int blockIdx = clonedPHI->getBasicBlockIndex(branchBlock);

        if (blockIdx > -1) {
#ifdef DEBUG
          llvm::errs() << "spezialising PHI for branch value.phi="
                       << clonedPHI->getName() << "\n";
#endif
          llvm::Value *branchValue = clonedPHI->getIncomingValue(blockIdx);
          // specialize PHI (in cloned block)
          clonedPHI->replaceAllUsesWith(branchValue);
#ifdef DEBUG
          llvm::errs() << "replacing phi" << clonedPHI->getName().str()
                       << " with " << branchValue->getName().str() << "\n";
#endif
          cloneMap[srcPHI] = branchValue;
          clonedPHI->eraseFromParent();

          // remove incoming edge from branchBlock (in source block)
          srcPHI->removeIncomingValue(branchBlock, true);
        }
      } else {
#ifdef DEBUG
        llvm::errs() << "## PHI before\n";
        clonedPHI->dump();
#endif
        for (uint i = 0; i < clonedPHI->getNumIncomingValues(); ++i) {
          llvm::BasicBlock *incBlock = clonedPHI->getIncomingBlock(i);
#ifdef DEBUG
          llvm::errs() << "incoming : " << incBlock->getName() << "\n";
#endif
          auto it = std::find(clonedBlocks.begin(), clonedBlocks.end(), incBlock);
          if (it == clonedBlocks.end() &&
              !branchBlocks.count(incBlock)) {
#ifdef DEBUG
            llvm::errs() << "not from current set! remove";
#endif
            clonedPHI->removeIncomingValue(incBlock, false);
          }
        }
#ifdef DEBUG
        llvm::errs() << "## PHI after\n";
#endif
        clonedPHI->dump();
      }
    }

// Fix all branches coming from branchBlocks
#ifdef DEBUG
    llvm::errs() << "## Patching branchBlocks\n";
#endif
    for (BlockSet::iterator itBranchBlock = branchBlocks.begin();
         itBranchBlock != branchBlocks.end(); ++itBranchBlock) {
      llvm::TerminatorInst *termInst = (*itBranchBlock)->getTerminator();
#ifdef DEBUG
      llvm::errs() << "unpatched:";
      termInst->dump();
#endif
      RemapInstruction(termInst, cloneMap, RF_IgnoreMissingLocals);
#ifdef DEBUG
      llvm::errs() << "patched:";
      termInst->dump();
#endif
    }

#ifdef DEBUG
    llvm::errs() << "## Patching cloned block\n";
#endif
    // Fix all instructions in the block itself
    for (Instruction &inst : *clonedBlock) {
#ifdef DEBUG
      llvm::errs() << "unpatched:";
      inst.dump();
#endif
      RemapInstruction(&inst, cloneMap, RF_IgnoreMissingLocals);
#ifdef DEBUG
      llvm::errs() << "patched:";
      inst.dump();
#endif
    }

    // Repair receiving PHI-nodes
    for (llvm::succ_iterator itSucc = llvm::succ_begin(clonedBlock);
         itSucc != llvm::succ_end(clonedBlock); ++itSucc) {
      llvm::BasicBlock *succBlock = *itSucc;
      ValueSet handledSrcValues;
      llvm::BasicBlock::iterator itPHI;
      for (itPHI = succBlock->begin(); llvm::isa<llvm::PHINode>(itPHI);
           ++itPHI) {
#ifdef DEBUG
        llvm::errs() << "## patching PHI:";
        itPHI->dump();
#endif
        llvm::PHINode *phi = llvm::cast<llvm::PHINode>(itPHI);
        assert(phi->getBasicBlockIndex(clonedBlock) == -1 &&
               "value already mapped!");
        llvm::Value *inVal = phi->getIncomingValueForBlock(srcBlock);
        handledSrcValues.insert(inVal);

#ifdef DEBUG
        llvm::errs() << "### fixed PHI for new incoming edge\n";
        inVal->dump();
#endif
        phi->addIncoming(cloneMap[inVal], clonedBlock);
      }

      // create new PHI-nodes for not handled values
      llvm::BasicBlock::iterator itAfterPHI = itPHI;
      for (ValueMap::const_iterator itClonePair = cloneMap.begin();
           itClonePair != cloneMap.end(); ++itClonePair) {
        llvm::Value *srcVal = const_cast<llvm::Value *>(itClonePair->first);
        llvm::Value *cloneVal = itClonePair->second;

        if (llvm::isa<llvm::Instruction>(srcVal) &&
            !handledSrcValues.count(srcVal)) {
          llvm::Instruction *srcInst = llvm::cast<llvm::Instruction>(srcVal);
          if (srcInst->isUsedInBasicBlock(succBlock)) {
            llvm::PHINode *resolvePHI = llvm::PHINode::Create(
                srcVal->getType(), 2, "intro", cast<Instruction>(itAfterPHI));
            resolvePHI->addIncoming(srcVal, srcBlock);
            resolvePHI->addIncoming(cloneVal, clonedBlock);
            cloneMap[srcVal] = resolvePHI;
          }
        }
      }

      // remap the rest of the block
      for (; itAfterPHI != succBlock->end(); ++itAfterPHI)
        RemapInstruction(cast<Instruction>(itAfterPHI), cloneMap, RF_IgnoreMissingLocals);
    }
  }
}

/*
 * creates a copy of @srcBlock that replaces the original srcBlock on the edge
 * coming from branchBlock
 */
llvm::BasicBlock *cloneBlockForBranch(llvm::BasicBlock *srcBlock,
                                      llvm::BasicBlock *branchBlock,
                                      llvm::DominatorTree *domTree) {
  BlockSet branchSet;
  branchSet.insert(branchBlock);
  return cloneBlockForBranchSet(srcBlock, branchSet, domTree);
}

llvm::BasicBlock *cloneBlockForBranchSet(llvm::BasicBlock *srcBlock,
                                         BlockSet branchSet,
                                         llvm::DominatorTree *domTree) {
#ifdef DEBUG
  llvm::errs() << " cloning block : " << srcBlock->getName().str() << "\n";
               //<< " for blockset : " << toString(branchSet) << "\n";
#endif

  // sanity check
  assert(!srcBlock->getUniquePredecessor() &&
         "block already has only a single predecessor");

  ValueMap cloneMap;

  llvm::BasicBlock *clonedBlock =
      cloneBlockAndMapInstructions(srcBlock, cloneMap);
  cloneMap[srcBlock] = clonedBlock;
  ValueMap branchFixMap;
  // branchFixMap[srcBlock] = clonedBlock;

  // reattach branches
  BlockVector originalBlocks(1, srcBlock);
  BlockVector clonedBlocks(1, clonedBlock);

  patchClonedBlocksForBranches(cloneMap, originalBlocks, clonedBlocks,
                               branchSet);

  // fix up the dominance tree
  if (domTree) {
    if (branchSet.size() == 1) {
      domTree->addNewBlock(clonedBlock, *(branchSet.begin()));
    } else {
      domTree->addNewBlock(
          clonedBlock, findImmediateDominator(*domTree, branchSet)->getBlock());
    }
  }

  return clonedBlock;
}
}
