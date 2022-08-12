//===---- utils/llvmDuplication.cpp - Convenient BB Cloning -----*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "llvmDuplication.h"
#include <llvm/Transforms/Utils/Cloning.h>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/ValueMap.h>
#include <llvm/Transforms/Utils/SSAUpdater.h>
#include <llvm/IR/IRBuilder.h>

using namespace llvm;

#if 0
#undef IF_DEBUG_CNS
#define IF_DEBUG_CNS if (true)
#else
#define IF_DEBUG_CNS if (false)
#endif

// #define CNS_SSA_UPDATER // TODO did not work (defaulting to Store+Load, folloed by mem2reg)

static void
simplifyPhis(BasicBlock & block, BasicBlock & predBlock) {
  // Fix all PHI-nodes of the cloned block
  // FIXME don't do this for loop carries..
  for (llvm::BasicBlock::iterator it = block.begin();
       it != block.end() && llvm::isa<llvm::PHINode>(it);) {
    llvm::PHINode *phi = llvm::cast<llvm::PHINode>(it++);

    auto * liveIn = phi->getIncomingValueForBlock(&predBlock);
    assert(liveIn && "pred is not incoming block!");
    // turn into a sinle-input phi
    for (size_t i = 0; i < phi->getNumIncomingValues();) {
      if (phi->getIncomingBlock(i) != &predBlock) {
        phi->removeIncomingValue(i, false);
      } else {
        ++i;
      }
    }
  }
}

static Value*
ExtractVal(Value * val) {
  auto * phi = dyn_cast<PHINode>(val);
  if (phi && phi->getNumIncomingValues() == 1) return phi->getIncomingValue(0);
  return val;
}

namespace rv {

BlockSet splitNode(llvm::BasicBlock *srcBlock, llvm::DominatorTree *domTree) {
  assert(srcBlock && "was NULL");

  IF_DEBUG_CNS {
    errs() << "Splitting " << srcBlock->getName() << "\n";
    errs() << *srcBlock << "\n";
  }

  BlockVector preds;
  for (auto * BB : predecessors(srcBlock)) {
    preds.push_back(BB);
  }

  BlockSet succSet;
  for (auto * succ : successors(srcBlock)) {
    succSet.insert(succ);
  }

  BlockSet clones;

  if (preds.size() <= 1)
    return clones;

  if (domTree) {
    llvm::BasicBlock *first = *preds.begin();
    domTree->changeImmediateDominator(srcBlock, first);
  }

  // clone all blocks
  std::map<BasicBlock*,ValueToValueMapTy*> cloneMap;

  for (BlockVector::iterator itPred = (preds.begin() + 1);
       itPred != preds.end(); ++itPred) {
    llvm::BasicBlock *predBlock = *itPred;
    llvm::BasicBlock *clonedBlock = cloneBlockForBranch(srcBlock, predBlock, cloneMap, domTree);
    clones.insert(clonedBlock);
    simplifyPhis(*clonedBlock, *predBlock);
  }

  simplifyPhis(*srcBlock, *preds[0]);



  IF_DEBUG_CNS {
    errs() << "--- predecessors after CFG embedding ---\n";
    for (auto * p : preds) {
      errs() << *p << "\n";
    }

    errs() << "--- cloned blocks after CFG embedding ---\n";
    errs() << *srcBlock << "\n";
    for (auto * clone : clones) {
      errs() << *clone << "\n";
    }
  }

  IF_DEBUG_CNS errs() << "-- fixing live out uses ---\n";

  // fix remote live out uses
  for (auto & inst : *srcBlock) {
    if (inst.getType()->isVoidTy()) continue;
    if (inst.getNumUses() == 0) continue;

    IF_DEBUG_CNS errs() << "Fixing uses of " << inst << "\n";

    bool ssaUpReady = false;
#ifdef CNS_SSA_UPDATER
    SmallVector<PHINode*, 8> phiVec;
    SSAUpdater ssaUpdater(&phiVec);
    ssaUpdater.Initialize(inst.getType(), inst.getName().str() + ".cnsphi");
#else
    AllocaInst * location = nullptr;
#endif

    IF_DEBUG_CNS errs() << "Definition : " << inst << "   @" << srcBlock->getName() << "\n";
    std::vector<Use*> cachedUses;
    for (auto & use : inst.uses()) {
      cachedUses.push_back(&use);
    }
    for (auto * useP : cachedUses) {
      auto & use = *useP;
      auto & userInst = *cast<Instruction>(use.getUser());

      if (userInst.getParent() == srcBlock) continue; // self loops etc should be patched already

      IF_DEBUG_CNS errs() << "USER : " << *use.getUser() << "    @" << userInst.getParent()->getName() << "\n";
      auto * userPhi = dyn_cast<PHINode>(use.getUser());

      // there is a pre-existing receiving phi in the successor block
      if (userPhi && succSet.count(userPhi->getParent())) {
        IF_DEBUG_CNS errs() << "\t successor phi case:\n";
        int recIdx = userPhi->getBasicBlockIndex(srcBlock);
        assert(recIdx >= 0);
        assert(userPhi->getIncomingValue(recIdx) == &inst && "cross use");

        // userPhi->removeIncomingValue(recIdx, false);

        for (auto itMap : cloneMap) {
          auto * cloneBlock = itMap.first;
          auto * clonedVal = &*(*itMap.second)[&inst];
          userPhi->addIncoming(clonedVal, cloneBlock);
        }

      } else {
        if (!ssaUpReady) {
#ifdef CNS_SSA_UPDATER
#else
          auto * func = srcBlock->getParent();
          location = new AllocaInst(inst.getType(), 0, "cns.stash", func->begin()->getFirstNonPHI());
#endif
          IF_DEBUG_CNS errs() << "SSAUpdater setup:\n";
          for (auto itMap : cloneMap) {
            auto * cloneBlock = itMap.first;
            auto * clonedInst = cast<Instruction>(&*(*itMap.second)[&inst]);
            auto * clonedVal = ExtractVal(clonedInst);
            assert(clonedVal && "cloned val not mapped!");
            IF_DEBUG_CNS errs() << "\t\t\t" << *clonedVal << "   @" << cloneBlock->getName() << "\n";
#ifdef CNS_SSA_UPDATER
            ssaUpdater.AddAvailableValue(cloneBlock, clonedVal);
#else
            IRBuilder<> builder(clonedInst->getParent(), clonedInst->getParent()->getTerminator()->getIterator());
            builder.CreateStore(clonedVal, location, false);
#endif
          }
          IF_DEBUG_CNS errs() << "\t\t\t" << *ExtractVal(&inst) << "   @" << srcBlock->getName() << "\n";
#ifdef CNS_SSA_UPDATER
          ssaUpdater.AddAvailableValue(srcBlock, ExtractVal(&inst));
#else
          IRBuilder<> builder(inst.getParent(), inst.getParent()->getTerminator()->getIterator());
          builder.CreateStore(ExtractVal(&inst), location, false);
#endif
          ssaUpReady = true;
        }

#if 0
        ssaUpdater.RewriteUse(use);
#else
        Value * fixedDef = nullptr;
        if (!userPhi) {
          // non-phi request def in middle of block (creating phi nodes)
          IF_DEBUG_CNS errs() << "\t inst user case:\n";

#ifdef CNS_SSA_UPDATER
          fixedDef = ssaUpdater.GetValueInMiddleOfBlock(userInst.getParent());
          assert(ssaUpdater.HasValueForBlock(userInst.getParent()));
#else
          IRBuilder<> builder(userInst.getParent(), userInst.getIterator());
          fixedDef = builder.CreateLoad(inst.getType(), location, "cns.2reg");
#endif

        } else {
          IF_DEBUG_CNS errs() << "\t remote phi user case:\n";
          // using phi request definition at incoming block
          int valIdx = userPhi->getIncomingValueNumForOperand(use.getOperandNo());
          auto * inBlock = userPhi->getIncomingBlock(valIdx);
          IF_DEBUG_CNS {
            errs() << "incoming value at block " << inBlock->getName() << "\n";
            for (auto * predBlock: predecessors(inBlock)) {
              errs() << "- " << predBlock->getName() << "\n";
            }
          }

#ifdef CNS_SSA_UPDATER
          fixedDef = ssaUpdater.GetValueAtEndOfBlock(inBlock);
          assert(ssaUpdater.HasValueForBlock(inBlock));
#else
          IRBuilder<> builder(inBlock, inBlock->getTerminator()->getIterator());
          fixedDef = builder.CreateLoad(inst.getType(), location, "cns.2reg");
#endif
        }

        userInst.setOperand(use.getOperandNo(), fixedDef);
#endif
      }

      IF_DEBUG_CNS errs() << "fixed USER : " << *use.getUser() << "\n";
    }
  }

#if 0
    IF_DEBUG_CNS errs() << "@" << srcBlock->getName() << " : " << inst << "\n";
    for (auto * clone : clones) {
      auto * cloneVal = &*(*cloneMap[clone])[&inst];
      IF_DEBUG_CNS errs() << "@" << clone->getName() << " : " << *cloneVal << "\n";
    }
#endif

  for (auto it : cloneMap) delete it.second;

  return clones;
}

llvm::BasicBlock *cloneBlockAndMapInstructions(llvm::BasicBlock *block,
                                               ValueToValueMapTy &cloneMap) {
  llvm::BasicBlock *clonedBlock =
      llvm::CloneBasicBlock(block, cloneMap, ".cns", block->getParent());

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

void patchClonedBlocksForBranch(ValueToValueMapTy &cloneMap,
                                const BlockVector &originalBlocks,
                                const BlockVector &clonedBlocks,
                                llvm::BasicBlock *branchBlock) {
  for (BlockVector::const_iterator itBlock = originalBlocks.begin();
       itBlock != originalBlocks.end(); ++itBlock) {
    llvm::BasicBlock *srcBlock = *itBlock;
    llvm::BasicBlock *clonedBlock =
        llvm::cast<llvm::BasicBlock>(cloneMap[srcBlock]);

// Fix all branches coming from branchBlocks
    IF_DEBUG_CNS llvm::errs() << "## Patching branchBlocks\n";
      auto *termInst = branchBlock->getTerminator();
      IF_DEBUG_CNS {
        llvm::errs() << "unpatched:";
        termInst->dump();
      }
      RemapInstruction(termInst, cloneMap, RF_IgnoreMissingLocals);
      IF_DEBUG_CNS {
        llvm::errs() << "patched:";
        termInst->dump();
      }

    IF_DEBUG_CNS llvm::errs() << "## Patching cloned block\n";
    // Fix all instructions in the block itself
    for (Instruction &inst : *clonedBlock) {
      IF_DEBUG_CNS {
        llvm::errs() << "unpatched:";
        inst.dump();
      }
      RemapInstruction(&inst, cloneMap, RF_IgnoreMissingLocals);
      IF_DEBUG_CNS {
        llvm::errs() << "patched:";
        inst.dump();
      }
    }
  }
}

/*
 * creates a copy of @srcBlock that replaces the original srcBlock on the edge
 * coming from branchBlock
 */
llvm::BasicBlock *cloneBlockForBranch(llvm::BasicBlock *srcBlock,
                                      llvm::BasicBlock *branchBlock,
                                      std::map<BasicBlock*,ValueToValueMapTy*> & unifiedCloneMap,
                                      llvm::DominatorTree *domTree) {
  IF_DEBUG_CNS {
    llvm::errs() << " cloning block : " << srcBlock->getName().str() << "\n";
               //<< " for blockset : " << toString(branchSet) << "\n";
  }

  // sanity check
  assert(!srcBlock->getUniquePredecessor() &&
         "block already has only a single predecessor");

  auto * cloneMap = new ValueToValueMapTy;

  llvm::BasicBlock *clonedBlock = cloneBlockAndMapInstructions(srcBlock, *cloneMap);
  (*cloneMap)[srcBlock] = clonedBlock;
  ValueToValueMapTy branchFixMap;
  // branchFixMap[srcBlock] = clonedBlock;

  // reattach branches
  BlockVector originalBlocks(1, srcBlock);
  BlockVector clonedBlocks(1, clonedBlock);

  patchClonedBlocksForBranch(*cloneMap, originalBlocks, clonedBlocks, branchBlock);

  // fix up the dominance tree
  if (domTree) {
    domTree->addNewBlock(clonedBlock, branchBlock);
  }

  unifiedCloneMap[clonedBlock] = cloneMap;

  return clonedBlock;
}
}
