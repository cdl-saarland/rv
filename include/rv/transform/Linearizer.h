//===- Linearizer.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon
//
// ----------------------------------------------------------------------------
// Partial control linearizer
// Convertes divergent branches to data flow
// ----------------------------------------------------------------------------

#ifndef RV_TRANSFORM_LINEARIZER_H_
#define RV_TRANSFORM_LINEARIZER_H_


#include "rv/vectorizationInfo.h"
#include "rv/Region/Region.h"
#include "rv/analysis/maskAnalysis.h"

#include <llvm/IR/Dominators.h>

#include <vector>
#include <map>
#include <set>

namespace llvm {
  class LoopInfo;
  class Loop;
  class BasicBlock;
  class TerminatorInst;
}

#include "rvConfig.h"

#if 1
#define IF_DEBUG_LIN IF_DEBUG
#else
#define IF_DEBUG_LIN if (false)
#endif


namespace rv {

  typedef std::vector<llvm::BasicBlock*> BlockVector;
  typedef std::map<const llvm::BasicBlock*, unsigned> BlockIndex;
  typedef std::set<const llvm::BasicBlock*> ConstBlockSet;

  class Linearizer {

  // relay logic

    // every block branching to @relayBlock actually need to execute the blocks in @destBlocks next
    // we need to defer these edges to we can schedule linearized blocks in between
    struct RelayNode {
      // branch to this block if you need to execute any of @destBlocks next
      llvm::BasicBlock * block;

      // blocks that will be executed in any case when branching to @block
      // this implies that these blocks will be completely linearized (possible interspread by uniform subgraphs)
      ConstBlockSet destBlocks;

      RelayNode(llvm::BasicBlock * _block, ConstBlockSet _destBlocks)
      : block(_block)
      , destBlocks(_destBlocks)
      {}
    };

    // maps blocks of the CFG to the relay states they are heading
    // This means they have the minimal index by the topologic order of all blocks in @destBlocks of this relay node
    // FIXME use a proper tree structure instead
    std::map<const BasicBlock*, RelayNode*> scheduleHeads; // maps current heads to RelayNodes
    std::map<const BasicBlock*, RelayNode*> relayBlocks;   // maps relay BBs to their RelayNodes

    RelayNode * getRelay(const BasicBlock & head) const {
      auto it = scheduleHeads.find(&head);
      if (it != scheduleHeads.end()) {
        return it->second;
      } else {
        return nullptr;
      }
    }

    void getDestBlocks(const BasicBlock & relayBlock, ConstBlockSet & oDestBlocks) {
      assert(relayBlocks.count(&relayBlock));
      for (auto * block : relayBlocks[&relayBlock]->destBlocks) {
        oDestBlocks.insert(block);
      }
    }

    // return a new schedule head that represents all @destBlocks at the schedule head @head minus @head itself
    // @return the advanced schedule head. Otw, nullptr if @head is not a schedule head or no @destBlocks remain
    // @oRelayBlock will hold the actual basic block if the schedule head was advanced
    RelayNode * advanceScheduleHead(llvm::BasicBlock & head, BasicBlock *& oRelayBlock) {
      auto * oldRelay = getRelay(head);
      if (!oldRelay) {
        oRelayBlock = nullptr;
        return nullptr;
      }

      oRelayBlock = oldRelay->block;

      auto destBlocks = oldRelay->destBlocks;
      destBlocks.erase(&head);

      // free old relay
      scheduleHeads.erase(&head);
      delete oldRelay;

      // create a new relay for any remaining blocks
      if (destBlocks.empty()) return nullptr;
      return &requestRelay(destBlocks);
    }

    RelayNode & requestRelay(ConstBlockSet & destBlocks) {
      unsigned minIdx = blocks.size();
      for (auto * block : destBlocks) {
        minIdx = std::min<>(minIdx, getIndex(*block));
      }
      BasicBlock & head = getBlock(minIdx);

      assert(!finishedBlocks.count(&head) && "relay head already emitted -> relay block will never be linked back");

      IF_DEBUG_LIN {
        errs() << "\t - relay request " << head.getName() << " -> {";
        for (auto * block : destBlocks) {
          if (&head != block) errs() << ", " << block->getName();
        }
        errs() << "}\n";
      }

      auto it = scheduleHeads.find(&head);
      if (it != scheduleHeads.end()) {
        IF_DEBUG_LIN{ errs() << "\treusing relay\n"; }
        // TODO we could optimize the schedule by splitting
        // Consider the case of A-->B and B-->C relay nodes
        // right now we merge these two to A-->B-->C
        // we could instead split B and create to instances A-->B and B'-->C
        // This would avoid executing C whenever we reached B by A
        //
        // We could either we split now or we make the scheduleHead map a n:n relation
        //
      // merge additional destBlocks into existing node
        auto * relNode = it->second;

        for (const auto * block : destBlocks) {
          relNode->destBlocks.insert(block);
        }

        return *it->second;
      }

      auto * relayBlock = BasicBlock::Create(context, "relay_" + head.getName(), &func);
      auto * relay = new RelayNode(relayBlock, destBlocks);
      relayBlocks[relayBlock] = relay;
      scheduleHeads[&head] = relay;

      return *relay;
    }

    ConstBlockSet finishedBlocks;
    RelayNode* emitBlock(llvm::BasicBlock & block);

  // transformations
    // partially linearize a range of blocks in the blockIndex
    uint processRange(uint startId, uint endId, ConstBlockSet mustHaves, llvm::Loop * parentLoop);

    uint processBlock(llvm::BasicBlock & regionHead, ConstBlockSet mustHaves, llvm::Loop * parentLoop);

    uint processLoop(llvm::BasicBlock & loopHead, ConstBlockSet mustHaves);

    bool needsFolding(llvm::TerminatorInst & branch);
    void processBranch(llvm::BasicBlock & block, ConstBlockSet mustHaves, llvm::Loop * parentLoop);

  // loop transofmr
    RelayNode & convertToSingleExitLoop(Loop & loop, ConstBlockSet mustHaves);
    void dropLoopExit(BasicBlock & block, Loop & loop);

  // analysis structures
    VectorizationInfo & vecInfo;
    MaskAnalysis & maskAnalysis;
    Region * region;
    llvm::DominatorTree & dt;
    llvm::LoopInfo & li;
    Function & func;
    LLVMContext & context;

  // region support
    bool inRegion(const BasicBlock & block) const { return !region || region->contains(&block); }

  // topological sorted blocks in the region
    BlockIndex blockIndex;
    BlockVector blocks;
    void addToBlockIndex(llvm::BasicBlock & block);

    inline unsigned getIndex(const llvm::BasicBlock & block) const { return blockIndex.at(&block); }
    BasicBlock & getBlock(unsigned i) const { assert(i < blocks.size()); return *blocks[i]; }

  // passes
    // topo sort basic blocks in function
    void buildBlockIndex();

    // verify the block index integrity
    // a) the block index is a topological sorting of the regions blocks.
    // b) the index range of loop blocks must be tight (there should be not blocks in the range that do not belong to the loop)
    void verifyBlockIndex();
    void verifyLoopIndex(llvm::Loop & loop);


    // linearize all divergent control
    void linearizeControl();

    // simplify the cfg again
    void cleanup();

    // verify that
    // a) all divergent branches have been folded
    // b) the function, domTree and loop tree are consistent
    void verify();

    std::set<BasicBlock*> finalizedBlocks;

  // mask reduction helper
    Instruction & createReduction(Value & pred, const std::string & name, BasicBlock & atEnd);
    Function * requestReductionFunc(llvm::Module & mod, const std::string & name);

  public:
    Linearizer(VectorizationInfo & _vecInfo, MaskAnalysis & _maskAnalysis, llvm::DominatorTree & _dt, llvm::LoopInfo & _li)
    : vecInfo(_vecInfo)
    , maskAnalysis(_maskAnalysis)
    , region(vecInfo.getRegion())
    , dt(_dt)
    , li(_li)
    , func(vecInfo.getScalarFunction()) // TODO really always our target?
    , context(func.getContext())
    {}

    void run();
  };


}

#endif // RV_TRANSFORM_LINEARIZER_H_
