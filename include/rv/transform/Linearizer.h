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
#include <unordered_map>

namespace llvm {
  class LoopInfo;
  class Loop;
  class BasicBlock;
  class TerminatorInst;
}

namespace rv {
  typedef std::unordered_map<const llvm::BasicBlock*, int> BlockIndex;
  typedef std::pair<llvm::BasicBlock*, llvm::BasicBlock*> Edge;
  typedef llvm::DenseMap<Edge, llvm::Value*> EdgeMaskCache;

  // internal helper class that tunnels values leaving on divergent loop exits through tracker PHI nodes
  class LiveValueTracker;

  class Linearizer {

  // relay logic
    // every block branching to @relayBlock actually need to execute the blocks in @destBlocks next
    // we need to defer these edges to we can schedule linearized blocks in between
    struct RelayNode {
      // the next destionation for every branch going to this relaynode
      llvm::BasicBlock & head;

      // head block id
      int id;

      // branch to this block if you need to execute any of @destBlocks next
      llvm::BasicBlock * block;

      // blocks that will be executed in any case when branching to @block
      // this implies that these blocks will be completely linearized (possible interspread by uniform subgraphs)
      RelayNode * next;

      RelayNode(llvm::BasicBlock & _head, int _id)
      : head(_head)
      , id (_id)
      , block(nullptr)
      , next(nullptr)
      {}

      void enable(BasicBlock & relayBlock, RelayNode * _tail) {
        block = &relayBlock; next = _tail;
      }

      void finalize() { block = nullptr; }

      bool isActive() const { return block != nullptr; }
    };

    RelayNode & createRelay(int headId, RelayNode * tail) {
      auto & head = relays[headId].head;

      auto * relayBlock = BasicBlock::Create(context, "relay_" + head.getName(), &func);
      relays[headId].enable(*relayBlock, tail);
      return relays[headId];
    }

    std::vector<RelayNode> relays; // map target blocks to relay nodes

    RelayNode * getRelay(int blockId) {
      auto & blockRelay =  relays[blockId];
      return blockRelay.isActive() ? &blockRelay : nullptr;
    }

    const RelayNode * getRelay(int blockId) const {
      const auto & blockRelay =  relays[blockId];
      return blockRelay.isActive() ? &blockRelay : nullptr;
    }

    void dumpRelayChain(int headId) const {
      errs() << "chain at " << getBlock(headId).getName() << " {";
      auto * relay = getRelay(headId);
      while (relay) {
        if (relay->id != headId) {
          errs() << " -> " << relay->head.getName();
        }
        relay = relay->next;
      }
      errs() << "}";
    }


    // @return the advanced schedule head. Otw, nullptr if @head is not a schedule head or no @destBlocks remain
    // @oRelayBlock will hold the actual basic block if the schedule head was advanced
    RelayNode * advanceScheduleHead(int headId, BasicBlock *& oRelayBlock) {
      auto * oldRelay = getRelay(headId);
      if (!oldRelay) {
        oRelayBlock = nullptr;
        return nullptr;
      }

      oRelayBlock = oldRelay->block;

      // free old relay (this unliks oldRelay from @oRelayBlock)
      auto * nextRelay = oldRelay->next;
      oldRelay->finalize(); // delete oldRelay;

      // create a new relay for any remaining blocks
      return nextRelay; // &requestRelay(destBlocks);
    }

    // relay chain merging
    RelayNode * mergeRelays(RelayNode * a, RelayNode * b) {
      if (a->id < b->id) {
        if (!a->next) {
          // we reached the tail of a
          a->next = b;

        } else {
          // we need to interleave here
          auto * tailHead = mergeRelays(a->next, b);
          a->next = tailHead;
        }
      } else if (a->id > b->id) {
        return mergeRelays(b, a);
      } else {
        // return a;
      }

      return a;
    }

    // merges the chain starting at @targetId into the chain defined by @headRelay
    // creates a new single element chain for @targetId if there was none before
    // if there is no @headRelay, returns the chain for @targetId
    RelayNode & addTargetToRelay(RelayNode * headRelay, int targetId) {
      auto * targetRelay = getRelay(targetId);
      if (!targetRelay) {
        targetRelay = &createRelay(targetId, nullptr);
      }

      if (!headRelay) {
        return *targetRelay;
      }

      return *mergeRelays(headRelay, targetRelay);
    }

    bool needsFolding(llvm::TerminatorInst & branch);

  // transformations
    // partially linearize a range of blocks in the blockIndex
    int processRange(int startId, int endId, llvm::Loop * parentLoop);

    // process the terminator in @head subjecting all sucessors to @exitRelay
    // if @headId is the header of a loop transform the entire loop
    int processBlock(int headId, llvm::Loop * parentLoop);

    // process all blocks and branches in the loop
    // if that loop is divergent, processLoop will make it uniform before processing its body
    int processLoop(int headId, Loop * loop);

    // update the relays for all branch targets of @block, linking them to @exitRelay
    void processBranch(llvm::BasicBlock & block, RelayNode * exitRelay, llvm::Loop * parentLoop);

    // emit the block at @targetId
    // any branches to the (optional) relay block for @targetId will be linked to the real block
    // will erase the relay block for @target Id and finalize its RelayNode
    RelayNode* emitBlock(int targetId);

    // add undef inputs to PHINodes for all predecessors of @block that do not occur in the PHINodes' block lists
    void addUndefInputs(BasicBlock & block);

  // phi -> select conversion
    // we invalidate mask analysis's and track edge masks on our own
    EdgeMaskCache edgeMasks;
    llvm::Value * getEdgeMask(llvm::BasicBlock & start, llvm::BasicBlock & dest) { return edgeMasks[Edge(&start, &dest)]; }
    void setEdgeMask(llvm::BasicBlock & start, llvm::BasicBlock & dest, llvm::Value * val) { edgeMasks[Edge(&start, &dest)] = val; }

    // will replace all Phis in @block with select insts using edge masks
    bool needsFolding(llvm::PHINode & phi);
    void foldPhis(llvm::BasicBlock & block);

  // divergent loop transform
    // creates a single exiting edge at the latch
    // adds all old exit blocks as relay targets for the new single exit block
    RelayNode & convertToSingleExitLoop(Loop & loop, RelayNode * exitRelay);

    // removes all exiting control edges from @block out of @loop
    void dropLoopExit(BasicBlock & block, Loop & loop);

    // make @inst defined in @destBlock by adding PHI nodes with incoming undef edges
    llvm::Value & promoteDefinition(llvm::Value & inst, llvm::Value & defaultDef, int defBlockId, int destBlockId);

  // analysis structures
  protected:
    friend class LiveValueTracker;

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
    void addToBlockIndex(llvm::BasicBlock & block);

    inline int getIndex(const llvm::BasicBlock & block) const { return blockIndex.at(&block); }
    BasicBlock & getBlock(unsigned i) { assert(i < relays.size()); return relays[i].head; }
    const BasicBlock & getBlock(unsigned i) const { assert(i < relays.size()); return relays[i].head; }
    int getNumBlocks() const { return (int) relays.size(); }

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
