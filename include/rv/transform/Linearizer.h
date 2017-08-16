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
#include "rv/region/Region.h"

#include <llvm/IR/Dominators.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/IR/Function.h>

#include <vector>
#include <unordered_map>

#include <llvm/IR/CFG.h>
#include<llvm/ADT/PostOrderIterator.h>

typedef llvm::ReversePostOrderTraversal<llvm::Function*> RPOT;

namespace llvm {
  class PHINode;
  class LoopInfo;
  class Loop;
  class BasicBlock;
  class TerminatorInst;
}

namespace rv {
  typedef std::unordered_map<const llvm::BasicBlock*, int> BlockIndex;
  typedef std::pair<llvm::BasicBlock*, llvm::BasicBlock*> Edge;
  typedef llvm::DenseMap<Edge, llvm::WeakVH> EdgeMaskCache;
  class MaskExpander;

  // internal helper class that tunnels values leaving on divergent loop exits through tracker PHI nodes
  class LiveValueTracker;

  // internal helper class for partially folded incoming blocks of PHI nodes
  class SuperInput;

  class Linearizer {

  // block index helper
  void scheduleLoop(llvm::Loop * loop, std::string padStr, RPOT::rpo_iterator itStart, RPOT::rpo_iterator itEnd);
  void scheduleDomRegion(llvm::BasicBlock * domEntry, llvm::Loop * loop, std::string padStr, RPOT::rpo_iterator itStart, RPOT::rpo_iterator itEnd);

  // statistics
      // # kept phi incoming values
      size_t numUniformAssignments;
      // # num of super blocks before partially linearized phi nodes
      size_t numPreservedAssignments;
      // # phi incoming values folded into selects
      size_t numFoldedAssignments;
      // number of schedule heads that had to be diverted
      size_t numDivertedHeads;
      // number of lost returns
      size_t numDelayedReturns;
      // number of folded branches
      size_t numFoldedBranches;
      // number of preserved uniform branches
      size_t numPreservedBranches;
      // number of uniform loops
      size_t numUniformLoops;
      // number of select instructions
      size_t numBlends;
      // number of simplified blends (including pre-existing blends)
      size_t numSimplifiedBlends;

  // relay logic
    // we need to defer these edges to we can schedule linearized blocks in between
    struct RelayNode {
      // the next destionation for every branch going to this relaynode
      llvm::BasicBlock & head;

      // block index number
      int id;

      // branch to this block if you need to execute any of @destBlocks next
      llvm::BasicBlock * block;

      // blocks that will be executed in any case when branching to @block
      // this implies that these blocks will be completely linearized (possible interspread by uniform subgraphs)
      RelayNode * next;

      // do phi nodes in this block need to be folded?
      bool needsPhiFolding;

      // a set of some blocks that will reach this block
      llvm::SmallPtrSet<llvm::BasicBlock*, 4> reachingBlocks;

      RelayNode(llvm::BasicBlock & _head, int _id)
      : head(_head)
      , id (_id)
      , block(nullptr)
      , next(nullptr)
      , needsPhiFolding(false)
      , reachingBlocks()
      {}

      // whether there is a post dominance constraint on this block
      bool empty() {
        return next == nullptr;
      }

      void addReachingBlock(llvm::BasicBlock & reachingBlock) {
        reachingBlocks.insert(&reachingBlock);
      }

      void enable(llvm::BasicBlock & relayBlock, RelayNode * _tail) {
        block = &relayBlock; next = _tail;
      }

      void finalize() { block = nullptr; }

      bool isActive() const { return block != nullptr; }
    };

    decltype(RelayNode::reachingBlocks)&
    getReachingBlocks(int blockId) {
      return relays[blockId].reachingBlocks;
    }

    RelayNode & createRelay(int headId, RelayNode * tail) {
      auto & head = relays[headId].head;

      assert(!relays[headId].block && "overwriting exiting relay block!");

      auto * relayBlock = llvm::BasicBlock::Create(context, "relay_" + head.getName(), &func);
      relays[headId].enable(*relayBlock, tail);
      return relays[headId];
    }

    std::vector<RelayNode> relays; // maps block index numbers to relay nodes

    RelayNode * getRelay(int blockId) {
      auto & blockRelay =  relays[blockId];
      return blockRelay.isActive() ? &blockRelay : nullptr;
    }

    const RelayNode * getRelay(int blockId) const {
      const auto & blockRelay =  relays[blockId];
      return blockRelay.isActive() ? &blockRelay : nullptr;
    }

    void dumpRelayChain(int headId) const {
      llvm::errs() << "head " << getBlock(headId).getName();
      auto * relay = getRelay(headId);

      if (!relay) {
        llvm::errs() << " chain {}";
        return;
      }

      // dump all reaching blocks
      if (!relay->reachingBlocks.empty()) {
        bool first = true;
        llvm::errs() << " reaching [";
        for (auto * reachBlock : relay->reachingBlocks) {
          if (!first) llvm::errs() << ", ";
          llvm::errs() << reachBlock->getName();
          first = false;
        }
        llvm::errs() << "] ";
      }
      // dump chain
      llvm::errs() << " chain {";
      while (relay) {
        if (relay->id != headId) {
          llvm::errs() << " -> " << relay->head.getName();
        }
        relay = relay->next;
      }
      llvm::errs() << "}";
    }


    // @return the advanced schedule head. Otw, nullptr if @head is not a schedule head or no @destBlocks remain
    // @oRelayBlock will hold the actual basic block if the schedule head was advanced
    RelayNode * advanceScheduleHead(int headId, llvm::BasicBlock *& oRelayBlock) {
      auto * oldRelay = getRelay(headId);
      if (!oldRelay) {
        oRelayBlock = nullptr;
        return nullptr;
      }

      oRelayBlock = oldRelay->block;

      // free old relay (this unliks oldRelay from @oRelayBlock)
      auto * nextRelay = oldRelay->next;
      oldRelay->finalize();

      // create a new relay for any remaining blocks
      return nextRelay;
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

    // merge all reaching blocks to source into dest
    RelayNode & getRelayUnchecked(uint i) { return relays[i]; }
    void mergeInReaching(RelayNode & dest, RelayNode & source);

    bool needsFolding(llvm::TerminatorInst & branch);

  // transformations
    // partially linearize a range of blocks in the blockIndex
    int processRange(int startId, int endId, llvm::Loop * parentLoop);

    // process the terminator in @head subjecting all sucessors to @exitRelay
    // if @headId is the header of a loop transform the entire loop
    int processBlock(int headId, llvm::Loop * parentLoop);

    // process all blocks and branches in the loop
    // if that loop is divergent, processLoop will make it uniform before processing its body
    int processLoop(int headId, llvm::Loop * loop);

    // update the relays for all branch targets of @block, linking them to @exitRelay
    void processBranch(llvm::BasicBlock & block, RelayNode * exitRelay, llvm::Loop * parentLoop);

    // emit the block at @targetId
    // any branches to the (optional) relay block for @targetId will be linked to the real block
    // will erase the relay block for @target Id and finalize its RelayNode
    RelayNode* emitBlock(int targetId);

    // add undef inputs to PHINodes for all predecessors of @block that do not occur in the PHINodes' block lists
    void addUndefInputs(llvm::BasicBlock & block);

  // phi -> select conversion aka phi folding
    // we invalidate mask analysis's and track edge masks on our own
    void cacheMasks();

    // whether @block contains a phi node that existed in the original program
    bool containsOriginalPhis(llvm::BasicBlock & block);

    // will replace all Phis in @block with select insts using edge masks
    bool needsFolding(llvm::PHINode & phi);
    void foldPhis(llvm::BasicBlock & block);

  // edge masks
    // these are cached before the transformation so we can query them eventhough the CFG changes
    EdgeMaskCache edgeMasks; // if true for an edge A->B control proceeds from block A to block B both being executed
    llvm::Value * getEdgeMask(llvm::BasicBlock & start, llvm::BasicBlock & dest) {
      Edge edge(&start, &dest);
      auto it = edgeMasks.find(edge);
      if (it == edgeMasks.end())
        return nullptr;
      else return it->second;
    }

    void setEdgeMask(llvm::BasicBlock & start, llvm::BasicBlock & dest, llvm::Value * val) {
      if (!val) {
        edgeMasks.erase(Edge(&start, &dest));
        return;
      }
      assert(val->getType() == llvm::Type::getInt1Ty(val->getContext()) && "expected i1 mask type");
      edgeMasks[Edge(&start, &dest)] = val;
    }

  // SSA repair
    // \brief make @inst defined in @destBlock by adding PHI nodes with incoming undef edges
    llvm::Value & promoteDefToBlock(llvm::BasicBlock & block, llvm::SmallVector<llvm::Value*, 16> & defs, llvm::Value & defaultDef, int defBlockId, int blockId, VectorShape instShape);

    /// \brief promotes a definition in indexed blocks: the definition in @defBlockId is promoted to @destBlockId
    //  defaults to @defaultDef whenever @inst does not dominate a block
    llvm::Value & promoteDefinition(llvm::Value & inst, llvm::Value & defaultDef, int defBlockId, int destBlockId);
    /// \brief promoted a definition from the indexed block @defBlockId to the (possibly) non-indexed block @userBlock
    //  the userBlock is expected to have only indexed predecessors
    llvm::Value & promoteDefinition(llvm::Value & inst, llvm::Value & defaultDef, int defBlockId, llvm::BasicBlock & userBlock);

    /// \brief promotes a definition from @defBlockId to @blockId (returns intermediate definitions on the interval between @defBlockId an @destBlockID
    llvm::Value & promoteDefinitionExt(llvm::SmallVector<llvm::Value*, 16> & defs, llvm::Value & inst, llvm::Value & defaultDef, int defBlockId, int destBlockId);

    llvm::Value * createSuperInput(llvm::PHINode & phi, SuperInput & superInput);

  // analysis structures
  protected:
    friend class LiveValueTracker;

    VectorizationInfo & vecInfo;
    MaskExpander & maskEx;
    Region * region;
    llvm::DominatorTree & dt;
    llvm::LoopInfo & li;
    llvm::Function & func;
    llvm::LLVMContext & context;

  // region support
    bool inRegion(const llvm::BasicBlock & block) const { return !region || region->contains(&block); }

  // topological sorted blocks in the region
    BlockIndex blockIndex;
    void addToBlockIndex(llvm::BasicBlock & block);

    inline bool hasIndex(const llvm::BasicBlock & block) const { return blockIndex.count(&block); }
    inline int getIndex(const llvm::BasicBlock & block) const { return blockIndex.at(&block); }
    /// the highest index of @block s predecessors
    int getLeastIndex(const llvm::BasicBlock & block) const;
    llvm::BasicBlock & getBlock(unsigned i) { assert(i < relays.size()); return relays[i].head; }
    const llvm::BasicBlock & getBlock(unsigned i) const { assert(i < relays.size()); return relays[i].head; }
    int getNumBlocks() const { return (int) relays.size(); }

  // passes
    // topo sort basic blocks in function
    void buildBlockIndex();

    // verify the block index integrity
    // a) the block index is a topological sorting of the regions blocks.
    // b) the index range of loop blocks must be tight (there should be not blocks in the range that do not belong to the loop)
    void verifyBlockIndex();
    void verifyLoopIndex(llvm::Loop & loop);
    void verifyCompactDominance(llvm::BasicBlock & head);

    // linearize all divergent control
    void linearizeControl();

    // simplify the cfg again
    void cleanup();

    // verify that
    // a) all divergent branches have been folded
    // b) the function, domTree and loop tree are consistent
    void verify();

  // late SSA repair support
    // each incoming block and value of a repairPHI represents a definition of the repairPHI
    llvm::PHINode & createRepairPhi(llvm::Value & val, llvm::BasicBlock & destBlock);
    llvm::PHINode & createRepairPhi(llvm::Value & val, llvm::IRBuilder<> & builder);

    llvm::DenseSet<llvm::PHINode*> repairPhis;
    bool isRepairPhi(llvm::PHINode & val) const { return repairPhis.count(&val); }

    // repair the data flow graph denoted by repairPhis
    // we run SSA repair with these definitions and replace all uses of the repairPhi with the new value
    void resolveRepairPhis();

  // re-establish SSA form by inserting phis + undef
    void fixSSA();

  // simplify blend code
    size_t simplifyBlends();
  public:
    Linearizer(VectorizationInfo & _vecInfo, MaskExpander & _maskEx, llvm::DominatorTree & _dt, llvm::LoopInfo & _li)
    : numUniformAssignments(0)
    , numPreservedAssignments(0)
    , numFoldedAssignments(0)
    , numDivertedHeads(0)
    , numDelayedReturns(0)
    , numFoldedBranches(0)
    , numPreservedBranches(0)
    , numUniformLoops(0)
    , numBlends(0)
    , numSimplifiedBlends(0)

    , vecInfo(_vecInfo)
    , maskEx(_maskEx)
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
