//===- vectorizationInfo.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef INCLUDE_RV_VECTORIZATIONINFO_H_
#define INCLUDE_RV_VECTORIZATIONINFO_H_

namespace llvm {
  class LLVMContext;
  class BasicBlock;
  class Instruction;
  class Value;
}


#include "rv/shape/vectorShape.h"
#include "rv/vectorMapping.h"
#include "region/Region.h"

#include "llvm/IR/ValueHandle.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Analysis/LoopInfo.h"

#include <unordered_map>
#include <set>

namespace rv
{

class Region;

// provides vectorization information (vector shapes, block predicates) for a function
class VectorizationInfo
{
    const llvm::DataLayout & DL;
    VectorMapping mapping;

    std::unordered_map<const llvm::BasicBlock*, llvm::TrackingVH<llvm::Value>> predicates;
    std::unordered_map<const llvm::Value*, VectorShape> shapes;

    // detected divergent loops
    std::set<const llvm::Loop*> mDivergentLoops;

    // detected divergent loop exits
    std::set<const llvm::BasicBlock*> DivergentLoopExits;

    std::set<const llvm::BasicBlock*> JoinDivergentBlocks;

    Region* region;

    // fixed shapes (will be preserved through VA)
    std::set<const llvm::Value*> pinned;

public:
    const llvm::DataLayout & getDataLayout() const;

    // disjoin path divergence
    bool isJoinDivergent(const llvm::BasicBlock & JoinBlock) const { return JoinDivergentBlocks.count(&JoinBlock); }
    bool addJoinDivergentBlock(const llvm::BasicBlock& JoinBlock) { return JoinDivergentBlocks.insert(&JoinBlock).second; }

    // loop divergence
    bool addDivergentLoop(const llvm::Loop & divLoop);
    void removeDivergentLoop(const llvm::Loop & divLoop);
    bool isDivergentLoop(const llvm::Loop& loop) const;
    bool isDivergentLoopTopLevel(const llvm::Loop& loop) const;

    // loop exit divergence
    bool isDivergentLoopExit(const llvm::BasicBlock & block) const;
    bool isKillExit(const llvm::BasicBlock & block) const { return !isDivergentLoopExit(block); }
    bool addDivergentLoopExit(const llvm::BasicBlock& block);
    void removeDivergentLoopExit(const llvm::BasicBlock& block);

    bool inRegion(const llvm::Instruction & inst) const;
    bool inRegion(const llvm::BasicBlock & block) const;
    llvm::BasicBlock & getEntry() const;
    Region* getRegion() const { return region; }

    const VectorMapping& getMapping() const { return mapping; }

    size_t getVectorWidth() const { return mapping.vectorWidth; }

    VectorizationInfo(Region & funcRegion, VectorMapping _mapping);
    VectorizationInfo(llvm::Function& parentFn, unsigned vectorWidth, Region& _region);


    /// Disable recomputation of this value's shape and make it effectvely final
    const decltype(pinned) & pinned_values() const { return pinned; }
    void setPinned(const llvm::Value&);
    void setPinnedShape(const llvm::Value& v, VectorShape shape) {
      setPinned(v);
      setVectorShape(v, shape);
    }
    bool isPinned(const llvm::Value&) const;

    bool hasKnownShape(const llvm::Value& val) const;

    // get the shape of @val observed at @observerBlock. This will be varying if @val is defined in divergent loop.
    VectorShape getObservedShape(const llvm::LoopInfo & LI, const llvm::BasicBlock & observerBlock, const llvm::Value & val) const;

    // get the shape of @val observerd in the defining block of @val (if it is an instruction).
    VectorShape getVectorShape(const llvm::Value& val) const;

    void setVectorShape(const llvm::Value& val, VectorShape shape);
    void dropVectorShape(const llvm::Value& val);

    bool isTemporalDivergent(const llvm::LoopInfo & LI,
                             const llvm::BasicBlock &ObservingBlock,
                             const llvm::Value &Val) const;

    // return the predicate value for this instruction
    llvm::Value* getPredicate(const llvm::BasicBlock& block) const;

    void setPredicate(const llvm::BasicBlock& block, llvm::Value& predicate);
    void dropPredicate(const llvm::BasicBlock& block);

    void remapPredicate(llvm::Value& dest, llvm::Value& old);

    void dump() const;
    void print(llvm::raw_ostream & out) const;
    void dump(const llvm::Value * val) const;
    void print(const llvm::Value * val, llvm::raw_ostream&) const;
    void printBlockInfo(const llvm::BasicBlock & block, llvm::raw_ostream&) const;
    void dumpBlockInfo(const llvm::BasicBlock & block) const;
    void printArguments(llvm::raw_ostream&) const;
    void dumpArguments() const;


    llvm::LLVMContext & getContext() const;
    llvm::Function & getScalarFunction() { return *mapping.scalarFn; }
    llvm::Function & getVectorFunction() { return *mapping.vectorFn; }
};


}

#endif /* INCLUDE_RV_VECTORIZATIONINFO_H_ */
