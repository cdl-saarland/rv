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
  class Loop;
}

#include "llvm/IR/ValueHandle.h"

#include "vectorShape.h"
#include "vectorMapping.h"
#include "region/Region.h"

#include <unordered_map>
#include <set>

namespace rv
{

class Region;

// provides vectorization information (vector shapes, block predicates) for a function
class VectorizationInfo
{
    VectorMapping mapping;
    std::unordered_map<const llvm::BasicBlock*, llvm::TrackingVH<llvm::Value>> predicates;
    std::unordered_map<const llvm::Value*, VectorShape> shapes;

    std::set<const llvm::Loop*> mDivergentLoops;
    std::set<const llvm::BasicBlock*> NonKillExits;

    Region* region;

    std::set<const llvm::Value*> pinned;
    std::map<const llvm::Value*, VectorShape> initial;

public:
    bool inRegion(const llvm::Instruction & inst) const;
    bool inRegion(const llvm::BasicBlock & block) const;
    llvm::BasicBlock & getEntry() const;

    Region* getRegion() const { return region; }

    const VectorMapping& getMapping() const { return mapping; }

    uint getVectorWidth() const { return mapping.vectorWidth; }

    VectorizationInfo(VectorMapping _mapping);
    VectorizationInfo(llvm::Function& parentFn, uint vectorWidth, Region& _region);

    bool hasKnownShape(const llvm::Value& val) const;

    VectorShape getVectorShape(const llvm::Value& val) const;
    void setVectorShape(const llvm::Value& val, VectorShape shape);
    void dropVectorShape(const llvm::Value& val);

    // return the predicate value for this instruction
    llvm::Value* getPredicate(const llvm::BasicBlock& block) const;

    void setPredicate(const llvm::BasicBlock& block, llvm::Value& predicate);
    void dropPredicate(const llvm::BasicBlock& block);

    void remapPredicate(llvm::Value& dest, llvm::Value& old);

    bool isDivergentLoop(const llvm::Loop* loop) const;
    bool isDivergentLoopTopLevel(const llvm::Loop* loop) const;

    void dump() const;
    void dump(const llvm::Value * val) const;
    void dumpBlockInfo(const llvm::BasicBlock & block) const;
    void dumpArguments() const;

    void setDivergentLoop(const llvm::Loop* loop);
    void setLoopDivergence(const llvm::Loop & loop, bool toUniform);

    // whether this exit block terminates the loop
    bool isKillExit(const llvm::BasicBlock & block) const;
    void setNotKillExit(const llvm::BasicBlock* block);

    /// Disable recomputation of this value's shape and make it effectvely final
    void setPinned(const llvm::Value*);

    bool isPinned(const llvm::Value*);


    /// Adds a value and its assumed shape the initialization list.
    /// Dependent values will be updated accordingly during analysis.
    void addInitial(const llvm::Value*, VectorShape);

    /// Convenience function that combines setPinned and setInitial.
    void addPinnedInitial(const llvm::Value*, VectorShape);

    /// Returns a pointer to the initialization mapping
    std::map<const llvm::Value*, VectorShape>* getInitialization();

    /// Empties the initialization mapping to allow for a new analysis run.
    void clearInitialization();

    llvm::LLVMContext & getContext() const;
    llvm::Function & getScalarFunction() { return *mapping.scalarFn; }
    llvm::Function & getVectorFunction() { return *mapping.vectorFn; }
};


}

#endif /* INCLUDE_RV_VECTORIZATIONINFO_H_ */
