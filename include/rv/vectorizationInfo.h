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
#include <llvm/Support/raw_ostream.h>

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

public:
    bool inRegion(const llvm::Instruction & inst) const;
    bool inRegion(const llvm::BasicBlock & block) const;
    llvm::BasicBlock & getEntry() const;

    Region* getRegion() const { return region; }

    const VectorMapping& getMapping() const { return mapping; }

    size_t getVectorWidth() const { return mapping.vectorWidth; }

    VectorizationInfo(Region & funcRegion, VectorMapping _mapping);
    VectorizationInfo(llvm::Function& parentFn, unsigned vectorWidth, Region& _region);

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
    void print(llvm::raw_ostream & out) const;
    void dump(const llvm::Value * val) const;
    void print(const llvm::Value * val, llvm::raw_ostream&) const;
    void printBlockInfo(const llvm::BasicBlock & block, llvm::raw_ostream&) const;
    void dumpBlockInfo(const llvm::BasicBlock & block) const;
    void printArguments(llvm::raw_ostream&) const;
    void dumpArguments() const;

    void setLoopDivergence(const llvm::Loop & loop, bool toUniform);

    // whether this exit block terminates the loop
    bool isKillExit(const llvm::BasicBlock & block) const;
    void setNotKillExit(const llvm::BasicBlock* block);

    /// Disable recomputation of this value's shape and make it effectvely final
    const decltype(pinned) & pinned_values() const { return pinned; }
    void setPinned(const llvm::Value&);
    void setPinnedShape(const llvm::Value& v, VectorShape shape) {
      setPinned(v);
      setVectorShape(v, shape);
    }
    bool isPinned(const llvm::Value&) const;

    llvm::LLVMContext & getContext() const;
    llvm::Function & getScalarFunction() { return *mapping.scalarFn; }
    llvm::Function & getVectorFunction() { return *mapping.vectorFn; }
};


}

#endif /* INCLUDE_RV_VECTORIZATIONINFO_H_ */
