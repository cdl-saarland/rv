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

using namespace llvm;

#include "llvm/IR/ValueHandle.h"

#include "vectorShape.h"
#include "vectorMapping.h"

#include <unordered_map>
#include <set>

namespace rv
{

class Region;

// provides vectorization information (vector shapes, block predicates) for a function
class VectorizationInfo
{
    VectorMapping mapping;
    std::unordered_map<const BasicBlock*, WeakVH> predicates;
    std::unordered_map<const Value*, VectorShape> shapes;

    std::set<const Loop*> mDivergentLoops;

    std::set<const BasicBlock*> MandatoryBlocks;

    Region* region;

public:
    bool inRegion(const llvm::Instruction & inst) const;
    bool inRegion(const llvm::BasicBlock & block) const;
    BasicBlock & getEntry() const;

    Region* getRegion() const {
        return region;
    }

    const VectorMapping& getMapping() const {
        return mapping;
    }

    uint getVectorWidth() const
    {
        return mapping.vectorWidth;
    }

    VectorizationInfo(VectorMapping _mapping);
    VectorizationInfo(llvm::Function& parentFn, uint vectorWidth, Region& _region);

    bool hasKnownShape(const Value& val) const;

    VectorShape getVectorShape(const Value& val) const;
    void setVectorShape(const Value& val, VectorShape shape);
    void dropVectorShape(const Value& val);

    // return the predicate value for this instruction
    Value* getPredicate(const BasicBlock& block) const;

    void setPredicate(const BasicBlock& block, Value& predicate);
    void dropPredicate(const BasicBlock& block);

    void remapPredicate(Value& dest, Value& old);

    bool isDivergentLoop(const Loop* loop) const;
    bool isDivergentLoopTopLevel(const Loop* loop) const;

    void dump() const;
    void dump(const Value * val) const;
    void dumpBlockInfo(const BasicBlock & block) const;
    void dumpArguments() const;

    void setDivergentLoop(const Loop* loop);
    void setLoopDivergence(const Loop & loop, bool toUniform);

    bool isMandatory(const BasicBlock* block) const;
    bool isMetadataMask(const Instruction* inst) const;

    // whether this exit block terminates the loop
    bool isKillExit(const BasicBlock & block) const;

    void markMandatory(const BasicBlock* block);

    LLVMContext & getContext() const;
    Function & getScalarFunction() { return *mapping.scalarFn; }
    Function & getVectorFunction() { return *mapping.vectorFn; }
};


}

#endif /* INCLUDE_RV_VECTORIZATIONINFO_H_ */
