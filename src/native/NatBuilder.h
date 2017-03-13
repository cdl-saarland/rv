//===- NatBuilder.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef NATIVE_NATBUILDER_H
#define NATIVE_NATBUILDER_H


#include "MemoryAccessGrouper.h"

#include <vector>

#include <rv/analysis/maskAnalysis.h>
#include <rv/vectorizationInfo.h>
#include <rv/PlatformInfo.h>

#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>

namespace rv {
  class Region;
  class ReductionAnalysis;
  class Reduction;
}

namespace native {
  typedef std::map<const llvm::Function *, const rv::VectorMapping *> VectorMappingMap;
  typedef std::vector<llvm::Value *> LaneValueVector;
  typedef std::vector<llvm::BasicBlock *> BasicBlockVector;

  class NatBuilder {
    llvm::IRBuilder<> builder;

    rv::PlatformInfo &platformInfo;
    rv::VectorizationInfo &vectorizationInfo;
    const llvm::DominatorTree &dominatorTree;
    llvm::MemoryDependenceAnalysis &memDepAnalysis;
    llvm::ScalarEvolution &SE;
    rv::ReductionAnalysis & reda;

    llvm::DataLayout layout;

    llvm::Type *i1Ty;
    llvm::Type *i32Ty;

    rv::Region *region;

    bool useScatterGatherIntrinsics;
    bool vectorizeInterleavedAccess;

    rv::VectorShape getShape(const Value & val);

    // generate reduction code (after all other instructions have been vectorized)
    void materializeReduction(rv::Reduction & red);
    llvm::Value& materializeVectorReduce(llvm::IRBuilder<> & builder, llvm::Value & phiInitVal, llvm::Value & vecVal, llvm::Instruction & reduceOp);

  public:
    NatBuilder(rv::PlatformInfo &platformInfo, VectorizationInfo &vectorizationInfo,
               const llvm::DominatorTree &dominatorTree, llvm::MemoryDependenceAnalysis &memDepAnalysis,
               llvm::ScalarEvolution &SE, rv::ReductionAnalysis & _reda);

    void vectorize();

    void mapVectorValue(const llvm::Value *const value, llvm::Value *vecValue);
    void mapScalarValue(const llvm::Value *const value, llvm::Value *mapValue, unsigned laneIdx = 0);

    llvm::Value *getVectorValue(llvm::Value *const value, bool getLastBlock = false);
    llvm::Value *getScalarValue(llvm::Value *const value, unsigned laneIdx = 0);

  private:
    void vectorize(llvm::BasicBlock *const bb, llvm::BasicBlock *vecBlock);
    void vectorize(llvm::Instruction *const inst);
    void vectorizePHIInstruction(llvm::PHINode *const scalPhi);
    void vectorizeMemoryInstruction(llvm::Instruction *const inst);
    void vectorizeCallInstruction(llvm::CallInst *const scalCall);
    void vectorizeAllocaInstruction(llvm::AllocaInst *const alloca);
    void vectorizeReductionCall(CallInst *rvCall, bool isRv_all);
    void vectorizeExtractCall(CallInst *rvCall);
    void vectorizeBallotCall(CallInst *rvCall);
    GetElementPtrInst *vectorizeGEPInstruction(GetElementPtrInst *const gep, bool buildVectorGEP, unsigned interleavedIndex = 0,
                                                bool skipMapping = false);

    void copyInstruction(llvm::Instruction *const inst, unsigned laneIdx = 0);
    void copyCallInstruction(llvm::CallInst *const scalCall, unsigned laneIdx = 0);

    void fallbackVectorize(llvm::Instruction *const inst);

    void addValuesToPHINodes();

    void mapOperandsInto(llvm::Instruction *const scalInst, llvm::Instruction *inst, bool vectorizedInst,
                         unsigned laneIdx = 0);

    llvm::DenseMap<unsigned, llvm::Function *> cascadeLoadMap;
    llvm::DenseMap<unsigned, llvm::Function *> cascadeStoreMap;
    llvm::DenseMap<const llvm::Value *, llvm::Value *> vectorValueMap;
    std::map<const llvm::Value *, LaneValueVector> scalarValueMap;
    std::map<const llvm::BasicBlock *, BasicBlockVector> basicBlockMap;
    std::map<const llvm::Type *, MemoryAccessGrouper> grouperMap;
    std::vector<llvm::PHINode *> phiVector;
    std::vector<llvm::Instruction *> willNotVectorize;
    std::deque<llvm::Instruction *> lazyInstructions;

    void requestLazyInstructions(llvm::Instruction *const upToInstruction);
    llvm::Value *requestVectorValue(llvm::Value *const value);
    llvm::Value *requestScalarValue(llvm::Value *const value, unsigned laneIdx = 0,
                                    bool skipMappingWhenDone = false);
    llvm::Value *requestCascadeLoad(llvm::Value *vecPtr, unsigned alignment, llvm::Value *mask);
    llvm::Value *requestCascadeStore(llvm::Value *vecVal, llvm::Value *vecPtr, unsigned alignment, llvm::Value *mask);
    llvm::Function *createCascadeMemory(llvm::VectorType *pointerVectorType, unsigned alignment,
                                        llvm::VectorType *maskType, bool store);

    void mapCascadeFunction(unsigned bitWidth, llvm::Function *function, bool store);
    llvm::Function *getCascadeFunction(unsigned bitWidth, bool store);

    BasicBlockVector &getAllBasicBlocksFor(llvm::BasicBlock *basicBlock);

    llvm::Value *createPTest(llvm::Value *vector, bool isRv_all);
    llvm::Value *maskInactiveLanes(llvm::Value *const value, const BasicBlock* const block, bool invert);

    unsigned vectorWidth();

    bool canVectorize(llvm::Instruction *inst);
    bool shouldVectorize(llvm::Instruction *inst);

  };
}

#endif //NATIVE_NATBUILDER_H
