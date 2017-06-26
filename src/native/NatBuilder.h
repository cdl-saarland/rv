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

#include <rv/vectorizationInfo.h>
#include <rv/PlatformInfo.h>

#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

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
    llvm::MemoryDependenceResults & memDepRes;
    llvm::ScalarEvolution &SE;
    rv::ReductionAnalysis & reda;

    llvm::DataLayout layout;

    llvm::Type *i1Ty;
    llvm::Type *i32Ty;

    rv::Region *region;

    bool useScatterGatherIntrinsics;
    bool vectorizeInterleavedAccess;

    rv::VectorShape getVectorShape(const Value &val);

    // repair outside uses of redChainInst using repairFunc
    void repairOutsideUses(llvm::Instruction & scaChainInst, std::function<llvm::Value& (llvm::Value &,llvm::BasicBlock &)> repairFunc);

    // generate reduction code (after all other instructions have been vectorized)
    void materializeVaryingReduction(rv::Reduction & red);

    // fixup the
    void materializeStridedReduction(rv::Reduction & red);

    llvm::Value& materializeVectorReduce(llvm::IRBuilder<> & builder, llvm::Value & phiInitVal, llvm::Value & vecVal, llvm::Instruction & reduceOp);

  public:
    NatBuilder(rv::PlatformInfo &platformInfo, rv::VectorizationInfo &vectorizationInfo,
               const llvm::DominatorTree &dominatorTree, llvm::MemoryDependenceResults &memDepRes,
               llvm::ScalarEvolution &SE, rv::ReductionAnalysis & _reda);

    // if embedRegion is set, replace the scalar source blocks/instructions with the vectorized version
    // if vecInstMap is set, store the mapping from scalar source insts/blocks to vector versions
    void vectorize(bool embedRegion, ValueToValueMapTy * vecInstMap = nullptr);

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
    void vectorizeAlignCall(CallInst *rvCall);

    void copyInstruction(llvm::Instruction *const inst, unsigned laneIdx = 0);
    void copyCallInstruction(llvm::CallInst *const scalCall, unsigned laneIdx = 0);

    void fallbackVectorize(llvm::Instruction *const inst);

    void addValuesToPHINodes();

    void mapOperandsInto(llvm::Instruction *const scalInst, llvm::Instruction *inst, bool vectorizedInst,
                         unsigned laneIdx = 0);

    llvm::SmallPtrSet<llvm::Instruction *, 16> keepScalar;
    llvm::DenseMap<unsigned, llvm::Function *> cascadeLoadMap;
    llvm::DenseMap<unsigned, llvm::Function *> cascadeStoreMap;
    llvm::DenseMap<const llvm::Value *, llvm::Value *> vectorValueMap;
    std::map<const llvm::Value *, LaneValueVector> scalarValueMap;
    std::map<const llvm::BasicBlock *, BasicBlockVector> basicBlockMap;
    std::map<const llvm::Type *, MemoryAccessGrouper> grouperMap;
    std::vector<llvm::PHINode *> phiVector;
    std::deque<llvm::Instruction *> lazyInstructions;

    void requestLazyInstructions(llvm::Instruction *const upToInstruction);
    llvm::Value *requestVectorValue(llvm::Value *const value);
    llvm::Value *requestScalarValue(llvm::Value *const value, unsigned laneIdx = 0,
                                    bool skipMappingWhenDone = false);
    llvm::GetElementPtrInst *buildGEP(llvm::GetElementPtrInst *const gep, bool buildScalar, unsigned laneIdx);
    llvm::GetElementPtrInst *requestVectorGEP(llvm::GetElementPtrInst *const gep);
    llvm::GetElementPtrInst *requestScalarGEP(llvm::GetElementPtrInst *const gep, unsigned laneIdx);
    llvm::BitCastInst *requestVectorBitCast(llvm::BitCastInst *const bc);
    llvm::BitCastInst *requestScalarBitCast(llvm::BitCastInst *const bc, unsigned laneIdx);
    
    llvm::GetElementPtrInst *requestInterleavedGEP(llvm::GetElementPtrInst *const gep, unsigned interleavedIdx);
    Value *requestInterleavedAddress(llvm::Value *const addr, unsigned interleavedIdx, llvm::Type *const vecType);
    
    llvm::Value *requestCascadeLoad(llvm::Value *vecPtr, unsigned alignment, llvm::Value *mask);
    llvm::Value *requestCascadeStore(llvm::Value *vecVal, llvm::Value *vecPtr, unsigned alignment, llvm::Value *mask);
    llvm::Function *createCascadeMemory(llvm::VectorType *pointerVectorType, unsigned alignment,
                                        llvm::VectorType *maskType, bool store);

    void mapCascadeFunction(unsigned bitWidth, llvm::Function *function, bool store);
    llvm::Function *getCascadeFunction(unsigned bitWidth, bool store);

    llvm::Value *createPTest(llvm::Value *vector, bool isRv_all);
    llvm::Value *maskInactiveLanes(llvm::Value *const value, const BasicBlock* const block, bool invert);

    unsigned vectorWidth();

    bool canVectorize(llvm::Instruction *inst);
    bool shouldVectorize(llvm::Instruction *inst);
    bool isInterleaved(llvm::Instruction *inst, llvm::Value *accessedPtr, int byteSize, std::vector<Value *> &srcs);

    llvm::Value *createUniformMaskedMemory(llvm::Instruction *inst, llvm::Type *accessedType, unsigned alignment,
                                           llvm::Value *addr, llvm::Value *mask, llvm::Value *values);
    llvm::Value *createVaryingMemory(llvm::Type *vecType, unsigned alignment, llvm::Value *addr, llvm::Value *mask,
                                     llvm::Value *values);
    void createInterleavedMemory(llvm::Type *vecType, unsigned alignment, std::vector<Value *> *addr, llvm::Value *mask,
                                     std::vector<Value *> *values, std::vector<Value *> *srcs);

    llvm::Value *createContiguousStore(llvm::Value *val, llvm::Value *ptr, unsigned alignment, llvm::Value *mask);
    llvm::Value *createContiguousLoad(llvm::Value *ptr, unsigned alignment, llvm::Value *mask, llvm::Value *passThru);

    void visitMemInstructions();

  };
}

#endif //NATIVE_NATBUILDER_H
