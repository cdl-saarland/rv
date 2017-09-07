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
#include "rv/config.h"

#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/Transforms/Utils/ValueMapper.h>
#include <llvm/ADT/SmallVector.h>

namespace rv {
  class Region;
  class ReductionAnalysis;
  class Reduction;
  class StridePattern;
}

using ValVec = llvm::SmallVector<llvm::Value*, 16>;

namespace native {
  typedef std::map<const llvm::Function *, const rv::VectorMapping *> VectorMappingMap;
  typedef std::vector<llvm::Value *> LaneValueVector;
  typedef std::vector<llvm::Value *> PseudointerValueVector;
  typedef std::vector<llvm::BasicBlock *> BasicBlockVector;

  class NatBuilder {
    llvm::IRBuilder<> builder;

    rv::Config config;
    rv::PlatformInfo &platformInfo;
    rv::VectorizationInfo &vecInfo;
    const llvm::DominatorTree &dominatorTree;
    llvm::MemoryDependenceResults & memDepRes;
    llvm::ScalarEvolution &SE;
    rv::ReductionAnalysis & reda;

    llvm::DataLayout layout;

    llvm::Type *i1Ty;
    llvm::Type *i32Ty;

    rv::Region *region;

    void printStatistics();

    rv::VectorShape getVectorShape(const llvm::Value &val);

    // repair outside uses of redChainInst using repairFunc
    void repairOutsideUses(llvm::Instruction & scaChainInst, std::function<llvm::Value& (llvm::Value &,llvm::BasicBlock &)> repairFunc);

    // generate reduction code (after all other instructions have been vectorized)
    void materializeVaryingReduction(rv::Reduction & red, llvm::PHINode & scaPhi);

    // materialize a recurrence pattern (SCC only consists of phis and selects)
    void materializeRecurrence(rv::Reduction & red, llvm::PHINode & scaPhi);

    // fixup the
    void materializeStridePattern(rv::StridePattern & sp);

    llvm::Value& materializeVectorReduce(llvm::IRBuilder<> & builder, llvm::Value & phiInitVal, llvm::Value & vecVal, llvm::Instruction & reduceOp);

    // create a mask cascade at the current insertion point, call @genFunc in every cascaded block, if @packResult insert all values provided by @genFunc into
    // an accumulator and return that (result size 1). If !@packResult return dominating definitions of the computed value
    // @genFunc: first argument is an IRBuilder that inserts into a fresh mask-guarded block, second argument is the lane for which the instruction @inst should be scalarized
    ValVec scalarizeCascaded(llvm::BasicBlock & srcBlock, llvm::Instruction & inst, bool packResult, std::function<llvm::Value*(llvm::IRBuilder<>&,size_t)> genFunc);

  public:
    NatBuilder(rv::Config config, rv::PlatformInfo &_platformInfo, rv::VectorizationInfo &_vecInfo,
               const llvm::DominatorTree &_dominatorTree, llvm::MemoryDependenceResults &memDepRes,
               llvm::ScalarEvolution &_SE, rv::ReductionAnalysis & _reda);

    // if embedRegion is set, replace the scalar source blocks/instructions with the vectorized version
    // if vecInstMap is set, store the mapping from scalar source insts/blocks to vector versions
    void vectorize(bool embedRegion, llvm::ValueToValueMapTy * vecInstMap = nullptr);

    void mapVectorValue(const llvm::Value *const value, llvm::Value *vecValue);
    void mapScalarValue(const llvm::Value *const value, llvm::Value *mapValue, unsigned laneIdx = 0);

    llvm::Value *getVectorValue(llvm::Value *const value, bool getLastBlock = false);
    llvm::Value *getScalarValue(llvm::Value *const value, unsigned laneIdx = 0);
    BasicBlockVector &getMappedBlocks(llvm::BasicBlock *const bb);

  private:
    void vectorize(llvm::BasicBlock *const bb, llvm::BasicBlock *vecBlock);
    void vectorize(llvm::Instruction *const inst);
    void vectorizePHIInstruction(llvm::PHINode *const scalPhi);
    void vectorizeMemoryInstruction(llvm::Instruction *const inst);
    void vectorizeCallInstruction(llvm::CallInst *const scalCall);
    void vectorizeAllocaInstruction(llvm::AllocaInst *const alloca);
    void vectorizeReductionCall(llvm::CallInst *rvCall, bool isRv_all);
    void vectorizeExtractCall(llvm::CallInst *rvCall);
    void vectorizeInsertCall(llvm::CallInst *rvCall);
    void vectorizeLoadCall(llvm::CallInst *rvCall);
    void vectorizeStoreCall(llvm::CallInst *rvCall);
    void vectorizeShuffleCall(llvm::CallInst *rvCall);
    void vectorizeBallotCall(llvm::CallInst *rvCall);
    void vectorizeAlignCall(llvm::CallInst *rvCall);

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
    std::map<const llvm::Type *, rv::MemoryAccessGrouper> grouperMap;
    std::map<const llvm::Value *, PseudointerValueVector> pseudointerValueMap;
    std::vector<llvm::PHINode *> phiVector;
    std::deque<llvm::Instruction *> lazyInstructions;

    void addLazyInstruction(llvm::Instruction *const instr);
    void requestLazyInstructions(llvm::Instruction *const upToInstruction);
    llvm::Value *requestVectorValue(llvm::Value *const value);
    llvm::Value *requestScalarValue(llvm::Value *const value, unsigned laneIdx = 0,
                                    bool skipMapping = false);
    llvm::GetElementPtrInst *buildGEP(llvm::GetElementPtrInst *const gep, bool buildScalar, unsigned laneIdx);
    llvm::GetElementPtrInst *requestVectorGEP(llvm::GetElementPtrInst *const gep);
    llvm::GetElementPtrInst *requestScalarGEP(llvm::GetElementPtrInst *const gep, unsigned laneIdx, bool skipMapping);
    llvm::Value *requestVectorBitCast(llvm::BitCastInst *const bc);
    llvm::Value *requestScalarBitCast(llvm::BitCastInst *const bc, unsigned laneIdx, bool skipMapping);

    llvm::GetElementPtrInst *requestInterleavedGEP(llvm::GetElementPtrInst *const gep, unsigned interleavedIdx);
    llvm::Value *requestInterleavedAddress(llvm::Value *const addr, unsigned interleavedIdx, llvm::Type *const vecType);

    llvm::Value *requestCascadeLoad(llvm::Value *vecPtr, unsigned alignment, llvm::Value *mask);
    llvm::Value *requestCascadeStore(llvm::Value *vecVal, llvm::Value *vecPtr, unsigned alignment, llvm::Value *mask);
    llvm::Function *createCascadeMemory(llvm::VectorType *pointerVectorType, unsigned alignment,
                                        llvm::VectorType *maskType, bool store);

    void mapCascadeFunction(unsigned bitWidth, llvm::Function *function, bool store);
    llvm::Function *getCascadeFunction(unsigned bitWidth, bool store);

    llvm::Value *createPTest(llvm::Value *vector, bool isRv_all);
    llvm::Value *maskInactiveLanes(llvm::Value *const value, const llvm::BasicBlock* const block, bool invert);

    unsigned vectorWidth();

    bool canVectorize(llvm::Instruction *inst);
    bool shouldVectorize(llvm::Instruction *inst);
    bool isInterleaved(llvm::Instruction *inst, llvm::Value *accessedPtr, int byteSize, std::vector<llvm::Value *> &srcs);
    bool isPseudointerleaved(llvm::Instruction *inst, llvm::Value *addr, int byteSize);

    // a varying value is stored to a uniform ptr (under a predicate)
    llvm::Value *createVaryingToUniformStore(llvm::Instruction *inst, llvm::Type *accessedType, unsigned int alignment, llvm::Value *addr, llvm::Value *mask, llvm::Value *values);

    // a uniform value is stored to a uniform ptr (with a predicate)
    llvm::Value *createUniformMaskedMemory(llvm::Instruction *inst, llvm::Type *accessedType, unsigned alignment,
                                           llvm::Value *addr, llvm::Value *mask, llvm::Value *values);
    llvm::Value *createVaryingMemory(llvm::Type *vecType, unsigned alignment, llvm::Value *addr, llvm::Value *mask,
                                     llvm::Value *values);
    void createInterleavedMemory(llvm::Type *vecType, unsigned alignment, std::vector<llvm::Value *> *addr, std::vector<llvm::Value *> *mask,
                                     std::vector<llvm::Value *> *values, std::vector<llvm::Value *> *srcs, bool isPseudoInter = false);

    llvm::Value *createContiguousStore(llvm::Value *val, llvm::Value *ptr, unsigned alignment, llvm::Value *mask);
    llvm::Value *createContiguousLoad(llvm::Value *ptr, unsigned alignment, llvm::Value *mask, llvm::Value *passThru);

    void visitMemInstructions();

  };
}

#endif //NATIVE_NATBUILDER_H
