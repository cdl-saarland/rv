//===- src/native/NatBuilder.h - widening --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef NATIVE_NATBUILDER_H
#define NATIVE_NATBUILDER_H


#include "MemoryAccessGrouper.h"

#include <vector>

#include "rv/vectorizationInfo.h"
#include "rv/PlatformInfo.h"
#include "rv/config.h"
#include "rv/intrinsics.h"
#include "rv/analysis/UndeadMaskAnalysis.h"
#include "rv/analysis/reductions.h"
#include "llvm/IR/PassManager.h"

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

  using ValVec = llvm::SmallVector<llvm::Value*, 16>;

  typedef std::map<const llvm::Function *, const rv::VectorMapping *> VectorMappingMap;
  typedef std::vector<llvm::Value *> LaneValueVector;
  typedef std::vector<llvm::BasicBlock *> BasicBlockVector;

  class NatBuilder {
    llvm::IRBuilder<> builder;

    rv::Config config;
    rv::PlatformInfo & platInfo;
    rv::VectorizationInfo &vecInfo;
    const llvm::DominatorTree &dominatorTree;
    llvm::MemoryDependenceResults & memDepRes;
    llvm::ScalarEvolution &SE;
    rv::ReductionAnalysis & reda;
    rv::UndeadMaskAnalysis undeadMasks;

    llvm::DataLayout layout;

    llvm::Type *i1Ty;
    llvm::Type *i32Ty;

    // the predicate argument in the vector function (WFV mode)
    llvm::Value * vecMaskArg;

    void printStatistics();

    rv::VectorShape getVectorShape(const llvm::Value &val);

    // get the appropriate integer ty to index into the ptr-typed \p val.
    llvm::Type* getIndexTy(llvm::Value * val) const;

    // repair outside uses of redChainInst using repairFunc
    void repairOutsideUses(llvm::Instruction & scaChainInst, std::function<llvm::Value& (llvm::Value &,llvm::BasicBlock &)> repairFunc);

    // generate reduction code (after all other instructions have been vectorized)
    void materializeVaryingReduction(rv::Reduction & red, llvm::PHINode & scaPhi);
    void materializeOrderedReduction(rv::Reduction & red, llvm::PHINode & scaPhi);

    // materialize a recurrence pattern (SCC only consists of phis and selects)
    void materializeRecurrence(rv::Reduction & red, llvm::PHINode & scaPhi);

    // fixup the
    void materializeStridePattern(rv::StridePattern & sp);

    llvm::Value& materializeVectorReduce(llvm::IRBuilder<> & builder, llvm::Value & phiInitVal, llvm::Value & vecVal, llvm::Instruction & reduceOp);

    // create a mask cascade at the current insertion point, call @genFunc in every cascaded block, if @packResult insert all values provided by @genFunc into
    // an accumulator and return that (result size 1). If !@packResult return dominating definitions of the computed value
    // @genFunc: first argument is an IRBuilder that inserts into a fresh mask-guarded block, second argument is the lane for which the instruction @inst should be scalarized
    ValVec scalarizeCascaded(llvm::BasicBlock & srcBlock, llvm::Instruction & srcInst, bool packResult, std::function<llvm::Value*(llvm::IRBuilder<>&,size_t)> genFunc);

    // scalarize without if-guard
    ValVec scalarize(llvm::BasicBlock & srcBlock, llvm::Instruction & srcInst, bool packResult, std::function<llvm::Value*(llvm::IRBuilder<>&,size_t)> genFunc);

  public:
    NatBuilder(rv::Config config, rv::PlatformInfo &_platformInfo, rv::VectorizationInfo &_vecInfo,
               rv::ReductionAnalysis & _reda, llvm::FunctionAnalysisManager &FAM);

    // if embedRegion is set, replace the scalar source blocks/instructions with the vectorized version
    // if vecInstMap is set, store the mapping from scalar source insts/blocks to vector versions
    void vectorize(bool embedRegion, llvm::ValueToValueMapTy * vecInstMap = nullptr);

    void mapVectorValue(const llvm::Value *const value, llvm::Value *vecValue);
    void mapScalarValue(const llvm::Value *const value, llvm::Value *mapValue, unsigned laneIdx = 0);

    llvm::BasicBlock *getVectorBlock(llvm::BasicBlock &ScaBlock,
                                     bool LastBlock);
    llvm::Value *getVectorValue(llvm::Value &ScaValue,
                                bool getLastBlock = false);
    template <typename CastType>
    CastType *getVectorValueAs(llvm::Value &ScaValue,
                               bool getLastBlock = false) {
      auto *VecVal = getVectorValue(ScaValue, getLastBlock);
      if (!VecVal)
        return nullptr;
      return llvm::cast<CastType>(VecVal);
    }
    llvm::Value *getScalarValue(llvm::Value &ScaValue, unsigned laneIdx = 0);
    template <typename CastClass>
    CastClass *getScalarValueAs(llvm::Value &ScaValue, unsigned laneIdx = 0) {
      auto VecVal = getScalarValue(ScaValue, laneIdx);
      if (!VecVal)
        return nullptr;
      return llvm::cast<CastClass>(VecVal);
    }

    BasicBlockVector getMappedBlocks(llvm::BasicBlock *const bb);

  private:
    void vectorize(llvm::BasicBlock *const bb, llvm::BasicBlock *vecBlock);
    void vectorizeInstruction(llvm::Instruction *const inst);
    void vectorizePHIInstruction(llvm::PHINode *const scalPhi);
    void vectorizeMemoryInstruction(llvm::Instruction *const inst);
    void vectorizeCallInstruction(llvm::CallInst *const scalCall);
    void vectorizeReductionCall(llvm::CallInst *rvCall, bool isRv_all);
    void vectorizeExtractCall(llvm::CallInst *rvCall);
    void vectorizeInsertCall(llvm::CallInst *rvCall);
    void vectorizeLoadCall(llvm::CallInst *rvCall);
    void vectorizeStoreCall(llvm::CallInst *rvCall);
    void vectorizeShuffleCall(llvm::CallInst *rvCall);
    void vectorizeBallotCall(llvm::CallInst *rvCall);
    void vectorizePopCountCall(llvm::CallInst *rvCall);
    void vectorizeAlignCall(llvm::CallInst *rvCall);
    void vectorizeIndexCall(llvm::CallInst & rvCall);
    void vectorizeCompactCall(llvm::CallInst * rvCall);
    void vectorizeLaneIDCall(llvm::CallInst *rvCall);
    void vectorizeNumLanesCall(llvm::CallInst *rvCall);

    void vectorizeAtomicRMW(llvm::AtomicRMWInst *const atomicrmw);

    // create a lookup table for an efficient compaction intrinsic
    llvm::Constant* createCompactLookupTable(unsigned vecWidth);

    void vectorizeAlloca(llvm::AllocaInst *const allocaInst);

    // implement the mask summary function @mode (ballot/popcount) of @vecVal with @builder
    llvm::Value* createVectorMaskSummary(llvm::Type & indexTy, llvm::Value * vecVal, llvm::IRBuilder<> & builder, rv::RVIntrinsic mode);

    void copyInstruction(llvm::Instruction *const inst, unsigned laneIdx = 0);
    void copyCallInstruction(llvm::CallInst *const scalCall, unsigned laneIdx = 0);

    // "vectorize" the instruction by creating scalar replicas and inserting their results in a vector (where appropriate)
    void replicateInstruction(llvm::Instruction *const inst);

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
    std::vector<llvm::PHINode *> phiVector;
    std::deque<llvm::Instruction *> lazyInstructions;

    void addLazyInstruction(llvm::Instruction *const instr);
    void requestLazyInstructions(llvm::Instruction *const upToInstruction);
    llvm::Value* requestVectorPredicate(const llvm::BasicBlock& scaBlock);
    llvm::Value *requestVectorValue(llvm::Value *const value);
    void SetInsertPointAfterMappedInst(llvm::IRBuilder<> & builder, llvm::Instruction * mappedInst);
    llvm::Value *requestScalarValue(llvm::Value *const value, unsigned laneIdx = 0,
                                    bool skipMapping = false);
    llvm::Value *buildGEP(llvm::GetElementPtrInst *const gep, bool buildScalar, unsigned laneIdx);
    llvm::Value *requestVectorGEP(llvm::GetElementPtrInst *const gep);
    llvm::Value *requestScalarGEP(llvm::GetElementPtrInst *const gep, unsigned laneIdx, bool skipMapping);
    llvm::Value *requestVectorBitCast(llvm::BitCastInst *const bc);
    llvm::Value *requestScalarBitCast(llvm::BitCastInst *const bc, unsigned laneIdx, bool skipMapping);

    llvm::Value *requestInterleavedGEP(llvm::GetElementPtrInst *const gep, unsigned interleavedIdx);
    llvm::Value *requestInterleavedAddress(llvm::Value *const addr, unsigned interleavedIdx, llvm::Type *const vecType);

    llvm::Value *requestCascadeLoad(llvm::Type *accessedType, llvm::Value *vecPtr, unsigned alignment, llvm::Value *mask);
    llvm::Value *requestCascadeStore(llvm::Value *vecVal, llvm::Value *vecPtr, unsigned alignment, llvm::Value *mask);
    llvm::Function *createCascadeMemory(llvm::Type *accessedType, llvm::VectorType *pointerVectorType, unsigned alignment,
                                        llvm::VectorType *maskType, bool store);

    llvm::Value* getSplat(llvm::Constant* Elt);
    void mapCascadeFunction(unsigned bitWidth, llvm::Function *function, bool store);
    llvm::Function *getCascadeFunction(unsigned bitWidth, bool store);

    llvm::Value& widenScalar(llvm::Value & scaValue, VectorShape vecShape);
    bool hasUniformPredicate(const llvm::BasicBlock & BB) const;
    llvm::Value *createPTest(llvm::Value *vector, bool isRv_all);
    llvm::Value *maskInactiveLanes(llvm::Value *const value, const llvm::BasicBlock* const block, bool invert);

    int vectorWidth() const;

    bool canVectorize(llvm::Instruction *inst);
    bool shouldVectorize(llvm::Instruction *inst);
    bool isInterleaved(llvm::Instruction *inst, llvm::Value *accessedPtr, int byteSize, std::vector<llvm::Value *> &srcs);

    // request and return all the vector arguments for calling \p vecCall with the vector mappings for the arguments in \p scaCall. this should also include the mask (if any).
    void requestVectorCallArgs(llvm::CallInst & scaCall, llvm::Function & vecCall, int maskPos, std::vector<llvm::Value*> & vectorArgs);

    // a varying value is stored to a uniform ptr (under a predicate)
    llvm::Value *createVaryingToUniformStore(llvm::Instruction *inst, llvm::Type *accessedType, llvm::Align alignment, llvm::Value *addr, llvm::Value *mask, llvm::Value *values);

    // guard the code generated by @genFunc in a rv_any(ptest) on the predicate (if required).
    // if (p) { %phi = <genFunc> };
    // this will omit the guard if \p instNeedsGuard is false.
    llvm::Value &createAnyGuard(bool instNeedsGuard, llvm::BasicBlock & srcBlock, llvm::Instruction & inst, bool producesValue, std::function<llvm::Value*(llvm::IRBuilder<>&)> genFunc);

    // a uniform value is stored to a uniform ptr (with a predicate)
    llvm::Value *createUniformMaskedMemory(llvm::Instruction *inst, llvm::Type *accessedType, llvm::Align alignment,
                                           llvm::Value *addr, llvm::Value * scalarMask, llvm::Value *vectorMask, llvm::Value *values);

    llvm::Value *createVaryingMemory(llvm::Type *vecType, llvm::Align alignment, llvm::Value *addr, llvm::Value *mask,
                                     llvm::Value *values);
    void createInterleavedMemory(llvm::Type *targetType, llvm::Type *vecType, llvm::Align alignment, std::vector<llvm::Value *> *addr, std::vector<llvm::Value *> *mask,
                                     std::vector<llvm::Value *> *values, std::vector<llvm::Value *> *srcs);

    llvm::Value *createContiguousStore(llvm::Value *val, llvm::Value *ptr, llvm::Align alignment, llvm::Value *mask);
    llvm::Value *createContiguousLoad(llvm::Type *targetType, llvm::Value *ptr, llvm::Align alignment, llvm::Value *mask, llvm::Value *passThru);

    void visitMemInstructions();

    // match an "*uniPtr += varyinValue" kind of pattern
    RedKind matchMemoryReduction(llvm::Value * scaPtr, llvm::Value * scaValue, llvm::Value *& oPayload, llvm::Instruction *& oScaLoad);
  };
}

#endif //NATIVE_NATBUILDER_H
