//===- src/native/NatBuilder.cpp - widening --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <deque>

#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Metadata.h>
#include "llvm/IR/IntrinsicsX86.h"
#include "llvm/Support/Alignment.h"
#include "llvm/Transforms/Utils/LoopUtils.h"
#include <report.h>
#include <fstream>

#include "NatBuilder.h"
#include "Utils.h"

#include "rv/transform/redTools.h"
#include "rv/analysis/reductionAnalysis.h"
#include "rv/region/Region.h"
#include "rv/rvDebug.h"
#include "rv/intrinsics.h"

#include "rvConfig.h"
#include "ShuffleBuilder.h"

#define IF_DEBUG_NAT  IF_DEBUG

using namespace llvm;

// TODO move to vector builder class...
Value*
CreateBroadcast(IRBuilder<> & builder, Value & vec, int idx) {
  auto * intTy = Type::getInt32Ty(builder.getContext());
  auto *vecTy = cast<FixedVectorType>(vec.getType());
  const size_t vectorWidth = vecTy->getNumElements();
  std::vector<Constant*> shuffleConsts;
  for (size_t i = 0; i < vectorWidth; ++i) {
    shuffleConsts.push_back(ConstantInt::get(intTy, idx, false));
  }
  return builder.CreateShuffleVector(&vec, UndefValue::get(vecTy), ConstantVector::get(shuffleConsts));
}

Value*
CreateScalarBroadcast(IRBuilder<> & builder, Value & scaValue, int elemCount) {
  auto * vecTy = FixedVectorType::get(scaValue.getType(), elemCount);
  auto * udVal = UndefValue::get(vecTy);
  auto & firstLaneVec = *builder.CreateInsertElement(udVal, &scaValue, (uint64_t) 0, scaValue.getName() + ".infirst");
  return CreateBroadcast(builder, firstLaneVec, 0);
}

namespace rv {

unsigned numMaskedGather, numMaskedScatter, numGather, numScatter,
    numInterMaskedLoads, numInterMaskedStores, numInterLoads, numInterStores,
    numContMaskedLoads, numContMaskedStores, numContLoads, numContStores, numUniMaskedLoads, numUniMaskedStores,
    numUniLoads, numUniStores, numUniAllocas, numSlowAllocas;

unsigned numVecGEPs, numScalGEPs, numInterGEPs, numVecBCs, numScalBCs;
unsigned numVecCalls, numSemiCalls, numFallCalls, numCascadeCalls, numRVIntrinsics;
unsigned numScalarized, numVectorized, numFallbacked, numLazy;

unsigned numConstLoadMasks, numUniLoadMasks, numVarLoadMasks;
unsigned numConstStoreMasks, numUniStoreMasks, numVarStoreMasks;

bool DumpStatistics(std::string &file) {
  char * envVal = getenv("NAT_STAT_DUMP");
  if (!envVal) return false;
  else return !(file = envVal).empty();
}

Value*
NatBuilder::getSplat(Constant* Elt) {
  auto EC = ElementCount::getFixed(vectorWidth());
  return ConstantVector::getSplat(EC, Elt);
}

Type*
NatBuilder::getIndexTy(Value * val) const {
  IF_DEBUG { errs() << " querying the indexType for value: " << *val << "\n"; }
  auto * ptrTy = cast<PointerType>(val->getType());
  return layout.getIndexType(ptrTy);
}

void NatBuilder::printStatistics() {
  // memory statistics
  Report() << "nat memory:\n"
           << "\tuni allocas: " << numUniAllocas << "\n"
           << "\tslow allocas: " << numSlowAllocas << "\n"
           << "\tscatter/gather: " << numScatter << "/" << numGather << ", masked " << numMaskedScatter << "/" << numMaskedGather << "\n"
           << "\tinter load/store: " << numInterLoads << "/" << numInterStores << ", masked " << numInterMaskedLoads << "/" << numInterMaskedStores << "\n"
           << "\tcons load/store: " << numContLoads << "/" << numContStores << ", masked " <<  numContMaskedLoads << "/" << numContMaskedStores << "\n"
           << "\tuni load/store: " << numUniLoads << "/" << numUniStores << ", masked " << numUniMaskedLoads << "/" << numUniMaskedStores << "\n"
           << "\tstore masks (c/u/v): " << numConstStoreMasks << "/" << numUniStoreMasks << "/" << numVarStoreMasks << "\n"
           << "\tload  masks (c/u/v): " << numConstLoadMasks << "/" << numUniLoadMasks << "/" << numVarLoadMasks << "\n";

#if 0
  // lazy statistics
  Report() << "GEPs/BCs\n";
  Report() << "GEPs: " << numVecGEPs << "/" << numScalGEPs << "/" << numInterGEPs << " vec/scal/inter\n";
  Report() << "BCs: " << numVecBCs << "/" << numScalBCs << " vec/scal\n";
  Report() << "\n";
#endif

  // call statistics
  Report() << "nat calls:\n"
           << "\tVectorized: " << numVecCalls << "/" << numSemiCalls << " fully/semi\n"
           << "\tReplicated: " << numFallCalls << "/" << numCascadeCalls << " replicated/cascaded\n"
           << "\tRV Intrinsics: " << numRVIntrinsics << " intrinsics\n";

#if 0
  // general statistics
  Report() << "Everything else\n";
  Report() << "Scalarized: " << numScalarized << " instructions\n";
  Report() << "Vectorized: " << numVectorized << " instructions\n";
  Report() << "Replicated: " << numFallbacked << " instructions\n";
  Report() << "Lazy Instructions: " << numLazy << " instructions\n";
  Report() << "\n";
#endif

  std::string fileName;
  if (!DumpStatistics(fileName))
    return;

  std::ofstream file;
  file.open(fileName, std::fstream::app);

  // header
  file << "Feature,Frequency\n";

  // memory statistics
  file << (config.useScatterGatherIntrinsics ? "masked-scatter," : "masked-casc-store,") << numMaskedScatter << "\n";
  file << (config.useScatterGatherIntrinsics ? "masked-gather," : "masked-casc-load,") << numMaskedGather << "\n";
  file << (config.useScatterGatherIntrinsics ? "scatter," : "cascade-store,") << numScatter << "\n";
  file << (config.useScatterGatherIntrinsics ? "gather," : "cascade-load,")  << numGather << "\n";
  file << "interleaved-masked-load," << numInterMaskedLoads << "\n";
  file << "interleaved-masked-store," << numInterMaskedStores << "\n";
  file << "interleaved-load," << numInterLoads << "\n";
  file << "interleaved-store," << numInterStores << "\n";
  file << "contiguous-masked-load," << numContMaskedLoads << "\n";
  file << "contiguous-masked-store," << numContMaskedStores << "\n";
  file << "contiguous-load," << numContLoads << "\n";
  file << "contiguous-store," << numContStores << "\n";
  file << "uniform-masked-load," << numUniMaskedLoads << "\n";
  file << "uniform-masked-store," << numUniMaskedStores << "\n";
  file << "uniform-load," << numUniLoads << "\n";
  file << "uniform-store," << numUniStores << "\n";

  // lazy statistics
  file << "vector-GEP," << numVecGEPs << "\n";
  file << "scalar-GEP," << numScalGEPs << "\n";
  file << "interleaved-GEP," << numInterGEPs << "\n";
  file << "vector-BC," << numVecBCs << "\n";
  file << "scalar-BC," << numScalBCs << "\n";

  // call statistics
  file << "vec-call," << numVecCalls << "\n";
  file << "semi-vec-call," << numSemiCalls << "\n";
  file << "replicated-call," << numFallCalls << "\n";
  file << "cascaded-call," << numCascadeCalls << "\n";
  file << "rv-intrinsic," << numRVIntrinsics << "\n";

  // general statistics
  file << "scalarized," << numScalarized << "\n";
  file << "vectorized," << numVectorized << "\n";
  file << "replicated," << numFallbacked << "\n";
  file << "lazy-instr," << numLazy << "\n";

  file.close();
}

VectorShape NatBuilder::getVectorShape(const Value &val) {
  if (vecInfo.hasKnownShape(val)) return vecInfo.getVectorShape(val);
  else return VectorShape::uni();
}

NatBuilder::NatBuilder(Config _config, PlatformInfo &_platInfo, VectorizationInfo &_vecInfo,
                       ReductionAnalysis & _reda, FunctionAnalysisManager &FAM) :
    builder(_vecInfo.getMapping().vectorFn->getContext()),
    config(_config),
    platInfo(_platInfo),
    vecInfo(_vecInfo),
    dominatorTree(FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction())),
    memDepRes(FAM.getResult<MemoryDependenceAnalysis>(vecInfo.getScalarFunction())),
    SE(FAM.getResult<ScalarEvolutionAnalysis>(vecInfo.getScalarFunction())),
    reda(_reda),
    undeadMasks(vecInfo, FAM),
    layout(_vecInfo.getScalarFunction().getParent()),
    i1Ty(IntegerType::get(_vecInfo.getMapping().vectorFn->getContext(), 1)),
    i32Ty(IntegerType::get(_vecInfo.getMapping().vectorFn->getContext(), 32)),
    vecMaskArg(nullptr),
    keepScalar(),
    cascadeLoadMap(),
    cascadeStoreMap(),
    vectorValueMap(),
    scalarValueMap(),
    basicBlockMap(),
    grouperMap(),
    phiVector(),
    lazyInstructions() {}

void NatBuilder::vectorize(bool embedRegion, ValueToValueMapTy * vecInstMap) {
  const Function *func = vecInfo.getMapping().scalarFn;
  Function *vecFunc = vecInfo.getMapping().vectorFn;

  IF_DEBUG_NAT {
    errs() << "-- status before vector codegen --\n";
    vecInfo.dump();
    reda.dump();
  }

  // map arguments first

  if (!vecInfo.getRegion().isVectorLoop()) {
    IF_DEBUG_NAT { errs() << "VecFuncType: " << *vecFunc->getFunctionType() << "\n"; }
    int i = 0;
    int shapeIdx = 0;
    auto sit = func->arg_begin();
    for (auto it = vecFunc->arg_begin(), et = vecFunc->arg_end();
         it != et; ++it, ++sit, ++i) {
      Argument *arg = &*it;
      if (vecInfo.getMapping().maskPos == i) {
        vecMaskArg = arg;
        --sit; // hacky way to skip the scalar arg increment
        continue;
      }

      const Argument *sarg = &*sit;
      arg->setName(sarg->getName());
      VectorShape argShape = vecInfo.getMapping().argShapes[shapeIdx++];
      IF_DEBUG_NAT { errs() << *sarg << " -> " << *arg << "\n"; }
      if (argShape.isVarying() && !arg->getType()->isPointerTy()) {
        mapVectorValue(sarg, arg);
      } else {
        mapScalarValue(sarg, arg);
      }
    }
  }

  IF_DEBUG_NAT if (vecMaskArg) {
    errs() << "VecMaskArg: " << *vecMaskArg << "\n";
  }

  // visit all memory instructions and check if we can scalarize their index calculation
  if (config.scalarizeIndexComputation)
    visitMemInstructions();

  // create all BasicBlocks first and map them
  for (auto &block : *func) {
    if (!vecInfo.inRegion(block)) continue;

    BasicBlock *vecBlock = BasicBlock::Create(vecFunc->getContext(), block.getName() + ".rv", vecFunc);
    mapVectorValue(&block, vecBlock);
  }

  // traverse dominator tree in pre-order to ensure all uses have definitions vectorized first
  std::deque<const DomTreeNode *> nodeQueue;
  const DomTreeNode *rootNode = dominatorTree.getNode(&vecInfo.getEntry());
  nodeQueue.push_back(rootNode);
  while (!nodeQueue.empty()) {
    // FIFO for pre-order
    const DomTreeNode *node = nodeQueue.front();
    nodeQueue.pop_front();

    // vectorize
    BasicBlock *bb = node->getBlock();
    if (!vecInfo.inRegion(*bb)) continue;

    BasicBlock *vecBlock = getVectorBlock(*bb, false);
    vectorize(bb, vecBlock);

    // populate queue with pre-order dominators
    for (const auto* Child : node->children()) {
      nodeQueue.push_back(Child);
    }
  }

  // revisit PHINodes now and add the mapped incoming values
  if (!phiVector.empty()) addValuesToPHINodes();

  // report statistics
  printStatistics();

  if (!vecInfo.getRegion().isVectorLoop()) return;

  // TODO what about outside uses?

  // register vector insts
  if (vecInstMap) {
    for (auto & BB : *vecFunc) {
      if (vecInfo.inRegion(BB)) {
        (*vecInstMap)[&BB] = getVectorBlock(BB, false);
      }
      for (auto & I : BB) {
        auto * vecInst = getVectorValue(I);
        if (vecInst) {
          (*vecInstMap)[&I] = vecInst;
        } else {
          (*vecInstMap)[&I] = getScalarValue(I, 0);
        }
      }
    }
  }

  if (!embedRegion) return;

  // rewire branches outside the region to go to the region instead
  std::vector<BasicBlock *> oldBlocks;
  for (auto &BB : *vecFunc) {
    if (vecInfo.inRegion(BB)) {
      oldBlocks.push_back(&BB);
      continue; // keep old region
    }
    auto &termInst = *BB.getTerminator();
    for (unsigned i = 0; i < termInst.getNumOperands(); ++i) {
      auto *termOp = termInst.getOperand(i);
      auto *branchTarget = dyn_cast<BasicBlock>(termOp);
      if (!branchTarget) continue;
      if (vecInfo.inRegion(*branchTarget)) {
        termInst.setOperand(i, getVectorBlock(*branchTarget, false));
      }
    }
  }

  // remove old region
  for (auto *oldBB : oldBlocks) {
    new UnreachableInst(oldBB->getContext(), oldBB);
    while (oldBB->size() > 1) {
      auto I = oldBB->begin();
      if (!I->getType()->isVoidTy())
        I->replaceAllUsesWith(UndefValue::get(I->getType()));
      I->eraseFromParent();
    }
    // TODO: LoopInfo (probably) keeps an asserting handle on the old loop
    //       header. Remove the old loop first!
    // oldBB->eraseFromParent();
  }

  IF_DEBUG_NAT {
    errs() << "-- Vectorized IR: --\n";
    for (auto *oldBB : oldBlocks) {
      Dump(*getVectorValue(*oldBB));
    }
    errs() << "-- End of Vectorized IR: --\n";
  }
}

void NatBuilder::vectorize(BasicBlock *const bb, BasicBlock *vecBlock) {
  assert(vecBlock && "no block to insert vector code");
  builder.SetInsertPoint(vecBlock);
  for (BasicBlock::iterator it = bb->begin(), ie = bb->end(); it != ie; ++it) {
    Instruction *inst = &*it;

    // generate all lazy instructions first if it is the terminator
    if (inst == bb->getTerminator()) {
      // vectorize any left-over lazy instructions
      if (!lazyInstructions.empty())
        requestLazyInstructions(lazyInstructions.back());
      assert(lazyInstructions.empty() && "not all lazy instructions vectorized!!");
    }

    PHINode *phi = dyn_cast<PHINode>(inst);
    LoadInst *load = dyn_cast<LoadInst>(inst);
    StoreInst *store = dyn_cast<StoreInst>(inst);
    CallInst *call = dyn_cast<CallInst>(inst);
    GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst);
    BitCastInst *bc = dyn_cast<BitCastInst>(inst);
    AllocaInst *alloca = dyn_cast<AllocaInst>(inst);
    AtomicRMWInst *atomicrmw = dyn_cast<AtomicRMWInst>(inst);

    // analyze memory predicate
    if (load || store) {
      bool memMaskConst = false;
      bool memMaskUni = false;
      bool memMaskDiv = false;
      auto * scaBlockPred = vecInfo.getPredicate(*bb);

      if (!scaBlockPred || isa<Constant>(*scaBlockPred)) {
        memMaskConst = true;
      } else if (scaBlockPred && vecInfo.getVectorShape(*scaBlockPred).isUniform()) {
        memMaskUni = true;
      } else {
        memMaskDiv = true;
      }

      if (load) {
        numConstLoadMasks += memMaskConst;
        numUniLoadMasks += memMaskUni;
        numVarLoadMasks += memMaskDiv;
      } else if (store) {
        numConstStoreMasks += memMaskConst;
        numUniStoreMasks += memMaskUni;
        numVarStoreMasks += memMaskDiv;
      }
    }

    // loads and stores need special treatment (masking, shuffling, etc) (build them lazily)
    if (canVectorize(inst) && (load || store))
      if (false) addLazyInstruction(inst);
      else vectorizeMemoryInstruction(inst);
    else if (store) {
      Value *predicate = vecInfo.getPredicate(*inst->getParent());
      bool needsMask = predicate && !vecInfo.getVectorShape(*predicate).isUniform();

      Value *accessedPtr = store->getPointerOperand();
      VectorShape addrShape = getVectorShape(*accessedPtr);

      if (needsMask && addrShape.isUniform()) {
        Value *storedValue = store->getValueOperand();
        Type *accessedType = storedValue->getType();
        Value *accessedPtr = store->getPointerOperand();
        Value *vecMem;

        llvm::Align alignment = llvm::Align(addrShape.getAlignmentFirst());
        alignment = std::max<llvm::Align>(alignment, store->getAlign());
        Value *addr = requestScalarValue(accessedPtr);
        VectorShape valShape = vecInfo.getVectorShape(*storedValue);
        Value *mask = requestVectorValue(predicate);

        if (!valShape.isUniform()) {
          Value *mappedStoredVal = requestVectorValue(storedValue);
          vecMem = createVaryingToUniformStore(store, accessedType, alignment, addr, needsMask ? mask : nullptr, mappedStoredVal);
        } else {
          Value *mappedStoredVal = addrShape.isUniform() ? requestScalarValue(storedValue)
                                                         : requestVectorValue(storedValue);

          vecMem = createUniformMaskedMemory(store, accessedType, alignment, addr, predicate, mask, mappedStoredVal);
        }
        mapScalarValue(inst, vecMem);
      } else if (needsMask || !addrShape.isUniform())
        replicateInstruction(inst);
      else
        copyInstruction(inst);
    } else if (call) {
      // calls need special treatment
      switch (GetIntrinsicID(*call)) {
        case RVIntrinsic::EntryMask: mapVectorValue(call, vecMaskArg); break;
        case RVIntrinsic::Any: vectorizeReductionCall(call, false); break;
        case RVIntrinsic::All: vectorizeReductionCall(call, true); break;
        case RVIntrinsic::Extract: vectorizeExtractCall(call); break;
        case RVIntrinsic::Insert: vectorizeInsertCall(call); break;
        case RVIntrinsic::Compact: vectorizeCompactCall(call); break;
        case RVIntrinsic::Mask: mapVectorValue(call, requestVectorPredicate(*call->getParent())); break;
        case RVIntrinsic::VecLoad: vectorizeLoadCall(call); break;
        case RVIntrinsic::VecStore: vectorizeStoreCall(call); break;
        case RVIntrinsic::Shuffle: vectorizeShuffleCall(call); break;
        case RVIntrinsic::Ballot: vectorizeBallotCall(call); break;
        case RVIntrinsic::PopCount: vectorizePopCountCall(call); break;
        case RVIntrinsic::Index: vectorizeIndexCall(*call); break;
        case RVIntrinsic::Align: vectorizeAlignCall(call); break;
        case RVIntrinsic::LaneID: vectorizeLaneIDCall(call); break;
        case RVIntrinsic::NumLanes: vectorizeNumLanesCall(call); break;
        default: {
          if (false) addLazyInstruction(inst);
          else {
            if (shouldVectorize(call)) vectorizeCallInstruction(call);
            else copyCallInstruction(call);
          }
        }
      }
    } else if (phi) {
      // phis need special treatment as they might contain not-yet mapped instructions
      vectorizePHIInstruction(phi);
    } else if (alloca && shouldVectorize(inst)) {
      vectorizeAlloca(alloca);
    } else if (gep || bc) {
      continue; // skipped
    } else if (atomicrmw) {
      vectorizeAtomicRMW(atomicrmw);
    } else if (canVectorize(inst) && shouldVectorize(inst)) {
      vectorizeInstruction(inst);
    } else if (!canVectorize(inst) && shouldVectorize(inst)){
      replicateInstruction(inst);
    } else {
      if (alloca) ++numUniAllocas;
      copyInstruction(inst);
    }
  }
}

ValVec
NatBuilder::scalarize(BasicBlock & scaBlock, Instruction & inst, bool packResult, std::function<Value*(IRBuilder<>&,size_t)> genFunc) {
  auto * vecTy = packResult ? FixedVectorType::get(inst.getType(), vectorWidth()) : nullptr;
  Value * accu = packResult ? UndefValue::get(vecTy) : nullptr;

  ValVec laneRepls;
  for (int lane = 0; lane < vectorWidth(); ++lane) {
    Value *cpInst = genFunc(builder, lane);
    laneRepls.push_back(cpInst);

    if (accu) accu = builder.CreateInsertElement(accu, cpInst, lane, "scalarized");
  }

  // register result if applicable
  bool producesValue = !inst.getType()->isVoidTy();
  if (producesValue) {
    if (accu) {
      mapVectorValue(&inst, accu);
    } else {
      for (int l = 0; l < vectorWidth(); ++l) {
        mapScalarValue(&inst, laneRepls[l], l);
      }
    }
  }

  return laneRepls;
}

Value&
NatBuilder::createAnyGuard(bool instNeedsGuard, BasicBlock & origBlock, Instruction & inst, bool producesValue, std::function<Value*(IRBuilder<>&)> genFunc) {
  auto * scalarMask = vecInfo.getPredicate(origBlock);
  // only emit a guard if rv_any(p) may be false AND the emitted instructions will need it.
  bool needsGuard = instNeedsGuard && !undeadMasks.isUndead(*scalarMask, origBlock);

  BasicBlock* memBlock, * continueBlock;

  // prologue (guard branch)
  if (needsGuard) {
    // create a mask ptest
    Value * anyMask = createPTest(requestVectorValue(scalarMask), false);

    // create two new basic blocks
    memBlock = BasicBlock::Create(vecInfo.getVectorFunction().getContext(), "mem_block",
                                              &vecInfo.getVectorFunction());
    continueBlock = BasicBlock::Create(vecInfo.getVectorFunction().getContext(), "cont_block",
                                                   &vecInfo.getVectorFunction());

    // conditionally branch to both
    builder.CreateCondBr(anyMask, memBlock, continueBlock);
    builder.SetInsertPoint(memBlock);
  }

  //  emit the actual access
  Value *vecMem = genFunc(builder);

  // epilogue (continue block, return value phi)
  if (needsGuard) {
    builder.CreateBr(continueBlock);
    builder.SetInsertPoint(continueBlock);

    BasicBlock *vecOrigBlock = getVectorBlock(origBlock, true);
    PHINode *phi = producesValue ? builder.CreatePHI(vecMem->getType(), 2, "scal_mask_mem_phi") : nullptr;

    if (phi) {
      phi->addIncoming(vecMem, memBlock);
      phi->addIncoming(UndefValue::get(vecMem->getType()), vecOrigBlock);
      vecMem = phi;
    }

    mapVectorValue(&origBlock, continueBlock);
  }

  return *vecMem;
}

ValVec
NatBuilder::scalarizeCascaded(BasicBlock & srcBlock, Instruction & inst, bool packResult, std::function<Value*(IRBuilder<>&,size_t)> genFunc) {
   // check if we need cascade first
   Value *predicate = vecInfo.getPredicate(srcBlock);
   assert(predicate && "expected predicate!");

   // packResult -> a single vector value
   // !packResult -> results of all replicated elements
   ValVec resultVec;

   // if we need cascading, we need the vectorized predicate and the cascading blocks
   std::vector<BasicBlock *> condBlocks;
   std::vector<BasicBlock *> maskedBlocks;

   // block cascade
   BasicBlock *vecBlock = getVectorBlock(srcBlock, true);
   BasicBlock *resBlock = createCascadeBlocks(vecBlock->getParent(), vectorWidth(), condBlocks, maskedBlocks);
   condBlocks.push_back(resBlock);

   // branch to our entry block of the cascade
   builder.CreateBr(condBlocks[0]);
   builder.SetInsertPoint(condBlocks[0]);

   // vector aggregate if packing was requested
   auto * vecTy = packResult ? FixedVectorType::get(inst.getType(), vectorWidth()) : nullptr;
   Value * accu = packResult ? UndefValue::get(vecTy) : nullptr;

   bool producesValue = !inst.getType()->isVoidTy();

   // create <vector_width> scalar calls
   for (int lane = 0; lane < vectorWidth(); ++lane) {
     auto * condBlock = condBlocks[lane];     // the block with the if
     auto * maskedBlock = maskedBlocks[lane]; // the guarded block (containing the scalarized instructino)
     auto * nextBlock = condBlocks[lane + 1]; // next guard block

     assert(builder.GetInsertBlock() == condBlock);

     // guard if
     Value *mask = requestScalarValue(predicate, lane, true); // do not map this value if it's fresh to avoid dominance violations
     builder.CreateCondBr(mask, maskedBlock, nextBlock);

   // materialize the scalarized block
     builder.SetInsertPoint(maskedBlock);

     // call user provided function to get a scalarized version of that instruction
     auto * repl = genFunc(builder, lane);
     auto * scaTy = repl->getType();

    // insert value still in maskedBlock
    Value * laneRes = nullptr;
    Type * transferTy = nullptr;
     if (producesValue) {
         if (packResult) {
           transferTy = vecTy;
           laneRes = builder.CreateInsertElement(accu, repl, ConstantInt::get(i32Ty, lane),
                                            "insert_lane_" + std::to_string(lane));
         } else {
           transferTy = scaTy;
           laneRes = repl;
         }
     }

    // set insert to next guard block
     builder.CreateBr(nextBlock);
     builder.SetInsertPoint(nextBlock);

     Value * mappedLaneVal = laneRes; // object that should identify the produced value
     if (producesValue) {
       // get a dominating definition (PHINode)
       PHINode *phi = builder.CreatePHI(transferTy, 2);

       if (packResult) {
          phi->addIncoming(accu, condBlock);
          accu = phi;
       } else {
          phi->addIncoming(UndefValue::get(scaTy), condBlock);
          mappedLaneVal = phi;
       }
       phi->addIncoming(laneRes, maskedBlock);
     }
     resultVec.push_back(mappedLaneVal);
   }

   // register result if applicable
   if (producesValue) {
     if (accu) {
       mapVectorValue(&inst, accu);
     } else {
       for (int l = 0; l < vectorWidth(); ++l) {
         mapScalarValue(&inst, resultVec[l], l);
       }
     }
   }

   // remap to tail block
   mapVectorValue(inst.getParent(), resBlock);

   return resultVec;
}

/// Request all the vector function arguments for scalling \p vecCall at the (vectorized) call site of \p scaCall.
// Stores all arranged vector-world arguments in \p vectorArgs.
void
NatBuilder::requestVectorCallArgs(CallInst & scaCall, Function & vecFunc, int maskPos, std::vector<Value*> & vectorArgs) {
  auto itVecArg = vecFunc.arg_begin();

  for (int vecIdx = 0, scaIdx = 0;
       vecIdx < (int) vecFunc.arg_size();
       ++vecIdx, ++itVecArg) {

    if (vecIdx == maskPos) {
      vectorArgs.push_back(requestVectorPredicate(*scaCall.getParent()));
      // the mask argument does not exist in the scalar function
    } else {
      Value *op = scaCall.getArgOperand(scaIdx);
      bool vecTypeArg = itVecArg->getType()->isVectorTy();
      Value *mappedArg = vecTypeArg ? requestVectorValue(op) : requestScalarValue(op);
      vectorArgs.push_back(mappedArg);
      ++scaIdx; // actually consuming a scalar argument
    }
  }
}

// FIXME re-design this!
/* expects that builder has valid insertion point set */
void NatBuilder::mapOperandsInto(Instruction *const scalInst, Instruction *inst, bool vectorizedInst,
                                 unsigned laneIdx) {
  assert(inst && "no instruction to map operands into");
  assert(scalInst && "no instruction to map operands from");
  assert(builder.GetInsertBlock() && "no insertion point set");

  // check for division. opcodes for divisions are (in order) UDiv, SDiv, FDiv. only care if non-trivial mask
  auto opCode = scalInst->getOpcode();
  auto * pred = vecInfo.getPredicate(*scalInst->getParent());
  bool isPredicatedDiv = (opCode >= BinaryOperator::UDiv) && (opCode <= BinaryOperator::FDiv) && (pred && !isa<Constant>(pred));

  unsigned e = inst->getNumOperands();
  for (unsigned i = 0; i < e; ++i) {
    Value *op = scalInst->getOperand(i);
    Value *mappedOp = (vectorizedInst || isa<BasicBlock>(op)) ? requestVectorValue(op) : requestScalarValue(op, laneIdx);

    // only have to deal with the 2nd operand
    if (config.useSafeDivisors && (isPredicatedDiv && i > 0)) {
      // create a select between mappedOp and neutral element vector (1)
      Value *neutralVec = vectorizedInst ? getConstantVector(vectorWidth(), op->getType(), 1)
                                         : (op->getType()->isFloatingPointTy() ? ConstantFP::get(op->getType(), 1)
                                                                              : ConstantInt::get(op->getType(), 1));
      Value *mask = vecInfo.getPredicate(*scalInst->getParent());
      mask = vectorizedInst ? requestVectorValue(mask) : requestScalarValue(mask, laneIdx);

      mappedOp = builder.CreateSelect(mask, mappedOp, neutralVec, "divSelect");
    }

    assert(mappedOp && "could not map operand");
    inst->setOperand(i, mappedOp);
  }
}

void NatBuilder::vectorizePHIInstruction(PHINode *const scalPhi) {
  assert(vecInfo.hasKnownShape(*scalPhi) && "no VectorShape for PHINode available!");
  VectorShape shape = getVectorShape(*scalPhi);
  Type *scalType = scalPhi->getType();
  Type *type = !shape.isVarying() || scalType->isVectorTy() || scalType->isStructTy() ?
               scalType : getVectorType(scalPhi->getType(), vectorWidth());
  auto name = !shape.isVarying() || scalType->isVectorTy() || scalType->isStructTy() ?
              scalPhi->getName() : scalPhi->getName() + "_SIMD";

  // replicate phi <vector_width> times if type is not vectorizable
  unsigned loopEnd = shape.isVarying() && (scalType->isVectorTy() || scalType->isStructTy()) ? vectorWidth() : 1;
  for (unsigned lane = 0; lane < loopEnd; ++lane) {
    PHINode *phi = builder.CreatePHI(type, scalPhi->getNumIncomingValues(), name);
    if (loopEnd == 1 && shape.isVarying())
      mapVectorValue(scalPhi, phi);
    else
      mapScalarValue(scalPhi, phi, lane);
  }
  phiVector.push_back(scalPhi);

  shape.isVarying() ? loopEnd == 1 ? ++numVectorized : ++numFallbacked : ++numScalarized;
}

/* expects that builder has valid insertion point set */
void NatBuilder::copyInstruction(Instruction *const inst, unsigned laneIdx) {
  assert(inst && "no instruction to copy");
  assert(builder.GetInsertBlock() && "no insertion point set");
  Instruction *cpInst = inst->clone();
  BranchInst *branch = dyn_cast<BranchInst>(cpInst);
  if (branch && vecInfo.hasKnownShape(*inst))
    assert(getVectorShape(*inst).isUniform() && "branch not uniform");
  if (branch && branch->isConditional()) {
    Value *cond = branch->getCondition();
    VectorShape shape = getVectorShape(*cond);
    cond = shape.isUniform() ? requestScalarValue(cond) : createPTest(requestVectorValue(cond), false);
    branch->setCondition(cond);

    for (unsigned i = 0; i < branch->getNumSuccessors(); ++i) {
      BasicBlock *succ = getVectorBlock(*branch->getSuccessor(i), false);
      branch->setSuccessor(i, succ);
    }
  } else {
    mapOperandsInto(inst, cpInst, false, laneIdx);
  }
  builder.Insert(cpInst, inst->getName());
  mapScalarValue(inst, cpInst, laneIdx);

  ++numScalarized; // statistics
}

static
bool
NeedsGuarding(Instruction & inst) {
  return inst.mayReadOrWriteMemory();

  // if (isa<CallInst>(inst)) return cast<CallInst>(inst).mayHaveSideEffects();
  // return isa<LoadInst>(inst) || isa<StoreInst>(inst) || isa<AtomicCmpXchgInst>(inst);
}

static
bool IsVectorizableTy(const Type & ty) {
  return ty.isPointerTy() || ty.isIntegerTy() || ty.isFloatingPointTy();
}

void NatBuilder::vectorizeAlloca(AllocaInst *const allocaInst) {
  auto allocAlign = allocaInst->getAlign();
  auto * allocTy = allocaInst->getAllocatedType();

  ++numSlowAllocas;

  auto name = allocaInst->getName();
  if (!allocaInst->isArrayAllocation()) {
    auto indexTy = getIndexTy(allocaInst);

    // TODO support array allocation
    // auto * scaElemCount = allocaInst->getArraySize();
    // if (getVectorShape(*scaElemCount).isUniform()) {
    // auto * vecElemCount = requestScalarValue(scaElemCount);
    auto * scaleFactor = ConstantInt::get(indexTy, vectorWidth());

    auto * baseAlloca = builder.CreateAlloca(allocTy, allocaInst->getType()->getAddressSpace(), scaleFactor, name + ".scaled_alloca");
    baseAlloca->setAlignment(llvm::Align(allocAlign));

    // extract basePtrs
    auto * offsetVec = createContiguousVector(vectorWidth(), indexTy, 0, 1);
    auto * allocaPtrVec = builder.CreateGEP(allocTy, baseAlloca, offsetVec, name + ".alloca_vec");

    // register as vectorized alloca
    mapVectorValue(allocaInst, allocaPtrVec);
    return;
  }

  // TODO implement a batter vectorization scheme

  // fallback code path
  replicateInstruction(allocaInst);
}

void NatBuilder::vectorizeAtomicRMW(AtomicRMWInst *const atomicrmw) {
  VectorShape shape = getVectorShape(*atomicrmw);
  if (shape.isStrided() || shape.isContiguous()) {
    Value *predicate = vecInfo.getPredicate(*atomicrmw->getParent());
    assert(predicate && predicate->getType()->isIntegerTy(1) && "predicate must have i1 type!");
    bool needsMask = predicate && !vecInfo.getVectorShape(*predicate).isUniform();
    assert(!needsMask && "Masked AtomicRMW instructions cannot be strided!");

    AtomicRMWInst *clonedinst = cast<AtomicRMWInst>(atomicrmw->clone());
    clonedinst->setOperand(0, requestScalarValue(atomicrmw->getPointerOperand()));

    Value * val = atomicrmw->getValOperand();
    IntegerType * valtype = cast<IntegerType>(val->getType());

    int stride = (atomicrmw->getOperation() == AtomicRMWInst::Sub) ? -1 * shape.getStride() : shape.getStride();
    int updateresult = vectorWidth() * stride;

    clonedinst->setOperand(1, ConstantInt::get(valtype, updateresult));

    builder.Insert(clonedinst, atomicrmw->getName());
    mapScalarValue(atomicrmw, clonedinst);
  } else if (shouldVectorize(atomicrmw)) {
    Value * ptr = atomicrmw->getPointerOperand();
    Value * val = atomicrmw->getValOperand();
    VectorShape ptrShape = getVectorShape(*ptr);

    if (!ptrShape.isUniform()) {
      replicateInstruction(atomicrmw);
      return;
    }

    Value *predicate = vecInfo.getPredicate(*atomicrmw->getParent());
    assert(predicate && predicate->getType()->isIntegerTy(1) && "predicate must have i1 type!");
    bool needsMask = predicate && !vecInfo.getVectorShape(*predicate).isUniform();
    if (needsMask && !(
                atomicrmw->getOperation() == AtomicRMWInst::Add  ||
                atomicrmw->getOperation() == AtomicRMWInst::Sub  ||
                atomicrmw->getOperation() == AtomicRMWInst::And  ||
                atomicrmw->getOperation() == AtomicRMWInst::Or   ||
                atomicrmw->getOperation() == AtomicRMWInst::Min  ||
                atomicrmw->getOperation() == AtomicRMWInst::Max  ||
                atomicrmw->getOperation() == AtomicRMWInst::UMin ||
                atomicrmw->getOperation() == AtomicRMWInst::UMax
                )) {
      replicateInstruction(atomicrmw);
      return;
    }

    switch (atomicrmw->getOperation()) {
    case AtomicRMWInst::Xchg: {
      assert(!needsMask);

      AtomicRMWInst *clonedInst = cast<AtomicRMWInst>(atomicrmw->clone());
      clonedInst->setOperand(0, requestScalarValue(ptr));

      Value *vectorizedVal = requestVectorValue(val);
      assert(vectorizedVal);
      Value *finalVal = builder.CreateExtractElement(vectorizedVal, vectorWidth() - 1);
      clonedInst->setOperand(1, finalVal);

      std::vector<Constant*> constants(vectorWidth(), nullptr);
      constants[0] = ConstantInt::get(builder.getInt32Ty(), 0);
      for (int i = 1; i < vectorWidth(); ++i) {
        Constant *constant = ConstantInt::get(builder.getInt32Ty(), i - 1);
        constants[i] = constant;
      }
      Value *constantvec = ConstantVector::get(constants);
      Value *shuffle = builder.CreateShuffleVector(vectorizedVal, vectorizedVal, constantvec);

      builder.Insert(clonedInst);
      Value *insertfinal = builder.CreateInsertElement(shuffle, clonedInst, (uint64_t) 0, atomicrmw->getName());

      mapVectorValue(atomicrmw, insertfinal);
      break;
    }
    case AtomicRMWInst::Add:
    case AtomicRMWInst::Sub:
    case AtomicRMWInst::And:
    case AtomicRMWInst::Or:
    case AtomicRMWInst::Max:
    case AtomicRMWInst::Min:
    case AtomicRMWInst::UMax:
    case AtomicRMWInst::UMin: {
      AtomicRMWInst *clonedInst = cast<AtomicRMWInst>(atomicrmw->clone());
      clonedInst->setOperand(0, requestScalarValue(ptr));

      Value *vectorizedVal = requestVectorValue(val);

      RedKind reduction = RedKind::Top;
      switch (atomicrmw->getOperation()) {
      case AtomicRMWInst::Add:
      case AtomicRMWInst::Sub:  reduction = RedKind::Add; break;
      case AtomicRMWInst::And:  reduction = RedKind::And; break;
      case AtomicRMWInst::Or:   reduction = RedKind::Or; break;
      case AtomicRMWInst::Max:  reduction = RedKind::SMax; break;
      case AtomicRMWInst::Min:  reduction = RedKind::SMin; break;
      case AtomicRMWInst::UMax: reduction = RedKind::UMax; break;
      case AtomicRMWInst::UMin: reduction = RedKind::UMin; break;
      default:
        llvm_unreachable("case missing");
      }

      if (needsMask) {
          //Fill masked lanes with neutral element for reduction.
          auto elemTy = vectorizedVal->getType()->getScalarType();
          Value *neutralElement = builder.CreateVectorSplat(vectorWidth(), &GetNeutralElement(reduction, *elemTy));

          auto *vecMask = maskInactiveLanes(requestVectorValue(predicate), atomicrmw->getParent(), false);
          vectorizedVal = builder.CreateSelect(vecMask, vectorizedVal, neutralElement);
      }

      Value *finalVal = &CreateVectorReduce(config, builder, reduction, *vectorizedVal, nullptr);

      clonedInst->setOperand(1, finalVal);
      builder.Insert(clonedInst);

      Value *itervector = UndefValue::get(vectorizedVal->getType());
      Value *itervar = clonedInst;
      itervector = builder.CreateInsertElement(itervector, itervar, (uint64_t) 0);
      for (int i  = 1; i < vectorWidth(); i++) {
        Value *update = builder.CreateExtractElement(vectorizedVal, (uint64_t) i - 1);
        switch (atomicrmw->getOperation()) {
        case AtomicRMWInst::Add:  itervar = builder.CreateAdd(itervar, update); break;
        case AtomicRMWInst::Sub:  itervar = builder.CreateSub(itervar, update); break;
        case AtomicRMWInst::And:  itervar = builder.CreateAnd(itervar, update); break;
        case AtomicRMWInst::Or:   itervar = builder.CreateOr(itervar, update); break;
        case AtomicRMWInst::Max:  itervar = createMinMaxOp(builder, RecurKind::SMax, itervar, update); break;
        case AtomicRMWInst::UMax: itervar = createMinMaxOp(builder, RecurKind::UMax, itervar, update); break;
        case AtomicRMWInst::Min:  itervar = createMinMaxOp(builder, RecurKind::SMin, itervar, update); break;
        case AtomicRMWInst::UMin: itervar = createMinMaxOp(builder, RecurKind::UMin, itervar, update); break;
        default:
          llvm_unreachable("case missing");
        }
        itervector = builder.CreateInsertElement(itervector, itervar, (uint64_t) i);
      }

      mapVectorValue(atomicrmw, itervector);
      break;
    }
    default:
      replicateInstruction(atomicrmw);
      break;
    }
  } else {
    copyInstruction(atomicrmw);
  }
}


void NatBuilder::replicateInstruction(Instruction *const inst) {
  // fallback for vectorizing varying instructions:
  // if !void: create result vector
  // clone instruction
  // map operands into instruction
  // if !void: insert into result vector
  // repeat from line 3 for all lanes
  Type *type = inst->getType();
  auto * mask = vecInfo.getPredicate(*inst->getParent());
  bool nonTrivialMask = mask && !isa<Constant>(mask);

  // scalarized operation with side effects in predicated context -> if cascade & scalarize
  //
  auto replFunc =  [this,inst](IRBuilder<> & builder, size_t lane) -> Value* {
          auto * cpInst = inst->clone();
          mapOperandsInto(inst, cpInst, false, lane);
          builder.Insert(cpInst, inst->getName());
          return cpInst;
        };

  bool packResult = IsVectorizableTy(*type);
  if (nonTrivialMask && NeedsGuarding(*inst)) {
    ValVec resVec = scalarizeCascaded(*inst->getParent(), *inst, packResult, replFunc);
  } else {
    scalarize(*inst->getParent(), *inst, packResult, replFunc);
  }

  ++numFallbacked;
}

/* expects that builder has valid insertion point set */
void NatBuilder::vectorizeInstruction(Instruction *const inst) {
  assert(inst && "no instruction to vectorize");
  assert(builder.GetInsertBlock() && "no insertion point set");
  Instruction *vecInst = inst->clone();

  if (!vecInst->getType()->isVoidTy())
    vecInst->mutateType(getVectorType(inst->getType(), vectorWidth()));

  mapOperandsInto(inst, vecInst, true);

  if (vecInst->getType()->isVoidTy() || !inst->hasName())
    builder.Insert(vecInst);
  else
    builder.Insert(vecInst, inst->getName() + "_SIMD");

  mapVectorValue(inst, vecInst);

  ++numVectorized;
}

void NatBuilder::vectorizeReductionCall(CallInst *rvCall, bool isRv_all) {
  assert(rvCall->arg_size() == 1 && "expected only 1 argument for rv_any");

  Value *predicate = rvCall->getArgOperand(0);
  const VectorShape &shape = getVectorShape(*predicate);
  assert((shape.isVarying() || shape.isUniform()) && "predicate can't be contigious or strided");

  Value *reduction;
#if 1
  Value *vecPredicate = maskInactiveLanes(requestVectorValue(predicate), rvCall->getParent(), isRv_all);
  reduction = createPTest(vecPredicate, isRv_all);
#else
  // Value *vecPredicate = maskInactiveLanes(requestVectorValue(predicate), rvCall->getParent(), isRv_all);
  auto * ballotVal = createVectorMaskSummary(requestVectorValue(predicate), builder, RVIntrinsic::Ballot); // FIXME block predicate
  if (isRv_all) {
    uint64_t mask = ((1 << vectorWidth()) - 1);
    reduction = builder.CreateICmpEQ(ballotVal, ConstantInt::get(ballotVal->getType(), mask, false)); // mask == FullMask
  } else {
    reduction = builder.CreateICmpNE(ballotVal, ConstantInt::get(ballotVal->getType(), 0, false)); // mask != 0
  }
#endif

  mapScalarValue(rvCall, reduction);

  ++numRVIntrinsics;
}

void
NatBuilder::vectorizeExtractCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 2 && "expected 2 arguments for rv_extract(vec, laneId)");

  Value *vecArg = rvCall->getArgOperand(0);

// uniform arg
  if (getVectorShape(*vecArg).isUniform()) {
    auto * uniVal = requestScalarValue(vecArg);
    mapScalarValue(rvCall, uniVal);
    return;
  }

// non-uniform arg
  auto * vecVal = requestVectorValue(vecArg);
  assert(getVectorShape(*rvCall->getArgOperand(1)).isUniform());
  auto * laneId = requestScalarValue(rvCall->getArgOperand(1));

  auto * laneVal = builder.CreateExtractElement(vecVal, laneId, "rv_ext");
  mapScalarValue(rvCall, laneVal);
}

void
NatBuilder::vectorizeInsertCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 3 && "expected 3 arguments for rv_insert(vec, laneId, value)");

  Value *vecArg  = rvCall->getArgOperand(0);
  assert(getVectorShape(*rvCall->getArgOperand(2)).isUniform());
  Value *elemVal = requestScalarValue(rvCall->getArgOperand(2));

// uniform arg
  if (getVectorShape(*vecArg).isUniform()) {
    mapScalarValue(rvCall, elemVal);
    return;
  }

// non-uniform arg
  auto * vecVal = requestVectorValue(vecArg);
  assert(getVectorShape(*rvCall->getArgOperand(1)).isUniform());
  auto * laneId = requestScalarValue(rvCall->getArgOperand(1));

  auto * insertVal = builder.CreateInsertElement(vecVal, elemVal, laneId, "rv_ins");
  mapVectorValue(rvCall, insertVal);
}

void
NatBuilder::vectorizeLoadCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 2 && "expected 2 arguments for rv_load(vecPtr, laneId)");

  Value *vecPtr = rvCall->getArgOperand(0);
  assert(getVectorShape(*rvCall->getArgOperand(1)).isUniform());
  auto * laneId = requestScalarValue(rvCall->getArgOperand(1));

// uniform arg
  Value * laneVal = nullptr;
  if (getVectorShape(*vecPtr).isUniform()) {
    auto * uniVal = requestScalarValue(vecPtr);
    auto addressSpace = uniVal->getType()->getPointerAddressSpace();
    auto * castPtr = builder.CreatePointerCast(uniVal, PointerType::get(builder.getFloatTy(), addressSpace));
    auto * gepPtr = builder.CreateGEP(builder.getFloatTy(), castPtr, laneId);
    laneVal = builder.CreateLoad(builder.getFloatTy(), gepPtr);
  } else {
// non-uniform arg
    auto * vecVal = requestVectorValue(vecPtr);
    auto * lanePtr = builder.CreateExtractElement(vecVal, laneId, "rv_load");
    auto * targetType = rvCall->getType();
    laneVal = builder.CreateLoad(targetType, lanePtr, "rv_load");
  }

  if (vecInfo.getVectorShape(*rvCall).isUniform()) {
    mapScalarValue(rvCall, laneVal);
  } else {
    // imprecise mapping
    auto & vecVal = widenScalar(*laneVal, VectorShape::uni());
    mapVectorValue(rvCall, &vecVal);
  }
}

void
NatBuilder::vectorizeStoreCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 3 && "expected 3 arguments for rv_store(vecPtr, laneId, value)");

  Value *vecPtr  = rvCall->getArgOperand(0);
  Value *elemVal = requestScalarValue(rvCall->getArgOperand(2));
  auto * laneId = requestScalarValue(rvCall->getArgOperand(1));

// uniform arg
  if (getVectorShape(*vecPtr).isUniform()) {
    auto * uniVal = requestScalarValue(vecPtr);
    auto addressSpace = uniVal->getType()->getPointerAddressSpace();
    auto * castPtr = builder.CreatePointerCast(uniVal, PointerType::get(builder.getFloatTy(), addressSpace));
    auto * gepPtr = builder.CreateGEP(builder.getFloatTy(), castPtr, laneId );
    auto * store = builder.CreateStore(elemVal, gepPtr);
    mapScalarValue(rvCall, store);
    return;
  }

// non-uniform arg
  auto * vecVal = requestVectorValue(vecPtr);
  auto * lanePtr = builder.CreateExtractElement(vecVal, laneId, "rv_store");
  auto * store = builder.CreateStore(elemVal, lanePtr);
  mapScalarValue(rvCall, store);
}

void
NatBuilder::vectorizeShuffleCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 2 && "expected 2 arguments for rv_shuffle(vec, shift)");

  Value *vecArg = rvCall->getArgOperand(0);

// uniform arg
  if (getVectorShape(*vecArg).isUniform()) {
    auto * uniVal = requestScalarValue(vecArg);
    mapScalarValue(rvCall, uniVal);
    return;
  }

// non-uniform arg
  auto * vecVal = requestVectorValue(vecArg);
  auto * amountVal = rvCall->getArgOperand(1);
  if (!isa<ConstantInt>(amountVal)) {
    Error() << *rvCall << "\n";
    fail("rv_shuffle: shift amount needs to be a constant!\n");
  }

  int64_t shiftVal = cast<ConstantInt>(amountVal)->getSExtValue();
  if (shiftVal < 0) {
    shiftVal = vectorWidth() + shiftVal;
  }

  // build shuffle indices
  SmallVector<int, 32> shflIds(vectorWidth());
  for (int i = 0; i < vectorWidth(); i++) {
    shflIds[i] = (i + shiftVal) % vectorWidth();
  }

  auto * shflVal = builder.CreateShuffleVector(vecVal, vecVal, shflIds, "rv_shfl");
  mapVectorValue(rvCall, shflVal);
}

Value*
NatBuilder::createVectorMaskSummary(Type & indexTy, Value * vecVal, IRBuilder<> & builder, RVIntrinsic mode) {
  Module *mod = vecInfo.getMapping().vectorFn->getParent();

  auto vecWidth = cast<FixedVectorType>(vecVal->getType())->getNumElements();
  auto * intVecTy = FixedVectorType::get(&indexTy, vecWidth);

  Value * result = nullptr;
  switch (mode) {
    case RVIntrinsic::Ballot: {
      // If SSE is available, but AVX and above are not, and the vector width is greater than 4, split the vector
      bool shouldSplitForISA = vecWidth > 4 && config.useSSE && !config.useAVX && !config.useAVX2 && !config.useAVX512;
      if (vecWidth > 8 || shouldSplitForISA) {
        // split up vector
        std::vector<Constant*> lowerLanes;
        std::vector<Constant*> higherLanes;
        uint32_t halfWidth = vecWidth / 2;
        for (uint32_t i = 0; i < halfWidth; ++i) {
          lowerLanes.push_back(ConstantInt::get(i32Ty, i, false));
          higherLanes.push_back(ConstantInt::get(i32Ty, halfWidth + i, false));
        }

        auto * lowerHalf = builder.CreateShuffleVector(vecVal, UndefValue::get(vecVal->getType()), ConstantVector::get(lowerLanes), "lowerLanes");
        auto * lowerBallot = createVectorMaskSummary(indexTy, lowerHalf, builder, mode);

        auto * upperHalf = builder.CreateShuffleVector(vecVal, UndefValue::get(vecVal->getType()), ConstantVector::get(higherLanes), "higherLanes");
        auto * upperBallot = createVectorMaskSummary(indexTy, upperHalf, builder, mode);

        // ballot(vecVal) =  upperHalf << (halfWidth) | lowerHalf
        auto * up = builder.CreateShl(upperBallot, ConstantInt::get(&indexTy, halfWidth, false));
        return builder.CreateOr(up, lowerBallot);
      }

      // AVX-specific code path
      if ((config.useSSE || config.useAVX || config.useAVX2 || config.useAVX512) && (vecWidth == 2 || vecWidth == 4 || vecWidth == 8)) {
      // non-uniform arg
        uint32_t bits = indexTy.getScalarSizeInBits();
        Intrinsic::ID id;
        switch (vecWidth) {
        case 2: id = Intrinsic::x86_sse2_movmsk_pd; bits = 64; break;
        case 4: id = Intrinsic::x86_sse_movmsk_ps; break;
        case 8: id = Intrinsic::x86_avx_movmsk_ps_256; break;
        default: fail("Unsupported vector width in ballot !"); abort();
        }

        auto * extVal = builder.CreateSExt(vecVal, FixedVectorType::get(builder.getIntNTy(bits), vecWidth), "rv_ballot");
        auto * simdVal = builder.CreateBitCast(extVal, FixedVectorType::get(bits == 32 ? builder.getFloatTy() : builder.getDoubleTy(), vecWidth), "rv_ballot");

        auto movMaskDecl = Intrinsic::getDeclaration(mod, id);
        result = builder.CreateCall(movMaskDecl, simdVal, "rv_ballot");

      } else {
        // generic emulating code path
        //
      // a[lane] == 1 << lane
        std::vector<Constant*> constants(vecWidth, nullptr);
        for (unsigned i = 0; i < vecWidth; ++i) {
          unsigned int val = 1 << i;
          Constant *constant = ConstantInt::get(&indexTy, val);
          constants[i] = constant;
        }
        auto * flagVec = ConstantVector::get(constants);
        auto * zeroVec = ConstantVector::getNullValue(intVecTy);

      // select (vecVal[l] ? a[l] : 0)
        auto * maskedLaneVec = builder.CreateSelect(vecVal, flagVec, zeroVec);

      // reduce_or
        result = &CreateVectorReduce(config, builder, RedKind::Or, *maskedLaneVec, nullptr);
      }
    } break;

    case RVIntrinsic::PopCount: {
      if (config.useAVX || config.useAVX2) {
        // ISPC popcount pattern
        auto maskIntTy = builder.getIntNTy(vectorWidth());
        auto maskBitCast = builder.CreateBitCast(vecVal, maskIntTy);
        auto maskZExt = builder.CreateZExt(maskBitCast, &indexTy);
        auto ctPopFunc = Intrinsic::getDeclaration(mod, Intrinsic::ctpop, &indexTy); // FIXME use a larger type (what should happen for <4096 x i8>)??
        result = builder.CreateCall(ctPopFunc, {maskZExt}, "rv_popcount");

      } else {
        auto * maskedOnes = builder.CreateZExt(vecVal, intVecTy, "rv_zext");
        result = &CreateVectorReduce(config, builder, RedKind::Add, *maskedOnes, nullptr);
      }
    } break;

    default: abort();
  }

  return result;
}

void
NatBuilder::vectorizeBallotCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 1 && "expected 1 argument for rv_ballot(cond)");

  Value *condArg = rvCall->getArgOperand(0);

// non-uniform arg
  auto * vecVal = maskInactiveLanes(requestVectorValue(condArg), rvCall->getParent(), false);
  auto * mask = createVectorMaskSummary(*rvCall->getType(), vecVal, builder, RVIntrinsic::Ballot);
  mapScalarValue(rvCall, mask);
}

void
NatBuilder::vectorizeIndexCall(CallInst & rvCall) {
  ++numRVIntrinsics;

  auto vecWidth = vecInfo.getVectorWidth();
  assert(rvCall.arg_size() == 1 && "expected 1 argument for rv_index(mask)");
  Value *condArg = rvCall.getArgOperand(0);

  auto * intLaneTy = IntegerType::getIntNTy(rvCall.getContext(), 512 / vecWidth);
  bool argUniform = hasUniformPredicate(*rvCall.getParent()) && vecInfo.getVectorShape(*condArg).isUniform();

// uniform arg
  if (argUniform) {
    mapScalarValue(&rvCall, createContiguousVector(vecWidth, intLaneTy, 0, 1));
    return;
  }

// avx512vl - expand based implementation
  if (config.useAVX512 && (vecWidth == 4 || vecWidth == 8)) {
    Intrinsic::ID id = Intrinsic::x86_avx512_mask_expand;

    auto * maskVec = maskInactiveLanes(requestVectorValue(condArg), rvCall.getParent(), false);
    auto * contVec = createContiguousVector(vecWidth, intLaneTy, 0, 1);


    auto * fpLaneTy = Type::getDoubleTy(rvCall.getContext());
    auto * fpVecTy = FixedVectorType::get(fpLaneTy, vecWidth);
    auto * intVecTy = FixedVectorType::get(intLaneTy, vecWidth);

    auto * fpValVec = builder.CreateBitCast(contVec, fpVecTy);

    auto * expandDecl = Intrinsic::getDeclaration(rvCall.getParent()->getParent()->getParent(), id, {});

//flatten mask (<W x i1> --> <iW>)
    auto * flatMaskTy = Type::getIntNTy(rvCall.getContext(), vecWidth);
    auto * flatMask = builder.CreateBitCast(maskVec, flatMaskTy, "flatmask");

// call expand
    auto * expandedVec = builder.CreateCall(expandDecl, {fpValVec, Constant::getNullValue(fpVecTy), flatMask}, "psum_bits");

    auto * indexVec = builder.CreateBitCast(expandedVec, intVecTy, "bc_ivec");

    mapVectorValue(&rvCall, indexVec);
    return;
  } else {
// generic implementation
    auto * maskVec = maskInactiveLanes(requestVectorValue(condArg), rvCall.getParent(), false);

    llvm::Value * CacheVal = ConstantInt::get(rvCall.getType(), 0);
    auto * OneVal = ConstantInt::get(rvCall.getType(), 1);

    auto replFunc =  [vecWidth,&CacheVal,OneVal,maskVec](IRBuilder<> & builder, size_t lane) -> Value* {
      auto * OldVal = CacheVal;
      if (lane < vecWidth - 1) {
        auto * val = builder.CreateExtractElement(maskVec, ConstantInt::get(Type::getInt32Ty(builder.getContext()), lane));
        auto * addValue = builder.CreateAdd(CacheVal, OneVal);
        auto * NewVal = builder.CreateSelect(val, addValue, CacheVal);
        CacheVal = NewVal;
      }
      return OldVal;
    };

    ValVec resVec = scalarizeCascaded(*rvCall.getParent(), rvCall, true, replFunc);
    return;
  }
}

void
NatBuilder::vectorizePopCountCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  auto indexTy = rvCall->getType();

  assert(rvCall->arg_size() == 1 && "expected 1 argument for rv_ballot(cond)");

  Value *condArg = rvCall->getArgOperand(0);
  auto vecWidth = vecInfo.getVectorWidth();

// uniform arg
  if (getVectorShape(*condArg).isUniform()) {
    auto * uniVal = requestScalarValue(condArg);
    uniVal = builder.CreateSExt(uniVal, indexTy, "rv_popcount");
    uniVal = builder.CreateAnd(uniVal, ConstantInt::get(indexTy, vecWidth, false));
    mapScalarValue(rvCall, uniVal);
    return;
  }

  // FIXME mask out inactive threads also for uniform mask
  auto * vecVal = maskInactiveLanes(requestVectorValue(condArg), rvCall->getParent(), false);
  auto * mask = createVectorMaskSummary(*rvCall->getType(), vecVal, builder, RVIntrinsic::PopCount);
  mapScalarValue(rvCall, mask);
}

void
NatBuilder::vectorizeAlignCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 2 && "expected 2 arguments for rv_align(ptr, alignment)");

  Value *vecArg = rvCall->getArgOperand(0);

  if (getVectorShape(*vecArg).isVarying())
    mapVectorValue(rvCall, requestVectorValue(vecArg));
  else
    mapScalarValue(rvCall, requestScalarValue(vecArg));
}

void
NatBuilder::vectorizeLaneIDCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 0 && "expected 0 arguments for rv_lane_id()");
  Value *contVec = createContiguousVector(vectorWidth(), rvCall->getType(), 0, 1);
  mapVectorValue(rvCall, contVec);
}

void
NatBuilder::vectorizeNumLanesCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 0 && "expected 0 arguments for rv_num_lanes()");
  mapScalarValue(rvCall, ConstantInt::get(rvCall->getType(), vectorWidth(), false));
}

void
NatBuilder::vectorizeCompactCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->arg_size() == 2 && "expected 2 arguments for rv_compact(vec, mask)");

  Value *vecArg  = rvCall->getArgOperand(0);
  Value *maskArg = rvCall->getArgOperand(1);

// uniform arg
  if (getVectorShape(*vecArg).isUniform()) {
    mapScalarValue(rvCall, vecArg);
    return;
  }

// non-uniform arg
  auto * vecVal  = requestVectorValue(vecArg);
  auto * maskVal = requestVectorValue(maskArg);

  auto vecWidth = cast<FixedVectorType>(maskVal->getType())->getNumElements();
  auto tableIndex = createVectorMaskSummary(*rvCall->getType(), maskVal, builder, RVIntrinsic::Ballot);
  auto table = createCompactLookupTable(vecWidth);
  auto intVecType = FixedVectorType::get(builder.getInt32Ty(), vecWidth);
  auto intVecArrayType = ArrayType::get(intVecType, vecWidth * vecWidth);
  auto gepPtr = builder.CreateInBoundsGEP(intVecArrayType, table, { builder.getInt32(0), tableIndex });
  auto indices = builder.CreateLoad(intVecType, gepPtr, "rv_compact_indices");
  Value * compacted = UndefValue::get(vecVal->getType());
  for (size_t i = 0; i < vecWidth; ++i) {
    auto index = builder.CreateExtractElement(indices, builder.getInt32(i), "rv_compact_index");
    auto elem = builder.CreateExtractElement(vecVal, index, "rv_compact_elem");
    compacted = builder.CreateInsertElement(compacted, elem, builder.getInt32(i), "rv_compact");
  }
  mapVectorValue(rvCall, compacted);
}

Constant*
NatBuilder::createCompactLookupTable(unsigned vecWidth) {
  assert(vecWidth <= 8);
  auto module = vecInfo.getVectorFunction().getParent();
  auto tableName = "rv_compact_lookup_table" + std::to_string(vecWidth);
  auto global = module->getGlobalVariable(tableName);
  if (global)
    return global;

  // for all possible mask values
  auto elemTy = builder.getInt32Ty();
  std::vector<Constant*> tableElems;
  std::vector<Constant*> elemValues(vecWidth);
  for (unsigned i = 0; i < (1u << vecWidth); ++i) {
    for (unsigned j = 0; j < vecWidth; ++j)
      elemValues[j] = ConstantInt::get(elemTy, j);
    for (unsigned j = 0, k = 0; j < vecWidth; ++j) {
      if (i & (1 << j)) {
        elemValues[k] = ConstantInt::get(elemTy, j);
        k++;
      }
    }
    // generate a lookup table element
    tableElems.push_back(ConstantVector::get(elemValues));
  }
  auto tableValue = ConstantArray::get(ArrayType::get(tableElems[0]->getType(), tableElems.size()), tableElems);
  auto table = cast<GlobalVariable>(module->getOrInsertGlobal(tableName, tableValue->getType()));
  table->setConstant(true);
  table->setInitializer(tableValue);
  return table;
}

static
bool
MayRecurse(const Function &F) {
  return !F.doesNotRecurse();
}

static void
CopyTargetAttributes(Function & destFunc, Function & srcFunc) {
  auto attribSet = srcFunc.getAttributes().getFnAttrs();

  // parse SIMD signatures
  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    if ((attribText == "target-cpu") ||
       (attribText == "target-features"))
    {
      destFunc.addFnAttr(attrib);
    }
  }
}

void
NatBuilder::vectorizeCallInstruction(CallInst *const scalCall) {
  auto & scaBlock = *scalCall->getParent();
  bool hasCallPredicate = !hasUniformPredicate(scaBlock);

  Value * callee = scalCall->getCalledOperand();
  StringRef calleeName = callee->getName();
  Function * calledFunction = dyn_cast<Function>(callee);

  VectorShapeVec callArgShapes;
  for (int i = 0; i < (int) scalCall->arg_size(); ++i) {
    auto argShape = vecInfo.getVectorShape(*scalCall->getArgOperand(i));
    callArgShapes.push_back(argShape);
  }

// this is a workaround to avoid predicated lifetime markers
  if (calledFunction && hasCallPredicate) {
    switch (calledFunction->getIntrinsicID()) {
      default: break;
      case Intrinsic::lifetime_start:
      case Intrinsic::lifetime_end:
        Report() << "nat: dropped divergent lifetime marker!\n";
        return;
    }
  }

// Vectorize this function using a resolver provided vector function.
  auto & scaMask = *vecInfo.getPredicate(scaBlock);
  std::unique_ptr<FunctionResolver> funcResolver = nullptr;
  if (calledFunction) funcResolver = platInfo.getResolver(calledFunction->getName(), *calledFunction->getFunctionType(), callArgShapes, vectorWidth(), hasCallPredicate);
  if (funcResolver && !CheckFlag("RV_SPLIT")) {
    Function &simdFunc = funcResolver->requestVectorized();
    CopyTargetAttributes(simdFunc, vecInfo.getScalarFunction());

    bool needsGuardedCall =
      funcResolver->getCallSitePredicateMode() != CallPredicateMode::SafeWithoutPredicate &&
      hasCallPredicate && // call site with a non trivial predicate
      MayRecurse(simdFunc) && // the called function may actually recurse
      !undeadMasks.isUndead(scaMask, scaBlock); // there is not at least one live thread

    const int maskPos = funcResolver->getMaskPos();
    std::vector<Value*> vectorArgs;

    // request the vector arguments within the current scope
    requestVectorCallArgs(*scalCall, simdFunc, maskPos, vectorArgs);
    bool producesValue = !scalCall->getType()->isVoidTy();

    std::string callName = producesValue ? scalCall->getName().str() + ".rv" : "";

    auto & vecCall = createAnyGuard(needsGuardedCall, *scalCall->getParent(), *scalCall, producesValue,
      [&](IRBuilder<> & builder) {
        auto *call = builder.CreateCall(&simdFunc, vectorArgs, callName);
        call->setCallingConv(simdFunc.getCallingConv());
        return call;
      });

    if (producesValue) { vecCall.setName(scalCall->getName() + ".mapped"); }
    mapVectorValue(scalCall, &vecCall);
    ++numVecCalls;

  } else {
// Otw, try to semi-vectorize the call by replication a smaller vectorized version
    // TODO (this is a deprecated code path, we should use a ResolverService for this)
    unsigned vecWidth = vectorWidth() / 2;
    std::unique_ptr<FunctionResolver> funcResolver = nullptr;
    for (; calledFunction && vecWidth >= 2; vecWidth /= 2) {
      // FIXME update alignment in callArgShapes
      funcResolver = platInfo.getResolver(calleeName, *calledFunction->getFunctionType(), callArgShapes, vecWidth, hasCallPredicate);
      if (funcResolver) break;
    }
    bool replicate = !funcResolver;

    if (!replicate) {
      unsigned replicationFactor = vectorWidth() / vecWidth;
      //bool doublePrecision = false;
      //if (scalCall->arg_size() > 0) {
        // doublePrecision = scalCall->getArgOperand(0)->getType()->isDoubleTy();
      // }
      Function &simdFunc = funcResolver->requestVectorized();
      CopyTargetAttributes(simdFunc, vecInfo.getScalarFunction());

      ShuffleBuilder appender(vectorWidth());
      ShuffleBuilder extractor(vecWidth);

      // prepare the extract shuffler
      for (unsigned i = 0; i < scalCall->arg_size(); ++i) {
        Value *const arg = scalCall->getArgOperand(i);
        Value *mappedArg = requestVectorValue(arg);
        extractor.add(mappedArg);
      }

      // replicate vector call
      for (unsigned i = 0; i < replicationFactor; ++i) {
        CallInst *call = cast<CallInst>(scalCall->clone());
        call->mutateType(simdFunc.getReturnType());
        call->setCalledFunction(&simdFunc);
        call->setCallingConv(simdFunc.getCallingConv());

        // insert arguments into call
        for (unsigned j = 0; j < scalCall->arg_size(); ++j) {
          Value *vecArg = extractor.extractVector(builder, j, i * vecWidth);
          call->setArgOperand(j, vecArg);
        }
        builder.Insert(call);
        appender.add(call);
      }

      // append all sub-results to one vector and map
      Value *append = appender.append(builder);
      mapVectorValue(scalCall, append);

      ++numSemiCalls;
      return;
    }

// fallback to replication
    // check if we need cascade first
    Value *predicate = vecInfo.getPredicate(*scalCall->getParent());
    assert(predicate && "expected predicate!");
    assert(predicate->getType()->isIntegerTy(1) && "predicate must be i1 type!");
    bool needCascade = !isa<Constant>(predicate) && scalCall->mayHaveSideEffects();

    // scalar replication function
    auto replFunc = [this,scalCall](IRBuilder<> & builder, size_t lane) -> Value* {
          auto * cpInst = scalCall->clone();
          mapOperandsInto(scalCall, cpInst, false, lane);
          builder.Insert(cpInst, scalCall->getName());
          return cpInst;
        };

    Type *callType = scalCall->getType();
    bool packResult = IsVectorizableTy(*callType);

    ValVec resVec;
    if (needCascade) {
      resVec = scalarizeCascaded(*scalCall->getParent(), *scalCall, packResult, replFunc);
    } else {
      resVec = scalarize(*scalCall->getParent(), *scalCall, packResult, replFunc);
    }

    // if we need cascading, we need the vectorized predicate and the cascading blocks
    needCascade ? ++numCascadeCalls : ++numFallCalls;
  }
}

void NatBuilder::copyCallInstruction(CallInst *const scalCall, unsigned laneIdx) {
  // copying call instructions:
  // 1) get scalar callee
  // 2) construct arguments
  // 3) create call instruction
  auto *callee = scalCall->getCalledOperand();

  std::vector<Value *> args;
  for (unsigned i = 0; i < scalCall->arg_size(); ++i) {
    Value *scalArg = scalCall->getArgOperand(i);
    Value *laneArg = requestScalarValue(scalArg, laneIdx);
    args.push_back(laneArg);
  }

  // FIXME transfer fast math MD
  Value *call = builder.CreateCall(scalCall->getFunctionType(), callee, args, scalCall->getName());
  mapScalarValue(scalCall, call, laneIdx);

  ++numScalarized;
}


RedKind
NatBuilder::matchMemoryReduction(Value * scaPtr, Value * scaValue, Value *& oPayload, Instruction *& oScaLoad) {
   auto *scaInst = dyn_cast<Instruction>(scaValue);
   if (!scaInst) return RedKind::Top;

   // reducable operator?
   auto redKind = InferInstRedKind(*scaInst);
   if (redKind == RedKind::Top) return RedKind::Top;

   // one operand has to be a load
   auto lhsLoad = dyn_cast<LoadInst>(scaInst->getOperand(0));
   auto rhsLoad = dyn_cast<LoadInst>(scaInst->getOperand(1));
   if (!lhsLoad && !rhsLoad) return RedKind::Top;

   auto scaLoad = lhsLoad ? lhsLoad : rhsLoad;
   oScaLoad = scaLoad;
   if (lhsLoad) {
     oPayload = scaInst->getOperand(1);
   } else {
     oPayload = scaInst->getOperand(0);
   }

   // is this a "load ptr -> reduce -> store" ptr chain?
   auto scaLoadPtr = scaLoad->getPointerOperand();
   if (scaLoadPtr == scaPtr) return redKind;
   return RedKind::Top;
}

static Value*
GetUnderlyingAlloca(Value * ptr) {
  std::set<Value*> seen;
  if (!seen.insert(ptr).second) return nullptr;

// allocas
  auto * alloca = dyn_cast<AllocaInst>(ptr);
  if (alloca) return alloca;

// geps
  auto * gep = dyn_cast<GetElementPtrInst>(ptr);
  if (gep) return GetUnderlyingAlloca(gep->getPointerOperand());

// default
  return nullptr;
}

void NatBuilder::vectorizeMemoryInstruction(Instruction *const inst) {
  if (keepScalar.count(inst)) {
    return replicateInstruction(inst);
  }

  LoadInst *load = dyn_cast<LoadInst>(inst);
  StoreInst *store = dyn_cast<StoreInst>(inst);

  // need value, pointer and type of the access to decide what kind of load/store we need
  Value *storedValue = nullptr;
  Value *accessedPtr = nullptr;
  Type *accessedType = nullptr;
  if (load) {
    accessedType = load->getType();
    accessedPtr = load->getPointerOperand();
  } else {
    assert(store);
    storedValue = store->getValueOperand();
    accessedType = storedValue->getType();
    accessedPtr = store->getPointerOperand();
  }

  assert(cast<PointerType>(accessedPtr->getType())->isOpaqueOrPointeeTypeMatches(accessedType) &&
         "accessed type and pointed object type differ!");
  assert(vecInfo.hasKnownShape(*accessedPtr) && "no shape for accessed pointer!");
  VectorShape addrShape = getVectorShape(*accessedPtr);
  Type *vecType = getVectorType(accessedType, vectorWidth());

  Value *mask = nullptr;
  Value *predicate = vecInfo.getPredicate(*inst->getParent());
  assert(predicate && predicate->getType()->isIntegerTy(1) && "predicate must have i1 type!");
  bool needsMask = predicate && !vecInfo.getVectorShape(*predicate).isUniform();

  // uniform loads from allocations do not need a mask!
  auto alloca = GetUnderlyingAlloca(accessedPtr);
  if (needsMask && load && alloca && vecInfo.getVectorShape(*alloca).isUniform()) {
    needsMask = false;
  }

  if (needsMask)
    mask = requestVectorValue(predicate);
  else
    mask = getConstantVector(vectorWidth(), i1Ty, 1);

  // generate the address for the memory instruction now
  // uniform: uniform GEP
  // contiguous: contiguous GEP
  // interleaved: multiple contiguous GEPs
  // varying: varying vector GEP

  std::vector<Value *> addr;
  std::vector<Type *> addrTypess;
  std::vector<Value *> srcs;
  std::vector<Value *> masks;
  llvm::Align alignment;
  bool interleaved = false;
  uint64_t byteSize = static_cast<uint64_t>(layout.getTypeStoreSize(accessedType));

  if (addrShape.isUniform()) {
    // scalar access
    addr.push_back(requestScalarValue(accessedPtr));
    addrTypess.push_back(accessedType);
    alignment = llvm::Align(addrShape.getAlignmentFirst());

  } else if ((addrShape.isContiguous() || addrShape.isStrided(byteSize)) && !(needsMask && !config.enableMaskedMove)) {
    // cast pointer to vector-width pointer
    Value *ptr = requestScalarValue(accessedPtr);
    auto & ptrTy = *cast<PointerType>(ptr->getType());
    PointerType *vecPtrType = vecType->getPointerTo(ptrTy.getAddressSpace());
    addr.push_back(builder.CreatePointerCast(ptr, vecPtrType, "vec_cast"));
    addrTypess.push_back(vecType);
    alignment = llvm::Align(addrShape.getAlignmentFirst());

  } else if ((addrShape.isStrided() && isInterleaved(inst, accessedPtr, byteSize, srcs)) && !(needsMask && !config.enableMaskedMove)) {
    // interleaved access. ptrs: base, base+vector, base+2vector, ...
    Value *srcPtr = getPointerOperand(cast<Instruction>(srcs[0]));
    for (unsigned i = 0; i < srcs.size(); ++i) {
      Value *ptr = requestInterleavedAddress(srcPtr, i, vecType);
      addr.push_back(ptr);
      addrTypess.push_back(accessedType);
      if (needsMask)
        masks.push_back(mask);
    }
    alignment = llvm::Align(addrShape.getAlignmentFirst());
    interleaved = true;

  } else {
    addr.push_back(requestVectorValue(accessedPtr));
    addrTypess.push_back(vecType);
    alignment = llvm::Align(addrShape.getAlignmentGeneral());
  }

  llvm::Align origAlignment = load ? load->getAlign() : store->getAlign();
  alignment = std::max<llvm::Align>(alignment, origAlignment);

  Value *vecMem = nullptr;
  if (load) {
    if (needsMask && addrShape.isUniform()) {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      vecMem = createUniformMaskedMemory(load, accessedType, alignment, addr[0], predicate, mask, nullptr);

    } else if (((addrShape.isUniform() || addrShape.isContiguous() || addrShape.isStrided(byteSize))) && !(needsMask && !config.enableMaskedMove)) {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      assert(addrTypess.size() == 1 && "multiple address types for single access!");

      auto targetType = addrTypess[0];
      vecMem = createContiguousLoad(targetType, addr[0], alignment, needsMask ? mask : nullptr, UndefValue::get(vecType));

      addrShape.isUniform() ? ++numUniLoads : needsMask ? ++numContMaskedLoads : ++numContLoads;

    } else if (interleaved && !(needsMask && !config.enableMaskedMove)) {
      assert(addr.size() > 1 && "only one address for multiple accesses!");
      createInterleavedMemory(load->getType(), vecType, alignment, &addr, &masks, nullptr, &srcs);

    } else {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      vecMem = createVaryingMemory(vecType, alignment, addr[0], mask, nullptr);
    }


  } else {
    // store
    if (needsMask && addrShape.isUniform()) {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      auto valShape = vecInfo.getVectorShape(*store->getValueOperand());
      if (!valShape.isUniform()) {
        Value *mappedStoredVal = requestVectorValue(storedValue);
        vecMem = createVaryingToUniformStore(store, accessedType, alignment, addr[0], needsMask ? mask : nullptr, mappedStoredVal);
      } else {
        Value *mappedStoredVal = addrShape.isUniform() ? requestScalarValue(storedValue)
                                                       : requestVectorValue(storedValue);

        vecMem = createUniformMaskedMemory(store, accessedType, alignment, addr[0], predicate, mask, mappedStoredVal);
      }

    } else if (((addrShape.isUniform() || addrShape.isContiguous() || addrShape.isStrided(byteSize))) && !(needsMask && !config.enableMaskedMove)) {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      auto valShape = getVectorShape(*storedValue);

      // memory reduction pattern "*p = varyingValue" // FIXME generalize (eg for uniform inputs, etc)
      Value * mappedStoredVal = nullptr;
      if (addrShape.isUniform() && !valShape.isUniform()) {
        Instruction * scaOldLoad = nullptr;
        Value * scaPayload = nullptr;
        RedKind memRed = matchMemoryReduction(accessedPtr, storedValue, scaPayload, scaOldLoad);
        if (memRed == RedKind::Top) {
          errs() << *inst << "\n";
          errs() << *storedValue << " : " << valShape.str() << "\n";
          errs() << "ERROR: Storing a non-uniform value to a uniform pointer but could not identify reduction pattern!\n";
          abort();
        }

        auto vecOldLoad = requestScalarValue(scaOldLoad);
        auto vecPayload = requestVectorValue(scaPayload);
        mappedStoredVal = &CreateVectorReduce(config, builder, memRed, *vecPayload, vecOldLoad);
      } else {
        mappedStoredVal = addrShape.isUniform() ? requestScalarValue(storedValue)
                                                : requestVectorValue(storedValue);
      }
      vecMem = createContiguousStore(mappedStoredVal, addr[0], alignment, needsMask ? mask : nullptr);

      addrShape.isUniform() ? ++numUniStores : needsMask ? ++numContMaskedStores : ++numContStores;

    } else if (interleaved && !(needsMask && !config.enableMaskedMove)) {
      assert(addr.size() > 1 && "only one address for multiple accesses!");
      std::vector<Value *> vals;
      vals.reserve(addr.size());
      for (unsigned i = 0; i < addr.size(); ++i) {
        Value *srcVal = cast<StoreInst>(srcs[i])->getValueOperand();
        Value *val = requestVectorValue(srcVal);
        vals.push_back(val);
      }
      createInterleavedMemory(store->getValueOperand()->getType(), vecType, alignment, &addr, &masks, &vals, &srcs);

    } else {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      Value *mappedStoredVal = addrShape.isUniform() ? requestScalarValue(storedValue)
                                                       : requestVectorValue(storedValue);
      vecMem = createVaryingMemory(vecType, alignment, addr[0], mask, mappedStoredVal);
    }
  }


  // interleaved case creates mapping
  if (!interleaved) {
    if (addrShape.isUniform()) {
      bool impreciseLoad = load && vecInfo.getVectorShape(*load).isVarying();
      if (impreciseLoad) {
        // loads and stores can have a uniform pointer but produce a varying result shape
        // this is usually an artifact of SROV (if the VA is not re-run afterwards to make shapes more precise..)
        // v = insertvalue(undef, 0, %unifomPtr) : uniform
        // v1 = inservalue(%v, 1, %varyingValue) : varying
        // ...
        // %notActuallyVaryingPtr = extractvalue(%v1, 0) : varying  <--
        // ...
        // %x = load %notActuallyVaryingPtr // before SROC
        // %x = load %v1 // after SROC
        Report() << "nat: warning: load from uniform ptr with varing shape! " << *load << "\n";
        for (int i = 0; i < vectorWidth(); ++i) {
          mapScalarValue(inst, vecMem, i);
        }
      } else {
        mapScalarValue(inst, vecMem);
      }
    } else {
      mapVectorValue(inst, vecMem);
    }
  }

  return;
}


Value *
NatBuilder::createVaryingToUniformStore(Instruction *inst, Type *accessedType, llvm::Align alignment, Value *addr, Value *mask, Value *values) {
  Value * indexVal = nullptr;
  auto & ctx = builder.getContext();

  BasicBlock * continueBlock = nullptr;
  if (!mask) {
    // under uniform contexts store the value of the last lane
    indexVal = ConstantInt::get(Type::getInt32Ty(ctx), vectorWidth() - 1);
  } else {
  // guard against empty mask
    // create a mask ptest

    auto * nativeIntTy = Type::getInt32Ty(ctx);

  // use the potentially slower but generic max lane code
#define NAT_GENERIC_MAXLANE

#ifndef NAT_GENERIC_MAXLANE
  // create guard cde
  // extract the mask as an integer
    //
    // FIXME (llvm): the code below really should work but it does not:
    // auto * bitIntTy = IntegerType::getIntNTy(ctx, vectorWidth());
    // auto * bitIntVecTy = VectorType::get(bitIntTy, 1);

    // auto * maskIntVec = builder.CreateBitCast(mask, bitIntVecTy); // FIXME this extracts the first byte of the vector register that holds %mask (inspect the assembly)
    // auto * maskInt = builder.CreateExtractElement(maskIntVec, ConstantInt::get(nativeIntTy, 0));
    // auto * regMaskInt = builder.CreateZExt(maskInt, nativeIntTy);

    Module *mod = vecInfo.getMapping().vectorFn->getParent();

    // AVX specific code
    Intrinsic::ID movMaskID;
    Type * laneTy = nullptr;
    switch(vectorWidth()) {
      case 8: { movMaskID = Intrinsic::x86_avx_movmsk_ps_256; laneTy = nativeIntTy; break; }
      case 4: { movMaskID =  Intrinsic::x86_sse_movmsk_ps; laneTy = nativeIntTy; break; }
    }
    auto * movMaskFunc = Intrinsic::getDeclaration(mod, movMaskID);
    auto * movMaskTy = movMaskFunc->getFunctionType()->getParamType(0);

    // cast to argument type
    auto * vecLaneTy = VectorType::get(laneTy, vectorWidth());
    auto * sxMask = builder.CreateSExt(mask, vecLaneTy);
    auto * castMask = builder.CreateBitCast(sxMask, movMaskTy);
    auto * regMaskInt = builder.CreateCall(movMaskFunc, castMask);
#endif

    // create two new basic blocks
    BasicBlock *memBlock = BasicBlock::Create(vecInfo.getVectorFunction().getContext(), "mem_block",
                                              &vecInfo.getVectorFunction());
    continueBlock = BasicBlock::Create(vecInfo.getVectorFunction().getContext(), "cont_block",
                                                   &vecInfo.getVectorFunction());


  // branch to mem_block if any lane stores
#ifndef NAT_GENERIC_MAXLANE
    Value * branchMask = builder.CreateICmpNE(regMaskInt, ConstantInt::getNullValue(nativeIntTy)); // createPTest(mask, false); //
#else
    // rather use a ptest to allow folding with outer stores
    Value * branchMask = createPTest(mask, false);
#endif

    builder.CreateCondBr(branchMask, memBlock, continueBlock);

  // insert store in guarded block
    builder.SetInsertPoint(memBlock);
    auto * origBlock = inst->getParent();
    mapVectorValue(origBlock, continueBlock);

#ifdef NAT_GENERIC_MAXLANE
    // generic but slow implementation
    // SExt to full width int
    auto * vecLaneTy = FixedVectorType::get(nativeIntTy, vectorWidth());
    auto * sxMask = builder.CreateSExt(mask, vecLaneTy);

    // AND with lane index vector
    auto * laneIdxConst = createContiguousVector(vectorWidth(), nativeIntTy, 0, 1);
    auto * activeLaneVec = builder.CreateAnd(sxMask, laneIdxConst);

    // horizontal MAX reduction
    indexVal = &CreateVectorReduce(config, builder, RedKind::UMax, *activeLaneVec, nullptr);
#else
  // compute MSB from leading zeros
    // determine the MS (using the ctlz intrinsic)
    // llvm.ctlz.i32 (i32  <src>, i1 <is_zero_undef == false>)
    auto * ctlzFunc = Intrinsic::getDeclaration(mod, Intrinsic::ctlz, nativeIntTy);
    auto * leadingZerosVal = builder.CreateCall(ctlzFunc, {regMaskInt, ConstantInt::getTrue(ctx) });

    auto * constFullVal = ConstantInt::getSigned(nativeIntTy, 31);
    indexVal = builder.CreateSub(constFullVal, leadingZerosVal);
#endif

  }

  // extract and materialize store
  auto * lastLaneVal = builder.CreateExtractElement(values, indexVal, "xt.lastlane");
  auto * vecMem = builder.CreateStore(lastLaneVal, addr);
  cast<StoreInst>(vecMem)->setAlignment(llvm::Align(alignment));

  // proceed in continue block (if any)
  if (continueBlock) {
    builder.CreateBr(continueBlock);
    builder.SetInsertPoint(continueBlock);
  }

  return vecMem;
}

Value *NatBuilder::createUniformMaskedMemory(Instruction *inst, Type *accessedType, llvm::Align alignment,
                                             Value *addr, Value * scalarMask, Value * vectorMask, Value *values) {
  values ? ++numUniMaskedStores : ++numUniMaskedLoads;

  // emit a scalar memory acccess within a any-guarded section
  bool needsGuard = true;
  return &createAnyGuard(needsGuard, *inst->getParent(), *inst, isa<LoadInst>(inst),
      [=](IRBuilder<> & builder)
  {
    Instruction* vecMem;
    if (values) {
      vecMem = builder.CreateStore(values, addr);
      cast<StoreInst>(vecMem)->setAlignment(llvm::Align(alignment));
    } else {
      vecMem = builder.CreateLoad(accessedType, addr, "scal_mask_mem");
      cast<LoadInst>(vecMem)->setAlignment(llvm::Align(alignment));
    }
    return vecMem;
  });
}

Value *NatBuilder::createVaryingMemory(Type *vecType, llvm::Align alignment, Value *addr, Value *mask,
                                       Value *values) {
  bool scatter(values != nullptr);
  bool maskNonConst(!isa<ConstantVector>(mask));
  maskNonConst ? (scatter ? ++numMaskedScatter : ++numMaskedGather) : (scatter ? ++numScatter : ++numGather);

  if (config.useScatterGatherIntrinsics) {
    auto * vecPtrTy = addr->getType();

    std::vector<Value *> args;
    if (scatter) args.push_back(values);
    args.push_back(addr);
    args.push_back(ConstantInt::get(i32Ty, alignment.value()));
    args.push_back(mask);
    if (!scatter) args.push_back(UndefValue::get(vecType));
    Module *mod = vecInfo.getMapping().vectorFn->getParent();
    Function *intr = scatter ? Intrinsic::getDeclaration(mod, Intrinsic::masked_scatter, {vecType, vecPtrTy})
                             : Intrinsic::getDeclaration(mod, Intrinsic::masked_gather, {vecType, vecPtrTy});
    assert(intr && "scatter/gather not found!");
    return builder.CreateCall(intr, args);

  } else
    return scatter ? requestCascadeStore(values, addr, alignment.value(), mask) : requestCascadeLoad(vecType, addr, alignment.value(), mask);
}

void NatBuilder::createInterleavedMemory(Type *targetType, Type *vecType, llvm::Align alignment, std::vector<Value *> *addr, std::vector<Value *> *masks,
                                         std::vector<Value *> *values, std::vector<Value *> *srcs) {

  unsigned stride = (unsigned) addr->size();
  bool needsMask = masks->size() > 0;
  bool load = values == nullptr;

  // tranpose mask and values if needed
  ShuffleBuilder maskTransposer(vectorWidth());
  if (needsMask)
    maskTransposer.add(*masks);

  // build interleaved loads/stores
  Value *vecMem;
  ShuffleBuilder transposer(vectorWidth());
  if (!load)
    transposer.add(*values);

  for (unsigned i = 0; i < stride; ++i) {
    Value *ptr = (*addr)[i];
    Value *mask = needsMask ? maskTransposer.shuffleToInterleaved(builder, stride, i) : nullptr;

    if (masks->size() == 1 && i == (stride - 1)) {
      needsMask = true;
      mask = masks->front();
    }

    if (load) {
      vecMem = createContiguousLoad(targetType, ptr, alignment, mask, UndefValue::get(vecType));
      transposer.add(vecMem);
    } else {
      Value *val = transposer.shuffleToInterleaved(builder, stride, i);
      vecMem = createContiguousStore(val, ptr, alignment, mask);
      if (i < srcs->size())
        mapVectorValue((*srcs)[i], vecMem);
    }
  }
  if (load) {
    // de-interleave and map
    for (unsigned i = 0; i < stride; ++i) {
      vecMem = transposer.shuffleFromInterleaved(builder, stride, i);
      if (i < srcs->size())
        mapVectorValue((*srcs)[i], vecMem);
    }
  }
}

Value *NatBuilder::createContiguousStore(Value *val, Value *ptr, llvm::Align alignment, Value *mask) {
  if (mask) {
    return builder.CreateMaskedStore(val, ptr, alignment, mask);

  } else {
    StoreInst *store = builder.CreateStore(val, ptr);
    store->setAlignment(llvm::Align(alignment));
    return store;
  }
}

Value *NatBuilder::createContiguousLoad(Type *targetType, Value *ptr, llvm::Align alignment, Value *mask, Value *passThru) {
  if (mask) {
    return builder.CreateMaskedLoad(targetType, ptr, alignment, mask, passThru, "cont_load_masked");
  } else {
    LoadInst *load = builder.CreateLoad(targetType, ptr, "cont_load");
    load->setAlignment(llvm::Align(alignment));
    return load;
  }
}

void NatBuilder::addLazyInstruction(Instruction *const instr) {
  lazyInstructions.push_back(instr);
  ++numLazy;
}

llvm::Type*
GetPointerElementType(Type * ptrTy) {
  auto* innerTy = ptrTy->getPointerElementType();
  auto * innerArrTy = dyn_cast<ArrayType>(innerTy);
  if (innerArrTy && innerArrTy->getNumElements() == 0) {
    return innerArrTy->getElementType();
  }
  return innerTy;
}

void NatBuilder::requestLazyInstructions(Instruction *const upToInstruction) {
  assert(!lazyInstructions.empty() && "no lazy instructions to generate!");

  IF_DEBUG_NAT errs() << " --- reqLazy: " << upToInstruction->getName() << " --\n";

  Instruction *lazyInstr = lazyInstructions.front();
  lazyInstructions.pop_front();

  while (lazyInstr != upToInstruction) {
    // skip if already generated (only happens for interleaving)
    if (getVectorValue(*lazyInstr)) {
      lazyInstr = lazyInstructions.front();
      lazyInstructions.pop_front();
      continue;
    }

    assert(!getVectorValue(*lazyInstr) && !getScalarValue(*lazyInstr) && "instruction already generated!");

    if (isa<CallInst>(lazyInstr)) {
      if (shouldVectorize(lazyInstr))
        vectorizeCallInstruction(cast<CallInst>(lazyInstr));
      else
        copyCallInstruction(cast<CallInst>(lazyInstr));
    } else if (canVectorize(lazyInstr))
        vectorizeMemoryInstruction(lazyInstr);
    else if (shouldVectorize(lazyInstr))
      replicateInstruction(lazyInstr);
    else
      copyInstruction(lazyInstr);

    // interleaved memory generation might cause the queue to empty already at this point
    if (lazyInstructions.empty())
      return;

    lazyInstr = lazyInstructions.front();
    lazyInstructions.pop_front();
  }

  IF_DEBUG_NAT errs() << " --- DONE reqLazy: " << upToInstruction->getName() << " --\n";

  // if we reach this point this should be guaranteed:
  assert(lazyInstr == upToInstruction && "something went wrong during lazy generation!");

  // skip if already generated (only happens for interleaving, therefore only getVectorValue needed)
  if (getVectorValue(*lazyInstr))
    return;

  assert(!getVectorValue(*lazyInstr) && !getScalarValue(*lazyInstr) && "instruction already generated!");

  if (isa<CallInst>(lazyInstr)) {
    if (shouldVectorize(lazyInstr))
      vectorizeCallInstruction(cast<CallInst>(lazyInstr));
    else
      copyCallInstruction(cast<CallInst>(lazyInstr));
  } else
    vectorizeMemoryInstruction(lazyInstr);
}

static
void
SetInsertBeforeTerm(IRBuilder<> & builder, BasicBlock & block) {
  if (block.getTerminator())
    builder.SetInsertPoint(block.getTerminator());
  else
    builder.SetInsertPoint(&block);
}

llvm::Value*
NatBuilder::requestVectorValue(Value *const value) {
  if (isa<GetElementPtrInst>(value))
    return requestVectorGEP(cast<GetElementPtrInst>(value));

  if (isa<BitCastInst>(value)) {
    return requestVectorBitCast(cast<BitCastInst>(value));
  }

  if (isa<Instruction>(value)) {
    Instruction *lazyMemInstr = cast<Instruction>(value);
    if (std::find(lazyInstructions.begin(), lazyInstructions.end(), lazyMemInstr) != lazyInstructions.end())
      requestLazyInstructions(lazyMemInstr);
  }

  // check if already mapped
  Value *vecValue = getVectorValue(*value);
  if (vecValue) return vecValue;

  auto oldIP = builder.GetInsertPoint();
  auto oldIB = builder.GetInsertBlock();

  auto shape = getVectorShape(*value);
  if (shape.isVarying()) { // !vecValue
    auto * vecTy = FixedVectorType::get(value->getType(), vectorWidth());
    Value * accu = UndefValue::get(vecTy);
    auto * intTy = Type::getInt32Ty(builder.getContext());

    for (int i = 0; i < vectorWidth(); ++i) {
      auto * laneVal = getScalarValue(*value, i);
      auto * laneInst = dyn_cast<Instruction>(laneVal);
      if (laneInst) SetInsertBeforeTerm(builder, *laneInst->getParent());
      accu = builder.CreateInsertElement(accu, laneVal, ConstantInt::get(intTy, i, false), "_revec");
    }
    vecValue = accu;

  } else {
    vecValue = getScalarValue(*value);
    Instruction *vecInst = dyn_cast<Instruction>(vecValue);

    if (vecInst) {
      SetInsertBeforeTerm(builder, *vecInst->getParent());

    } else {
      // insert in header
      auto * oldInsertBlock = builder.GetInsertBlock();
      auto * insertFunc = oldInsertBlock->getParent();
      BasicBlock & entryBlock = insertFunc->getEntryBlock();
      if (oldInsertBlock != &entryBlock) {
        builder.SetInsertPoint(entryBlock.getTerminator());
      }
    }

    vecValue = &widenScalar(*vecValue, shape);
  }

  // recover insertpoint
  builder.SetInsertPoint(oldIB, oldIP);

  mapVectorValue(value, vecValue);
  return vecValue;
}

Value&
NatBuilder::widenScalar(Value & scaValue, VectorShape vecShape) {
  if (isa<Constant>(scaValue)) {
    return *getConstantVector(vectorWidth(), &cast<Constant>(scaValue));
  }

  // create a vector GEP to widen pointers
  Value * vecValue = nullptr;
  if (scaValue.getType()->isPointerTy()) {
    auto * scalarPtrTy = cast<PointerType>(scaValue.getType());
    auto * intTy = builder.getInt32Ty();

    auto AddrSpace = scalarPtrTy->getAddressSpace();

    // vecValue is a single pointer and has to be broadcasted to a vector of pointers first
    vecValue = builder.CreateVectorSplat(vectorWidth(), &scaValue);
    auto * actualPtrVecTy = vecValue->getType();

    if (!vecShape.isUniform()) { // stride != 0
      auto * ptrElemTy = GetPointerElementType(scalarPtrTy);
      assert(ptrElemTy->isSized() && "byte-stride shape on unsized element type");
      int scalarBytes = static_cast<int>(layout.getTypeStoreSize(ptrElemTy));
      if (vecShape.getStride() % scalarBytes == 0) {
        // stride aligned with object size
        Value *contVec = createContiguousVector(vectorWidth(), intTy, 0, vecShape.getStride() / scalarBytes);
        vecValue = builder.CreateGEP(scalarPtrTy->getPointerElementType(), vecValue, contVec, "expand_strided_ptr");
      } else {
        // sub element stride
        auto * charPtrTy = builder.getInt8PtrTy(AddrSpace);
        auto * charPtrVec = builder.CreatePointerCast(vecValue, FixedVectorType::get(charPtrTy, vectorWidth()), "byte_ptr");
        Value *contVec = createContiguousVector(vectorWidth(), intTy, 0, vecShape.getStride());
        auto * bytePtrVec = builder.CreateGEP(builder.getInt8Ty(), charPtrVec, contVec, "expand_byte_ptr");
        vecValue = builder.CreatePointerCast(bytePtrVec, actualPtrVecTy);
      }
    }

  } else {
    vecValue = builder.CreateVectorSplat(vectorWidth(), &scaValue);

    if (!vecShape.isUniform()) {
      assert(scaValue.getType()->isIntegerTy() || scaValue.getType()->isFloatingPointTy());

      auto *laneTy = scaValue.getType();
      Value *contVec = createContiguousVector(vectorWidth(), laneTy, 0, vecShape.getStride());
      vecValue = laneTy->isFloatingPointTy() ? builder.CreateFAdd(vecValue, contVec, "contiguous_add")
                                             : builder.CreateAdd(vecValue, contVec, "contiguous_add");
    }
  }
  assert(vecValue);

  return *vecValue;
}

void
NatBuilder::SetInsertPointAfterMappedInst (IRBuilder<> & builder, Instruction * mappedInst) {
  Instruction *nextNode = mappedInst->getNextNode();
  while (nextNode && isa<PHINode>(nextNode))
    nextNode = nextNode->getNextNode();
  if (nextNode)
    builder.SetInsertPoint(nextNode);
  else if (mappedInst->getParent()->getTerminator())
    builder.SetInsertPoint(mappedInst->getParent()->getTerminator());
  else
    builder.SetInsertPoint(mappedInst->getParent());
}


Value *NatBuilder::requestScalarValue(Value *const value, unsigned laneIdx, bool skipMapping) {
  if (isa<GetElementPtrInst>(value))
    return requestScalarGEP(cast<GetElementPtrInst>(value), laneIdx, false);

  if (isa<BitCastInst>(value))
    return requestScalarBitCast(cast<BitCastInst>(value), laneIdx, false);

  if (isa<Instruction>(value)) {
    Instruction *lazyMemInstr = cast<Instruction>(value);
    if (std::find(lazyInstructions.begin(), lazyInstructions.end(), lazyMemInstr) != lazyInstructions.end())
      requestLazyInstructions(lazyMemInstr);
  }

  Value *mappedVal = getScalarValue(*value, laneIdx);
  if (mappedVal) return mappedVal;

  // if value is integer or floating type, contiguous and has value for lane 0, add laneIdx
  Value *reqVal = nullptr;

  if (vecInfo.hasKnownShape(*value)) {
    VectorShape shape = getVectorShape(*value);
    Type *type = value->getType();
    mappedVal = getScalarValue(*value);
    if (mappedVal && (shape.isContiguous() || shape.isStrided()) &&
        (type->isIntegerTy() || type->isFloatingPointTy())) {
      // to avoid dominance problems as below
      auto oldIP = builder.GetInsertPoint();
      auto oldIB = builder.GetInsertBlock();
      Instruction *mappedInst = dyn_cast<Instruction>(mappedVal);
      Argument *mappedArg = dyn_cast<Argument>(mappedVal);
      if (mappedInst) {
        SetInsertPointAfterMappedInst(builder, mappedInst);
      } else if (mappedArg) {
        Function *insertFunction = mappedArg->getParent();
        BasicBlock &insertBlock = insertFunction->getEntryBlock();
        Instruction *firstInst = insertBlock.getFirstNonPHIOrDbg();
        if (firstInst)
          builder.SetInsertPoint(firstInst);
        else
          builder.SetInsertPoint(&insertBlock);
      }

      Constant *laneInt = type->isFloatingPointTy() ? ConstantFP::get(type, laneIdx * shape.getStride())
                                                    : ConstantInt::get(type, laneIdx * shape.getStride());
      reqVal = type->isFloatingPointTy() ? builder.CreateFAdd(mappedVal, laneInt,
                                                              value->getName() + "lane" + std::to_string(laneIdx))
                                         : builder.CreateAdd(mappedVal, laneInt,
                                                             value->getName() + "_lane" + std::to_string(laneIdx));
      if (mappedInst || mappedArg)
        builder.SetInsertPoint(oldIB, oldIP);
    }
  }

  // if value has a vector mapping -> extract from vector. if not -> clone scalar op
  if (!reqVal) {
    // to avoid dominance problems assume: if we only have a vectorized value and need a scalar one -> do not map
    skipMapping = true;
    mappedVal = getVectorValue(*value);
    Instruction *mappedInst = dyn_cast<Instruction>(mappedVal);
    Argument *mappedArg = dyn_cast<Argument>(mappedVal);
    auto oldIP = builder.GetInsertPoint();
    auto oldIB = builder.GetInsertBlock();
    if (mappedInst) {
      SetInsertPointAfterMappedInst(builder, mappedInst);
    } else if (mappedArg) {
      Function *insertFunction = mappedArg->getParent();
      BasicBlock &insertBlock = insertFunction->getEntryBlock();
      Instruction *firstInst = insertBlock.getFirstNonPHIOrDbg();
      if (firstInst)
        builder.SetInsertPoint(firstInst);
      else
        builder.SetInsertPoint(&insertBlock);
    }
    IF_DEBUG {
      errs() << "Extracting a scalar value from a vector:\n";
      errs() << "Original Value: ";
      Dump(*value);
      errs() << "Vector Value: ";
      Dump(*mappedVal);
    };

    // if the mappedVal is a alloca instruction, create a GEP instruction
    if (isa<AllocaInst>(mappedVal)) {
      auto indexTy = getIndexTy(mappedVal);
      reqVal = builder.CreateGEP(cast<AllocaInst>(mappedVal)->getAllocatedType(), mappedVal, ConstantInt::get(indexTy, laneIdx));
    } else {
      // extract from GEPs are not allowed. in that case recreate the scalar instruction and get that new value
      if (isa<GetElementPtrInst>(mappedVal) && isa<GetElementPtrInst>(value)) {
        auto indexTy = getIndexTy(mappedVal);
        reqVal = builder.CreateGEP(cast<GetElementPtrInst>(mappedVal)->getSourceElementType(), mappedVal, ConstantInt::get(indexTy, laneIdx));
      } else {
        reqVal = builder.CreateExtractElement(mappedVal, ConstantInt::get(i32Ty, laneIdx), "extract");
      }
    }


    if (reqVal->getType() != value->getType()) {
      reqVal = builder.CreateBitCast(reqVal, value->getType(), "bc");
    }

    if (mappedInst || mappedArg)
      builder.SetInsertPoint(oldIB, oldIP);
  }

  // only map if normal request. fresh requests will not get mapped
  if (!skipMapping) mapScalarValue(value, reqVal, laneIdx);
  return reqVal;
}

llvm::Value *
NatBuilder::buildGEP(GetElementPtrInst *const gep, bool buildScalar, unsigned laneIdx) {
  BasicBlockVector mappedBlocks = getMappedBlocks(gep->getParent());
  BasicBlock *insertBlock = builder.GetInsertBlock();
  auto insertPoint = builder.GetInsertPoint();
  setInsertionToDomBlockEnd(builder, mappedBlocks);

  assert(gep->getNumOperands() - 1 == gep->getNumIndices() && "llvm implementation for GEP changed!");

  // first, we need a vectorized base vecValue (or scalar if all_uniform). then, we have to calculate the indices
  // we need vector values if something is not all_uniform. we need to extract an dimension if !buildAllDimensions
  Value *basePtr = gep->getPointerOperand();
  Type *basePtrElementType = gep->getSourceElementType();
  VectorShape basePtrShape = getVectorShape(*basePtr);

  Value *vecBasePtr;
  Type *vecBasePtrElementType;
  if (buildScalar || basePtrShape.isUniform()) {
    vecBasePtr = requestScalarValue(basePtr, laneIdx);
    vecBasePtrElementType = basePtrElementType;
  } else {
    vecBasePtr = requestVectorValue(basePtr);
    vecBasePtrElementType = basePtrElementType;
  }

  std::vector<Value *> idxList;
  idxList.reserve(gep->getNumIndices());
  for (unsigned i = 0; i < gep->getNumIndices(); ++i) {
    Value *idx = gep->getOperand(i + 1);
    VectorShape idxShape = getVectorShape(*idx);

    Value *vecIdx;
    if (buildScalar || (idxShape.isUniform() && !basePtrShape.isUniform()))
      vecIdx = requestScalarValue(idx, laneIdx);
    else {
      vecIdx = requestVectorValue(idx);
    }

    idxList.push_back(vecIdx);
  }

  Value * vecGEP = builder.CreateGEP(vecBasePtrElementType, vecBasePtr, idxList, gep->getName());
  auto * vecGEPInst = dyn_cast<GetElementPtrInst>(vecGEP);
  if (vecGEPInst) {
    vecGEPInst->setIsInBounds(gep->isInBounds());
  }

  builder.SetInsertPoint(insertBlock, insertPoint);

  return vecGEP;
}

llvm::Value*
NatBuilder::requestVectorGEP(GetElementPtrInst *const gep) {
  Value *mapped = getVectorValue(*gep);
  if (mapped) return mapped;

  ++numVecGEPs;

  mapped = buildGEP(gep, false, 0);
  mapVectorValue(gep, mapped);
  return mapped;
}

llvm::Value*
NatBuilder::requestScalarGEP(llvm::GetElementPtrInst *const gep, unsigned laneIdx, bool skipMapping) {
  Value *mapped = getScalarValue(*gep, laneIdx);
  if (mapped) return mapped;

  ++numScalGEPs;

  mapped = buildGEP(gep, true, laneIdx);
  if (!skipMapping)
    mapScalarValue(gep, mapped, laneIdx);
  return mapped;
}

llvm::Value*
NatBuilder::requestVectorBitCast(BitCastInst *const bc) {
  Value *mapped = getVectorValue(*bc);
  if (mapped)
    return mapped;

  ++numVecBCs;

  BasicBlockVector mappedBlocks = getMappedBlocks(bc->getParent());
  BasicBlock *insertBlock = builder.GetInsertBlock();
  auto insertPoint = builder.GetInsertPoint();
  setInsertionToDomBlockEnd(builder, mappedBlocks);

  assert(bc->getNumOperands() == 1 && "code for bitcasts changed!");
  Value *op = bc->getOperand(0);
  Value *vecOp = requestVectorValue(op);
  Type *vecType = getVectorType(bc->getType(), vectorWidth());

  mapped = builder.CreateBitCast(vecOp, vecType, bc->getName());
  mapVectorValue(bc, mapped);

  builder.SetInsertPoint(insertBlock, insertPoint);

  return mapped;
}

Value *NatBuilder::requestScalarBitCast(llvm::BitCastInst *const bc, unsigned laneIdx, bool skipMapping) {
  Value *mapped = getScalarValue(*bc, laneIdx);
  if (mapped)
    return mapped;

  ++numScalBCs;

  BasicBlockVector mappedBlocks = getMappedBlocks(bc->getParent());
  BasicBlock *insertBlock = builder.GetInsertBlock();
  auto insertPoint = builder.GetInsertPoint();
  setInsertionToDomBlockEnd(builder, mappedBlocks);

  assert(bc->getNumOperands() == 1 && "code for bitcasts changed!");
  Value *op = bc->getOperand(0);
  Value *scalOp = requestScalarValue(op, laneIdx);
  mapped = builder.CreateBitCast(scalOp, bc->getType(), bc->getName());
  if (!skipMapping)
    mapScalarValue(bc, mapped, laneIdx);

  builder.SetInsertPoint(insertBlock, insertPoint);

  return mapped;
}

llvm::Value*
NatBuilder::requestInterleavedGEP(GetElementPtrInst *const gep, unsigned interleavedIdx) {
  assert(gep->getNumOperands() - 1 == gep->getNumIndices() && "llvm implementation for GEP changed!");

  // first, we need a vectorized base vecValue (or scalar if all_uniform). then, we have to calculate the indices
  // we need vector values if something is not all_uniform. we need to extract an dimension if !buildAllDimensions
  Value *scalBasePtr = gep->getPointerOperand();
  Value *basePtr = requestScalarValue(scalBasePtr);
  Type *st = isStructAccess(gep);

  std::vector<Value *> idxList;
  idxList.reserve(gep->getNumIndices());
  for (unsigned i = 0; i < gep->getNumIndices(); ++i) {
    Value *idx = gep->getOperand(i + 1);
    Value *interIdx = requestScalarValue(idx);
    VectorShape idxShape = getVectorShape(*idx);

    if (st && !idxShape.isUniform() && interleavedIdx > 0) {
      // calculate total offset from base and size of struct (1 if no struct)
      unsigned offset = vectorWidth() * interleavedIdx + getStructOffset(gep);
      unsigned structSize = getNumLeafElements(st, gep->getResultElementType(), layout);
      unsigned k = offset / structSize;
      if (k > 0)
        interIdx = builder.CreateAdd(interIdx, ConstantInt::get(interIdx->getType(), k), "inter_struct_idx");

      idxList.push_back(interIdx);

      // create the actual struct access indices
      for (++i; i < gep->getNumIndices(); ++i) {
        idx = gep->getOperand(i + 1);
        assert(isa<ConstantInt>(idx) && "element access with non-constant!");

        unsigned idxValue = (unsigned) cast<ConstantInt>(idx)->getLimitedValue();
        offset %= structSize;
        st = st->getContainedType(idxValue);
        structSize = getNumLeafElements(st, gep->getResultElementType(), layout);
        k = offset / structSize;

        idxList.push_back(ConstantInt::get(idx->getType(), k));
      }
      break;

    } else if (interleavedIdx > 0 && !idxShape.isUniform()) {
      interIdx = builder.CreateAdd(interIdx, ConstantInt::get(interIdx->getType(), vectorWidth() * interleavedIdx), "inter_idx");
    }

    idxList.push_back(interIdx);
  }

  auto *interGEP = builder.CreateGEP(gep->getSourceElementType(), basePtr, idxList, "inter_gep");
  auto * interGEPInst = dyn_cast<GetElementPtrInst>(interGEP);
  if (interGEPInst) interGEPInst->setIsInBounds(gep->isInBounds());

  return interGEP;
}

llvm::Value *
NatBuilder::requestInterleavedAddress(llvm::Value *const addr, unsigned interleavedIdx, Type *const vecType) {
  ++numInterGEPs;
  Value *interAddr = addr;

  if (isa<BitCastInst>(interAddr))
    interAddr = cast<BitCastInst>(interAddr)->getOperand(0);

  if (isa<GetElementPtrInst>(interAddr))
    interAddr = requestInterleavedGEP(cast<GetElementPtrInst>(interAddr), interleavedIdx);

  else {
    Value *ptr = requestScalarValue(addr, 0, true);
    auto * indexTy = getIndexTy(ptr);
    interAddr = builder.CreateGEP(vecType->getScalarType(), ptr, ConstantInt::get(indexTy, vectorWidth() * interleavedIdx), "inter_gep");
  }

  int AddrSpace = cast<PointerType>(addr->getType())->getAddressSpace();
  PointerType *vecPtrType = vecType->getPointerTo(AddrSpace);
  return builder.CreatePointerCast(interAddr, vecPtrType, "inter_cast");
}

llvm::Value *
NatBuilder::requestCascadeLoad(Type *accessedType, Value *vecPtr, unsigned alignment, Value *mask) {
  unsigned bitWidth = accessedType->getScalarSizeInBits();

  Function *func = getCascadeFunction(bitWidth, false);
  if (!func) {
    func = createCascadeMemory(accessedType, cast<VectorType>(vecPtr->getType()), alignment, cast<VectorType>(mask->getType()),
                               false);
    mapCascadeFunction(bitWidth, func, false);
  }

  // cast call argument to correct type if needed
  Argument *ptrArg = &*func->arg_begin();
  Value *callPtr = vecPtr;

  if (ptrArg->getType() != vecPtr->getType()) {
    callPtr = builder.CreateBitCast(callPtr, ptrArg->getType(), "bc");
  }

  std::vector<Value *> args;
  args.push_back(callPtr);
  args.push_back(mask);
  Value *ret = builder.CreateCall(func, args, "cascade_load");
  // cast call result to correct type if needed
  Type *vecType = getVectorType(accessedType, vectorWidth());
  if (ret->getType() != vecType) {
    ret = builder.CreateBitCast(ret, vecType, "bc");
  }
  return ret;
}

Value *NatBuilder::requestCascadeStore(Value *vecVal, Value *vecPtr, unsigned alignment, Value *mask) {
  unsigned bitWidth = vecVal->getType()->getScalarSizeInBits();

  Function *func = getCascadeFunction(bitWidth, true);
  if (!func) {
    func = createCascadeMemory(vecVal->getType(), cast<VectorType>(vecPtr->getType()), alignment, cast<VectorType>(mask->getType()),
                               true);
    mapCascadeFunction(bitWidth, func, true);
  }

  // cast call arguments to correct type if needed
  auto argIt = func->arg_begin();
  Argument *valArg = &*argIt++;
  Argument *ptrArg = &*argIt;
  Value *callVal = vecVal;
  Value *callPtr = vecPtr;

  if (valArg->getType() != callVal->getType()) {
    callVal = builder.CreateBitCast(callVal, valArg->getType());
  }
  if (ptrArg->getType() != callPtr->getType()) {
    callPtr = builder.CreateBitCast(callPtr, ptrArg->getType());
  }

  std::vector<Value *> args;
  args.push_back(callVal);
  args.push_back(callPtr);
  args.push_back(mask);
  return builder.CreateCall(func, args);
}

Function *NatBuilder::createCascadeMemory(Type *accessedType, VectorType *pointerVectorType, unsigned alignment, VectorType *maskType,
                                          bool store) {
  assert(cast<VectorType>(pointerVectorType)->getElementType()->isPointerTy()
         && "pointerVectorType must be of type vector of pointer!");
  assert(cast<VectorType>(maskType)->getElementType()->isIntegerTy(1)
         && "maskType must be of type vector of i1!");


  Module *mod = vecInfo.getScalarFunction().getParent();
  IRBuilder<> builder(mod->getContext());

  // create function
  Type *resType = store ? Type::getVoidTy(mod->getContext()) : getVectorType(accessedType, vectorWidth());
  std::vector<Type *> argTypes;
  if (store) {
    Type *valType = getVectorType(accessedType, vectorWidth());
    argTypes.push_back(valType);
  }
  argTypes.push_back(pointerVectorType);
  argTypes.push_back(maskType);

  std::string name = store ? "nativeCascadeStoreFn" : "nativeCascadeLoadFn";
  FunctionType *fnType = FunctionType::get(resType, argTypes, false);
  Function *func = Function::Create(fnType, GlobalValue::LinkageTypes::ExternalLinkage, name, mod);

  auto argIt = func->arg_begin();
  Argument *valVec = nullptr;
  if (store) {
    valVec = &*argIt++;
    valVec->setName("valVec");
  }
  Argument *ptrVec = &*argIt++;
  Argument *mask = &*argIt;

  ptrVec->setName("ptrVec");
  mask->setName("mask");

  // create body
  // following function:
  // vector a, mask m, vector r = undef
  // if(m) r = *a; (vector load)
  // else
  // if(m.x) r.x = *(a.x);
  // if(m.y) r.y = *(a.y);
  // if(m.z) r.z = *(a.z);
  // if(m.w) r.w = *(a.w);
  // return r;
  // example assumes vector width 4 and load. store basically the same

  // create blocks. we need <vectorWidth> load blocks, <vectorWidth> condition blocks and one return block
  std::vector<BasicBlock *> condBlocks;
  std::vector<BasicBlock *> loadBlocks;
  BasicBlock *ret = createCascadeBlocks(func, vectorWidth(), condBlocks, loadBlocks);

  // insert result vector in 1st block
  BasicBlock *entry = condBlocks[0];
  builder.SetInsertPoint(entry);
  Value *resVec = store ? nullptr : UndefValue::get(resType);

  // fill cond and load blocks
  for (int i = 0; i < vectorWidth(); ++i) {
    BasicBlock *cond = condBlocks[i];
    BasicBlock *masked = loadBlocks[i];
    BasicBlock *nextBlock = i == ((int) vectorWidth()) - 1 ? ret : condBlocks[i + 1];

    // code for cond block: extract mask lane i, branch to masked or next
    Value *maskLaneVal = builder.CreateExtractElement(mask, ConstantInt::get(i32Ty, i),
                                                      "mask_lane_" + std::to_string(i));
    builder.CreateCondBr(maskLaneVal, masked, nextBlock);

    // code for masked block: extract pointer lane i, ...
    builder.SetInsertPoint(masked);
    Value *pointerLaneVal = builder.CreateExtractElement(ptrVec, ConstantInt::get(i32Ty, i),
                                                         "ptr_lane_" + std::to_string(i));
    Value *insert = nullptr;

    if (store) {
      // ... extract value lane i, store val to ptr, branch to next
      Value *storeLaneVal = builder.CreateExtractElement(valVec, ConstantInt::get(i32Ty, i),
                                                         "val_lane_" + std::to_string(i));
      builder.CreateStore(storeLaneVal, pointerLaneVal);
    } else {
      // ... load from pointer, insert to result vector, branch to next
      Value *loadInst = builder.CreateLoad(accessedType, pointerLaneVal, "load_lane_" + std::to_string(i));
      cast<LoadInst>(loadInst)->setAlignment(llvm::Align(alignment));
      insert = builder.CreateInsertElement(resVec, loadInst, ConstantInt::get(i32Ty, i),
                                           "insert_lane_" + std::to_string(i));
    }
    builder.CreateBr(nextBlock);
    builder.SetInsertPoint(nextBlock);

    // ONLY IF LOAD: code for next block: phi <resVec, cond> <insert, masked>
    if (!store) {
      PHINode *phi = builder.CreatePHI(insert->getType(), 2);
      phi->addIncoming(resVec, cond);
      phi->addIncoming(insert, masked);
      resVec = phi;
    }
  }

  // fill result block
  // code for result block: return resVec or return void
  if (store) builder.CreateRetVoid();
  else builder.CreateRet(resVec);

  return func;
}

void NatBuilder::mapCascadeFunction(unsigned bitWidth, Function *function, bool store) {
  if (store) cascadeStoreMap[bitWidth] = function;
  else cascadeLoadMap[bitWidth] = function;
}

Function *NatBuilder::getCascadeFunction(unsigned bitWidth, bool store) {
  auto mapIt = store ? cascadeStoreMap.find(bitWidth) : cascadeLoadMap.find(bitWidth);
  auto mapEt = store ? cascadeStoreMap.end() : cascadeLoadMap.end();
  if (mapIt != mapEt) return mapIt->second;
  return nullptr;
}

Value *NatBuilder::createPTest(Value *vector, bool isRv_all) {
  assert(vector->getType()->isVectorTy() && "given value is no vector type!");
  assert(cast<VectorType>(vector->getType())->getElementType()->isIntegerTy(1) &&
         "vector elements must have i1 type!");

// new generic code path
  if (!config.enableIRPolish) {
    RedKind kind = isRv_all ? RedKind::And : RedKind::Or;
    return &CreateVectorReduce(config, builder, kind, *vector, nullptr);
  }

// legacy IR-polisher based code path
  if (isRv_all) {
    vector = builder.CreateNot(vector, "rvall_cond_not");
  }

  Value * ptest = nullptr;
  auto *redFunc = platInfo.requestVectorMaskReductionFunc(
      "rv_reduce_or",
      cast<FixedVectorType>(vector->getType())->getNumElements());
  ptest = builder.CreateCall(redFunc, vector, "ptest");

  if (isRv_all) {
    ptest = builder.CreateNot(ptest, "rvall_not");
  }

  return ptest;
}

bool
NatBuilder::hasUniformPredicate(const BasicBlock & BB) const {
  if (!vecInfo.getRegion().contains(&BB) || !vecInfo.getPredicate(BB)) return true;
  else return vecInfo.getVectorShape(*vecInfo.getPredicate(BB)).isUniform();
}

Value*
NatBuilder::requestVectorPredicate(const BasicBlock& scaBlock) {
  return requestVectorValue(vecInfo.getPredicate(scaBlock));
}

Value *NatBuilder::maskInactiveLanes(Value *const value, const BasicBlock* const block, bool invert) {
  auto pred = requestVectorPredicate(*block);
  if (invert) {
    return builder.CreateOr(value, builder.CreateNot(pred));
  } else {
    return builder.CreateAnd(value, pred);
  }
}

void
NatBuilder::repairOutsideUses(Instruction & scaChainInst, std::function<Value& (Value &,BasicBlock &)> repairFunc) {
  std::map<BasicBlock*, Value*> fixMap;

  // actually used value
  Value * vecUsed = getScalarValue(scaChainInst);
  if (!vecUsed) vecUsed = getVectorValue(scaChainInst, 0);

  assert(vecUsed && "could not infer vector version of scalar instruction");

  // iterate over all uses and invoke @repairFunc for use edges that leave the region
  for (auto itUse = scaChainInst.use_begin(); itUse != scaChainInst.use_end(); ){
    auto & use = *itUse++;

    int opIdx = use.getOperandNo();
    auto & userInst = cast<Instruction>(*use.getUser());

    // user is in the region
    if (vecInfo.inRegion(*userInst.getParent())) continue;

    auto * userPhi = dyn_cast<PHINode>(&userInst);
    bool isLcssaPhi = userPhi && userPhi->getNumIncomingValues() == 1;

    // for non-LCSSA phis ust the incoming block as def block
    BasicBlock * userBlock = userInst.getParent();
    if (userPhi && !isLcssaPhi) {
      int incomingIdx = userPhi->getIncomingValueNumForOperand(opIdx);
      userBlock = userPhi->getIncomingBlock(incomingIdx);
    }

    // look for a dominating definition somewhere
    // TODO take the incoming block for non-lcssa phis
    decltype(fixMap)::iterator itFix = fixMap.begin();
    for (; itFix != fixMap.end(); ++itFix) {
      auto *defBlock = itFix->first;
      if (dominatorTree.dominates(defBlock, userBlock)) break;
    }

    Value * reducedVal = nullptr;
    if (itFix != fixMap.end()) {
      reducedVal = itFix->second; // TODO make sure this comes before the user...
    } else {
      IF_DEBUG errs() << "Repairing use " << userInst << "\n";
      // invoke custom reduction function
      reducedVal = &repairFunc(*vecUsed, *userBlock);
      assert(&reducedVal);
      IF_DEBUG errs() << "\t reduced: " << *reducedVal << "\n";
      auto * redInst = cast<Instruction>(reducedVal);
      fixMap[redInst->getParent()] = redInst;
    }

    if (isLcssaPhi) {
      // TODO maintain LCSSA
      userInst.replaceAllUsesWith(reducedVal);
      userInst.eraseFromParent();

    } else {
      // regular use
      userInst.setOperand(opIdx, reducedVal);
    }
  }

}

void
NatBuilder::materializeStridePattern(rv::StridePattern & sp) {
  IF_DEBUG { errs() << "Fixing strided reduction "; sp.dump(); errs() << "\n"; }

  // widen stride to full vectorWidth
  int vectorWidth = vecInfo.getVectorWidth();

  auto redShape = sp.getShape(vectorWidth);
  if (redShape.isUniform()) return;

  assert(redShape.hasStridedShape());

// vectorize the reduction itself (loop internal uses)
  auto & vecPhi = *getScalarValueAs<PHINode>(*sp.phi, 0);
  auto & vecReductor = *getScalarValueAs<Instruction>(*sp.reductor, 0);

  // create an adjusted reductor (full SIMD stride)
  auto * clonedReductor = cast<Instruction>(vecReductor.clone());
  int vecStride = vectorWidth * redShape.getStride();
  auto & vecConst = *ConstantInt::getSigned(sp.phi->getType(), vecStride);

  // FIXME rematerialize reductor instead (currently unsount wrt to sub)
  int constIdx = isa<Constant>(sp.reductor->getOperand(0)) ? 0 : 1;
  clonedReductor->setOperand(constIdx, &vecConst);
  clonedReductor->insertAfter(&vecReductor);

  // remap phi operands
  int loopOpIdx = sp.latchIdx;
  int initOpIdx = sp.loopInitIdx;

  // attach reduced inputs to phi
  vecPhi.addIncoming(sp.phi->getIncomingValue(initOpIdx), sp.phi->getIncomingBlock(initOpIdx));
  auto * vecLatch = getVectorBlock(*sp.phi->getIncomingBlock(loopOpIdx), true);
  vecPhi.addIncoming(clonedReductor, vecLatch);

  repairOutsideUses(*sp.phi,
                    [&](Value& usedVal, BasicBlock& userBlock) ->Value& {
                      // otw, replace with reduced value
                      int64_t amount = (vectorWidth - 1) * redShape.getStride();
                      auto * insertPt = userBlock.getFirstNonPHI();
                      IRBuilder<> builder(&userBlock, insertPt->getIterator());

                      auto * liveOutView = builder.CreateAdd(&usedVal, ConstantInt::getSigned(usedVal.getType(), amount), ".red");
                      return *liveOutView;
                    }
  );

  repairOutsideUses(*sp.reductor,
                    [&](Value & usedVal, BasicBlock & userBlock) ->Value& {
                      // otw, replace with reduced value
                      int64_t amount = (vectorWidth - 1) * redShape.getStride();
                      auto * insertPt = userBlock.getFirstNonPHI();
                      IRBuilder<> builder(&userBlock, insertPt->getIterator());

                      auto * liveOutView = builder.CreateAdd(&usedVal, ConstantInt::getSigned(usedVal.getType(), amount), ".red");
                      return *liveOutView;
                    }
  );
}

void
NatBuilder::materializeRecurrence(Reduction & red, PHINode & scaPhi) {
  abort(); // this code is broken

  const int vectorWidth = vecInfo.getVectorWidth();
  assert(vecInfo.getVectorShape(scaPhi).isVarying());

// construct new (vectorized) initial value
  auto * vecPhi = getVectorValueAs<PHINode>(scaPhi);
  auto * vecTy = vecPhi->getType();

  auto * inAtZero = dyn_cast<Instruction>(scaPhi.getIncomingValue(0));
  int latchIdx = (inAtZero && vecInfo.inRegion(*inAtZero)) ? 0 : 1;
  int initIdx = 1 - latchIdx;

  BasicBlock * vecInitInputBlock = scaPhi.getIncomingBlock(initIdx);
  BasicBlock * vecLoopInputBlock = getVectorBlock(*scaPhi.getIncomingBlock(latchIdx), true);

// broadcast initial value to all lanes
  Value * scaInitValue = scaPhi.getIncomingValue(initIdx);
  IRBuilder<> phBuilder(vecInitInputBlock, vecInitInputBlock->getTerminator()->getIterator());
  auto * intTy = Type::getInt32Ty(scaPhi.getContext());
  auto * vecFirstLane = phBuilder.CreateInsertElement(UndefValue::get(vecTy), scaInitValue, ConstantInt::get(intTy, 0, false));
  auto * vecInitVal = CreateBroadcast(phBuilder, *vecFirstLane, 0);
  vecPhi->addIncoming(vecInitVal, vecInitInputBlock);

// add latch update (extract last lane)
  Instruction * scaLatchInst = cast<Instruction>(scaPhi.getIncomingValue(latchIdx));
  auto * vecLatchInst = getVectorValueAs<Instruction>(*scaLatchInst);
  auto itInsert = vecLatchInst->getIterator();
  ++itInsert;
  IRBuilder<> latchBuilder(vecLatchInst->getParent(), itInsert);

  // broadcast the last element
  auto * latchUpdate = CreateBroadcast(latchBuilder, *vecLatchInst, vectorWidth - 1);
  vecPhi->addIncoming(latchUpdate, vecLoopInputBlock);

// extract last lane for outside users
  repairOutsideUses(*scaLatchInst,
                    [&](Value & usedVal, BasicBlock & userBlock) ->Value& {
                      // otw, replace with reduced value
                      auto * insertPt = userBlock.getFirstNonPHI();
                      IRBuilder<> builder(&userBlock, insertPt->getIterator());
                      auto & extracted = CreateExtract(builder, *vecLatchInst, -1);
                      return extracted;
                    }
  );
}

void
NatBuilder::materializeOrderedReduction(Reduction & red, PHINode & scaPhi) {
  assert((red.kind != RedKind::Top) && (red.kind != RedKind::Bot));

  const auto vectorWidth = vecInfo.getVectorWidth();
  auto * vecPhi = getVectorValueAs<PHINode>(scaPhi);
  auto redShape = red.getShape(vectorWidth);
  assert(redShape.isVarying()); (void) redShape;


// construct new (vectorized) initial value
  // TODO generalize to multi phi reductions
  Value * vecNeutral = getSplat(&GetNeutralElement(red.kind, *scaPhi.getType()));

  auto * inAtZero = dyn_cast<Instruction>(scaPhi.getIncomingValue(0));
  int latchIdx = (inAtZero && vecInfo.inRegion(*inAtZero)) ? 0 : 1;
  int initIdx = 1 - latchIdx;

  BasicBlock * vecInitInputBlock = scaPhi.getIncomingBlock(initIdx);
  auto & vecLatchBlock = *getVectorBlock(*scaPhi.getIncomingBlock(latchIdx), true);

// materialize initial input (insert init value into last lane)
  Value * scaInitValue = scaPhi.getIncomingValue(initIdx);
  auto * scaLatchInst = cast<Instruction>(scaPhi.getIncomingValue(latchIdx));
  auto * vecLatchInst = getVectorValueAs<Instruction>(*scaLatchInst);

// create a scalar ordered Phi nodes
  auto * orderPhi = PHINode::Create(scaPhi.getType(), 2, scaPhi.getName() + ".ord", vecPhi);
  orderPhi->addIncoming(scaInitValue, vecInitInputBlock);
// (orderly) reduce vectors into scalars
  IRBuilder<> latchBuilder(&vecLatchBlock, vecLatchBlock.getTerminator()->getIterator());
  auto & reducedUpdate = CreateVectorReduce(config, latchBuilder, red.kind, *vecLatchInst, orderPhi);
  orderPhi->addIncoming(&reducedUpdate, &vecLatchBlock);

// reduce reduction phi for outside users
  repairOutsideUses(*scaLatchInst,
                    [&](Value & usedVal, BasicBlock & userBlock) ->Value& {
                      return reducedUpdate;
                    }
  );


  // construct a 1...10 mask
  std::vector<Constant*> selElems;
  for (size_t i = 1; i < vectorWidth; ++i) {
    selElems.push_back(ConstantInt::getTrue(orderPhi->getContext()));
  }
  selElems.push_back(ConstantInt::getFalse(orderPhi->getContext()));
  auto * selMask = ConstantVector::get(selElems); // 1...10

  // create reduced outside views for external users
  for (auto * elem : red.elements) {
    if (elem == scaLatchInst) continue; // already reduced that one
    if (!vecInfo.inRegion(*cast<Instruction>(elem))) continue;

    auto& vecElem = *getVectorValueAs<Instruction>(*elem);

    // reduce outside uses on demand
    repairOutsideUses(*elem,
                      [&](Value & usedVal, BasicBlock& userBlock) ->Value& {
                      auto * insertPt = userBlock.getFirstNonPHI();
                      IRBuilder<> builder(&userBlock, insertPt->getIterator());
                      // reduce all end-of-iteration values and request value of last iteration
                      auto & foldVec = *builder.CreateSelect(selMask, vecLatchInst, &vecElem, ".red");
                      auto & reducedVector = CreateVectorReduce(config, builder, red.kind, foldVec, orderPhi);
                      return reducedVector;
                    }
    );
  }

  // remap old vecPhi and erase
  mapVectorValue(&scaPhi, vecNeutral); // FIXME sound?
  vecPhi->replaceAllUsesWith(vecNeutral);
  vecPhi->eraseFromParent();
}


void
NatBuilder::materializeVaryingReduction(Reduction & red, PHINode & scaPhi) {
  assert((red.kind != RedKind::Top) && (red.kind != RedKind::Bot));

  const auto vectorWidth = vecInfo.getVectorWidth();
  auto * vecPhi = getVectorValueAs<PHINode>(scaPhi);
  auto redShape = red.getShape(vectorWidth);
  assert(redShape.isVarying()); (void) redShape;

// construct new (vectorized) initial value
  // TODO generalize to multi phi reductions
  Value * vecNeutral = getSplat(&GetNeutralElement(red.kind, *scaPhi.getType()));

  auto * inAtZero = dyn_cast<Instruction>(scaPhi.getIncomingValue(0));
  int latchIdx = (inAtZero && vecInfo.inRegion(*inAtZero)) ? 0 : 1;
  int initIdx = 1 - latchIdx;

  BasicBlock * vecInitInputBlock = scaPhi.getIncomingBlock(initIdx);
  BasicBlock * vecLoopInputBlock = getVectorBlock(*scaPhi.getIncomingBlock(latchIdx), true);

// materialize initial input (insert init value into last lane)
  Value * scaInitValue = scaPhi.getIncomingValue(initIdx);
  IRBuilder<> phBuilder(vecInitInputBlock, vecInitInputBlock->getTerminator()->getIterator());
  auto * intTy = Type::getInt32Ty(scaPhi.getContext());
  auto * vecInitVal = phBuilder.CreateInsertElement(vecNeutral, scaInitValue, ConstantInt::get(intTy, vectorWidth - 1, false));

// attach inputs (neutral elem and reduction inst)
  vecPhi->addIncoming(vecInitVal, vecInitInputBlock);

// add latch update
  Instruction * scaLatchInst = cast<Instruction>(scaPhi.getIncomingValue(latchIdx));
  auto * vecLatchInst = getVectorValueAs<Instruction>(*scaLatchInst);
  vecPhi->addIncoming(vecLatchInst, vecLoopInputBlock);

// reduce reduction phi for outside users
  repairOutsideUses(*scaLatchInst,
                    [&](Value & usedVal, BasicBlock & userBlock) ->Value& {
                      // otw, replace with reduced value
                      auto * insertPt = userBlock.getFirstNonPHI();
                      IRBuilder<> builder(&userBlock, insertPt->getIterator());
                      auto & reducedVector = CreateVectorReduce(config, builder, red.kind, *vecLatchInst, nullptr);
                      return reducedVector;
                    }
  );


  // construct a 1...10 mask
  std::vector<Constant*> selElems;
  for (size_t i = 1; i < vectorWidth; ++i) {
    selElems.push_back(ConstantInt::getTrue(vecPhi->getContext()));
  }
  selElems.push_back(ConstantInt::getFalse(vecPhi->getContext()));
  auto * selMask = ConstantVector::get(selElems); // 1...10

  // create reduced outside views for external users
  for (auto * elem : red.elements) {
    if (elem == scaLatchInst) continue; // already reduced that one
    if (!vecInfo.inRegion(*cast<Instruction>(elem))) continue;

    auto& vecElem = *getVectorValueAs<Instruction>(*elem);

    // reduce outside uses on demand
    repairOutsideUses(*elem,
                      [&](Value & usedVal, BasicBlock& userBlock) ->Value& {
                      auto * insertPt = userBlock.getFirstNonPHI();
                      IRBuilder<> builder(&userBlock, insertPt->getIterator());
                      // reduce all end-of-iteration values and request value of last iteration
                      auto & foldVec = *builder.CreateSelect(selMask, vecLatchInst, &vecElem, ".red");
                      auto & reducedVector = CreateVectorReduce(config, builder, red.kind, foldVec, nullptr);
                      return reducedVector;
                    }
    );
  }
}

void NatBuilder::addValuesToPHINodes() {
  // save current insertion point before continuing
//  auto IB = builder.GetInsertBlock();
//  auto IP = builder.GetInsertPoint();

  for (PHINode *scalPhi : phiVector) {
    assert(vecInfo.hasKnownShape(*scalPhi) && "no VectorShape for PHINode available!");
    VectorShape shape = getVectorShape(*scalPhi);
    Type *scalType = scalPhi->getType();

    // replicate phi <vector_width> times if type is not vectorizable
    bool replicate = shape.isVarying() && (scalType->isVectorTy() || scalType->isStructTy());
    unsigned loopEnd = replicate ? vectorWidth() : 1;

    auto *sp = reda.getStrideInfo(*scalPhi);
    auto *red = reda.getReductionInfo(*scalPhi);

    bool isVectorLoopHeader = &vecInfo.getEntry() == scalPhi->getParent();
    IF_DEBUG_NAT {
      errs() << "loopHead: " << isVectorLoopHeader << ": shape " << shape.str() << "red: "; if (red) red->dump(); errs() << "\n";
    }

    if (isVectorLoopHeader && shape.hasStridedShape() && sp) {
      IF_DEBUG_NAT { errs() << "-- materializing "; sp->dump(); errs() << "\n"; }
      materializeStridePattern(*sp);

    } else if (isVectorLoopHeader && shape.isVarying() && red && red->kind != RedKind::Bot) {
      // reduction phi handling
      IF_DEBUG_NAT { errs() << "-- materializing "; red->dump(); errs() << "\n"; }
      if (CheckFlag("RV_RED_ORDER")) {
        materializeOrderedReduction(*red, *scalPhi);
      } else {
        materializeVaryingReduction(*red, *scalPhi);
      }

    } else if (isVectorLoopHeader && red && red->kind == RedKind::Bot && shape.isVarying()) {
      // reduction phi handling
      IF_DEBUG_NAT { errs() << "-- materializing "; red->dump(); errs() << "\n"; }
      materializeRecurrence(*red, *scalPhi);

    } else {
      // default phi handling (includes fully uniform recurrences)
      for (unsigned lane = 0; lane < loopEnd; ++lane) {
        PHINode *phi = !shape.isVarying() || replicate ? getScalarValueAs<PHINode>(*scalPhi, lane) : getVectorValueAs<PHINode>(*scalPhi);
        for (unsigned i = 0; i < scalPhi->getNumIncomingValues(); ++i) {
          // set insertion point to before Terminator of incoming block
          BasicBlock *incVecBlock = getVectorBlock(*scalPhi->getIncomingBlock(i), true);
          builder.SetInsertPoint(incVecBlock->getTerminator());

          Value *val = !shape.isVarying() || replicate ? requestScalarValue(scalPhi->getIncomingValue(i), lane)
                                                       : requestVectorValue(scalPhi->getIncomingValue(i));
          phi->addIncoming(val, incVecBlock);
        }
      }
    }
  }

  // restore insertion point
//  builder.SetInsertPoint(IB, IP);
}

void NatBuilder::mapVectorValue(const Value *const value, Value *vecValue) {
  if (isa<BasicBlock>(value)) {
    const BasicBlock *const block = cast<const BasicBlock>(value);
    BasicBlock *vecBlock = cast<BasicBlock>(vecValue);
    BasicBlockVector &vectorBlocks = basicBlockMap[block];
    vectorBlocks.push_back(vecBlock);
  } else
    vectorValueMap[value] = vecValue;
}

Value *NatBuilder::getVectorValue(Value& ScaValue, bool getLastBlock) {
  if (isa<BasicBlock>(ScaValue)) {
    if (!vecInfo.inRegion(cast<BasicBlock>(ScaValue))) {
      return &ScaValue; // preserve BBs outside of the region
    }

    BasicBlock *const block = cast<BasicBlock>(&ScaValue);
    auto blockIt = basicBlockMap.find(block);
    if (blockIt != basicBlockMap.end()) {
      BasicBlockVector &blocks = blockIt->second;
      return getLastBlock ? blocks.back() : blocks.front();
    }
  }

  auto vecIt = vectorValueMap.find(&ScaValue);
  if (vecIt != vectorValueMap.end()) return vecIt->second;
  else return nullptr;
}

BasicBlock*
NatBuilder::getVectorBlock(BasicBlock & ScaBlock, bool ReturnLastBlock) {
  if (!vecInfo.inRegion(ScaBlock)) {
    return &ScaBlock; // preserve BBs outside of the region
  }

  auto blockIt = basicBlockMap.find(&ScaBlock);
  if (blockIt != basicBlockMap.end()) {
    BasicBlockVector &blocks = blockIt->second;
    return ReturnLastBlock ? blocks.back() : blocks.front();
  }
  return nullptr;
}

void NatBuilder::mapScalarValue(const Value *const value, Value *mapValue, unsigned laneIdx) {
  LaneValueVector &laneValues = scalarValueMap[value];
  if (laneValues.size() < laneIdx) laneValues.resize(laneIdx);
  laneValues.insert(laneValues.begin() + laneIdx, mapValue);
}

Value *NatBuilder::getScalarValue(Value & ScaValue, unsigned laneIdx) {
  if (isa<MetadataAsValue>(ScaValue)) {
    // as used in "llvm.dbg.value" calls
    return &ScaValue;
  }

  // in case of regions, keep any values that are live into the region
  // FIXME make this generic through explicit argument mapping
  if (vecInfo.getRegion().isVectorLoop() && isa<Argument>(ScaValue)) {
    return &ScaValue;
  } else if (vecInfo.getRegion().isVectorLoop() && isa<Instruction>(ScaValue) && !vecInfo.inRegion(*cast<Instruction>(ScaValue).getParent())) {
    return &ScaValue;
  }

  const Constant *constant = dyn_cast<const Constant>(&ScaValue);
  if (constant) return const_cast<Constant *>(constant);

  auto scalarIt = scalarValueMap.find(&ScaValue);
  if (scalarIt != scalarValueMap.end()) {
    VectorShape shape;
    if (vecInfo.hasKnownShape(ScaValue)) {
      shape = getVectorShape(ScaValue);
      if (shape.isUniform()) laneIdx = 0;
    }

    LaneValueVector &laneValues = scalarIt->second;
    if (laneValues.size() > laneIdx) return laneValues[laneIdx];
    else return nullptr;
  } else return nullptr;
}

BasicBlockVector
NatBuilder::getMappedBlocks(BasicBlock *const block) {
  auto blockIt = basicBlockMap.find(block);
  if (!vecInfo.inRegion(*block)) {
    BasicBlockVector blocks;
    blocks.push_back(const_cast<BasicBlock*>(block));
    return blocks;
  }

  assert(blockIt != basicBlockMap.end() && "no mapped blocks for block!");
  return blockIt->second;
}

int
NatBuilder::vectorWidth() const {
  return vecInfo.getMapping().vectorWidth;
}

bool NatBuilder::canVectorize(Instruction *const inst) {
  // whitelisting approach. for direct vectorization we support:
  // binary operations (normal & bitwise), memory access operations, conversion operations and other operations
  // force fallback for instructions in keepScalar
  if (keepScalar.count(inst))
    return false;

  // memory instruction that has no aggregate type anywhere
  if (isa<LoadInst>(inst) || isa<StoreInst>(inst)) {
    LoadInst *load = dyn_cast<LoadInst>(inst);
    StoreInst *store = dyn_cast<StoreInst>(inst);
    Type *accessedType = nullptr;
    if (load) {
      accessedType = load->getType();
    } else {
      assert(store);
      Value *storedValue = store->getValueOperand();
      accessedType = storedValue->getType();
    }

    return !(accessedType->isAggregateType() || accessedType->isVectorTy());
  }
// check for type vectorizability
  auto * instTy = inst->getType();
  if (!instTy->isVoidTy() && !instTy->isIntegerTy() && !instTy->isFloatingPointTy() && !instTy->isPointerTy()) return false;

// for AllocaInst: vectorize if not used in calls. replicate else
  if (isa<AllocaInst>(inst)) {
    for (auto user : inst->users()) {
      if (isa<CallInst>(user) || isa<InvokeInst>(user))
        return false;
    }
    return true;
  } else {
    return isSupportedOperation(inst);
  }
}

bool NatBuilder::shouldVectorize(Instruction *inst) {
  // we should vectorize iff
  // 1) varying vector shape
  // 2) GEP and non-uniform shape
  // 3) return instruction in function with vector return type
  // 4) at least one of these conditions holds for at least one operand
  // Note: branch instructions are never vectorized

  VectorShape shape = getVectorShape(*inst);

  if (isa<BranchInst>(inst)) {
    assert(shape.isUniform() && "non-uniform branch!");
    return false;
  }

  if (isa<GetElementPtrInst>(inst)) {
    GetElementPtrInst *gep = cast<GetElementPtrInst>(inst);
    return !(shape.isUniform() || shape.isContiguous() || shape.isStrided((int) layout.getTypeStoreSize(gep->getResultElementType())));
  }

  if (isa<ReturnInst>(inst)) {
    Function &func = vecInfo.getVectorFunction();
    if (func.getReturnType()->isVectorTy()) {
      IF_DEBUG {
        const VectorShape &retShape = getVectorShape(*inst);
        if (retShape.isUniform()) {
          errs() << "Warning: Uniform return in Function with Vector Type!\n";
          Dump(*inst);
        }
      };
      return true;
    }
  }

  if (shape.isVarying())
    return true;

  for (unsigned i = 0; i < inst->getNumOperands(); ++i) {
    Value *op = inst->getOperand(i);
    VectorShape opShape = getVectorShape(*op);

    if (opShape.isVarying())
      return true;

    if (isa<GetElementPtrInst>(op) && !opShape.isUniform())
      return true;
  }

  return false;
}

bool NatBuilder::isInterleaved(Instruction *inst, Value *accessedPtr, int byteSize, std::vector<Value *> &srcs) {
  return false;
}

void NatBuilder::visitMemInstructions() {
  // iterate over all instructions of all basic blocks (order does not matter)
  // if we encounter a GEP of a memory instruction, check if we would scalarize or optimize it
  // if yes, add the indices that are instructions to the queue
  // then, work the queue until empty: check if the operands are integer instructions
  // if we can keep this instruction scalar, add their users and their operands to the queue
  // instructions that will be kept scalar, are added to a vector
  std::deque<Instruction *> workQueue;
  Function *scalarFn = vecInfo.getMapping().scalarFn;
  for (inst_iterator I = inst_begin(scalarFn), E = inst_end(scalarFn); I != E; ++I) {
    Instruction *inst = &*I;
    GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst);

#if 0
    auto * instTy = inst->getType();
    // force integer loads to scalar
    if (isa<LoadInst>(inst) && inst->getType()->isIntegerTy()) {
      keepScalar.insert(inst);
    } else if (inst->getOpcode() == Instruction::Mul) {
      // integer multiply
      keepScalar.insert(inst);
    } if (instTy->getPrimitiveSizeInBits() > 1 && instTy->isIntegerTy() && vecInfo.getVectorShape(*inst).isVarying()) {
      // force all varying (non-bool) integer operations to scalar
      keepScalar.insert(inst);
    }
#endif

    if (!gep) continue;

    VectorShape addrShape = getVectorShape(*gep);
    Type *accessedType = gep->getResultElementType();
    int byteSize = static_cast<int>(layout.getTypeStoreSize(accessedType));

    // keep scalar if uniform or contiguous
    if (addrShape.isUniform() || addrShape.isContiguous() || addrShape.isStrided(byteSize)) {
      for (unsigned i = 0; i < gep->getNumIndices(); ++i) {
        Value *idxOp = gep->getOperand(i + 1);
        if (isa<Instruction>(idxOp))
          workQueue.push_back(cast<Instruction>(idxOp));
      }
    }
  }

  SmallPtrSet<Instruction *, 16> visited;
  while (!workQueue.empty()) {
    Instruction *inst = workQueue.front();
    workQueue.pop_front();
    visited.insert(inst);
    Type *type = inst->getType();

    // we only care about index calculation, which are exclusively integer type
    if (!type->isIntegerTy()) continue;

    // we will not vectorize it if we do not have to anyway. so nothing to do
    if (!shouldVectorize(inst)) continue;

    // if all users of this instruction are part of the keepScalar set, we keep this one scalar as well
    bool notScalar = false;
    for (auto user : inst->users()) {
      Instruction *uInst = dyn_cast<Instruction>(user);
      GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(user);

      if (!uInst) continue; // nothing to do if it is a constant

      if (gep) {
        VectorShape addrShape = getVectorShape(*gep);
        Type *accessedType = gep->getResultElementType();
        int byteSize = static_cast<int>(layout.getTypeStoreSize(accessedType));

        if (!(addrShape.isUniform() || addrShape.isContiguous() || addrShape.isStrided(byteSize))) {
          notScalar = true;
          break;
        }
        continue;
      }

      if (!keepScalar.count(uInst)) {
        notScalar = true;
        break;
      }
    }

    // cannot keep this scalar -> nothing to do
    if (notScalar) continue;

    // can keep this scalar! add to set and add operands to queue if they are GEP, BC or binOp
    keepScalar.insert(inst);

    // add all operands to the queue
    for (unsigned i = 0, iE = inst->getNumOperands(); i < iE; ++i) {
      Value *op = inst->getOperand(i);
      Instruction *opInst = dyn_cast<Instruction>(op);

      if (opInst && !visited.count(opInst))
        workQueue.push_back(opInst);
    }
  }
}

} // namespace rv
