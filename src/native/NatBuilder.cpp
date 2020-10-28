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
#include <report.h>
#include <fstream>


#include "NatBuilder.h"
#include "Utils.h"

#include "utils/rvTools.h"
#include "rv/transform/redTools.h"
#include "rv/analysis/reductionAnalysis.h"
#include "rv/region/Region.h"
#include "rv/rvDebug.h"
#include "rv/intrinsics.h"
#include "rv/Mask.h"
#include "rv/MaskBuilder.h"

#ifdef LLVM_HAVE_VP
#include <llvm/IR/VPBuilder.h>
#endif

#include "rvConfig.h"
#include "ShuffleBuilder.h"

#if 1
#define IF_DEBUG_NAT IF_DEBUG
#else
#define IF_DEBUG_NAT IF_DEBUG
#endif

using namespace llvm;

unsigned
GetVectorNumElements(Type * VecTy) {
  return cast<FixedVectorType>(VecTy)->getNumElements();
}

// TODO move to vector builder class...
Value*
CreateBroadcast(IRBuilder<> & builder, Value & vec, int idx) {
  auto * intTy = Type::getInt32Ty(builder.getContext());
  auto *vecTy = cast<VectorType>(vec.getType());
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
  return ConstantVector::getSplat(ElementCount::getFixed(vectorWidth()), Elt);
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

VectorShape NatBuilder::getVectorShape(const Value &val) const {
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
    phiVector()
{}

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

    BasicBlock *vecBlock = getVectorBlock(*bb, false); // TODO assert uniqueness
    vectorize(bb, vecBlock);

    // populate queue with pre-order dominators
    for (auto *N : node->children()) {
      nodeQueue.push_back(N);
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
  IF_DEBUG_NAT { errs() << ":: Vectorizing block: " << bb->getName().str() << "\n"; }

  builder.SetInsertPoint(vecBlock);
  for (BasicBlock::iterator it = bb->begin(), ie = bb->end(); it != ie; ++it) {
    Instruction *inst = &*it;

    IF_DEBUG_NAT { errs() << "Vectorizing:\n\t"; errs() << *inst << "\n"; }

    PHINode *phi = dyn_cast<PHINode>(inst);
    LoadInst *load = dyn_cast<LoadInst>(inst);
    StoreInst *store = dyn_cast<StoreInst>(inst);
    CallInst *call = dyn_cast<CallInst>(inst);
    GetElementPtrInst *gep = dyn_cast<GetElementPtrInst>(inst);
    BitCastInst *bc = dyn_cast<BitCastInst>(inst);
    AllocaInst *alloca = dyn_cast<AllocaInst>(inst);

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
      vectorizeMemoryInstruction(inst);
    else if (call) {
      // calls need special treatment
      switch (GetIntrinsicID(*call)) {
        case RVIntrinsic::EntryMask: mapVectorValue(call, vecMaskArg); break;

        case RVIntrinsic::Any:
        case RVIntrinsic::All:
        case RVIntrinsic::Ballot:
        case RVIntrinsic::PopCount:
          vectorizeMaskReductionCall(call, GetIntrinsicID(*call));
          break;

        case RVIntrinsic::Extract: vectorizeExtractCall(call); break;
        case RVIntrinsic::Insert: vectorizeInsertCall(call); break;
        case RVIntrinsic::Compact: vectorizeCompactCall(call); break;
        // case RVIntrinsic::Mask: mapVectorValue(call, requestVectorMask(*call->getParent())); break;
        case RVIntrinsic::VecLoad: vectorizeLoadCall(call); break;
        case RVIntrinsic::VecStore: vectorizeStoreCall(call); break;
        case RVIntrinsic::Shuffle: vectorizeShuffleCall(call); break;
        case RVIntrinsic::Index: vectorizeIndexCall(*call); break;
        case RVIntrinsic::Align: vectorizeAlignCall(call); break;
        default: {
          if (shouldVectorize(call)) vectorizeCallInstruction(call);
          else copyCallInstruction(call);
        }
      }
    } else if (phi) {
      // phis need special treatment as they might contain not-yet mapped instructions
      vectorizePHIInstruction(phi);
    } else if (alloca && shouldVectorize(inst)) {
      vectorizeAlloca(alloca);
    } else if (gep || bc) {
      continue; // skipped
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
NatBuilder::createAnyGuard(bool instNeedsGuard, BasicBlock & origBlock, bool producesValue, std::function<Value*(IRBuilder<>&)> genFunc) {
  Mask vecMask = requestVectorMask(origBlock);
  auto ScalarMask = vecInfo.getMask(origBlock);
  // assert(!vecMask.getAVL() && "TODO implement for AVL");
  // only emit a guard if rv_any(p) may be false AND the emitted instructions will need it.
  bool needsGuard =
      instNeedsGuard && !undeadMasks.isUndead(ScalarMask, origBlock);

  BasicBlock* memBlock, * continueBlock;

  // prologue (guard branch)
  if (needsGuard) {
    // create a mask ptest
    Value *anyMask = createVectorMaskSummary(vecMask, RVIntrinsic::Any, nullptr);

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

    BasicBlock &vecOrigBlock = *getVectorBlock(origBlock, true);
    PHINode *phi = producesValue ? builder.CreatePHI(vecMem->getType(), 2, "scal_mask_mem_phi") : nullptr;

    if (phi) {
      phi->addIncoming(vecMem, memBlock);
      phi->addIncoming(UndefValue::get(vecMem->getType()), &vecOrigBlock);
      vecMem = phi;
    }

    mapVectorValue(&origBlock, continueBlock);
  }

  return *vecMem;
}

ValVec
NatBuilder::scalarizeCascaded(BasicBlock & srcBlock, Instruction & inst, bool packResult, std::function<Value*(IRBuilder<>&,size_t)> genFunc) {
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
     Value *LaneMask = requestLanePredicate(srcBlock, lane); // do not map this value if it's fresh to avoid dominance violations
     builder.CreateCondBr(LaneMask, maskedBlock, nextBlock);

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
      Mask vecMask = requestVectorMask(*scaCall.getParent());
      if (vecMask.getAVL()) {
        Report() << "Ignoring AVL in call!\n";
      }
      vectorArgs.push_back(vecMask.getPred());
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
  llvm::Align allocAlign = allocaInst->getAlign();
  auto * allocTy = allocaInst->getType()->getElementType();

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
    baseAlloca->setAlignment(allocAlign);

    // extract basePtrs
    auto * offsetVec = createContiguousVector(vectorWidth(), indexTy, 0, 1);
    auto * allocaPtrVec = builder.CreateGEP(baseAlloca, offsetVec, name + ".alloca_vec");

    // register as vectorized alloca
    mapVectorValue(allocaInst, allocaPtrVec);
    return;
  }

  // TODO implement a batter vectorization scheme

  // fallback code path
  replicateInstruction(allocaInst);
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

#ifdef LLVM_HAVE_VP
  if (config.enableVP) {
    VPBuilder vpBuilder(builder);

    // use the Explicit Vector Length extension
    Mask blockMask;
    if (!hasTotalOperationTag(*inst)) {
      blockMask = requestVectorMask(*inst->getParent());
    }

    // configure predicate
    vpBuilder.setMask(blockMask.getPred());
    vpBuilder.setEVL(blockMask.getAVL());
    vpBuilder.setStaticVL(vecInfo.getVectorWidth());

    // request all operands
    SmallVector<Value*, 4> vecOperandVec;
    for (int opIdx = 0; opIdx < (int) inst->getNumOperands(); ++opIdx) {
      vecOperandVec.push_back(requestVectorValue(inst->getOperand(opIdx)));
    }

    // use VP intrinsics where available
    auto * vpInst = vpBuilder.CreateVectorCopy(*inst, vecOperandVec);
    if (vpInst) {
      mapVectorValue(inst, vpInst);
      return;
    }
  }
#endif

  // Otw, use the legacy code path
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

void NatBuilder::vectorizeMaskReductionCall(CallInst *rvCall, RVIntrinsic MaskIntrin) {
  assert(rvCall->getNumArgOperands() == 1 && "expected only 1 argument for rv_any");

  Value *predicate = rvCall->getArgOperand(0);
  const VectorShape &shape = getVectorShape(*predicate);
  assert((shape.isVarying() || shape.isUniform()) && "predicate can't be contigious or strided");

  // Factor the black into the argument mask
  Mask maskedMask = maskInactiveLanes(*predicate, *rvCall->getParent(), false); //isRv_all);
  Value *reduction = createVectorMaskSummary(maskedMask, MaskIntrin, rvCall->getType());
  mapScalarValue(rvCall, reduction);

  ++numRVIntrinsics;
}

void
NatBuilder::vectorizeExtractCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->getNumArgOperands() == 2 && "expected 2 arguments for rv_extract(vec, laneId)");

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

  assert(rvCall->getNumArgOperands() == 3 && "expected 3 arguments for rv_insert(vec, laneId, value)");

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

  assert(rvCall->getNumArgOperands() == 2 && "expected 2 arguments for rv_load(vecPtr, laneId)");

  Value *vecPtr = rvCall->getArgOperand(0);
  assert(getVectorShape(*rvCall->getArgOperand(1)).isUniform());
  auto * laneId = requestScalarValue(rvCall->getArgOperand(1));

// uniform arg
  Value * laneVal = nullptr;
  if (getVectorShape(*vecPtr).isUniform()) {
    auto * uniVal = requestScalarValue(vecPtr);
    auto addressSpace = uniVal->getType()->getPointerAddressSpace();
    auto * castPtr = builder.CreatePointerCast(uniVal, PointerType::get(builder.getFloatTy(), addressSpace));
    auto * gepPtr = builder.CreateGEP(castPtr, laneId);
    laneVal = builder.CreateLoad(gepPtr);
  } else {
// non-uniform arg
    auto * vecVal = requestVectorValue(vecPtr);
    auto * lanePtr = builder.CreateExtractElement(vecVal, laneId, "rv_load");
    laneVal = builder.CreateLoad(lanePtr, "rv_load");
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

  assert(rvCall->getNumArgOperands() == 3 && "expected 3 arguments for rv_store(vecPtr, laneId, value)");

  Value *vecPtr  = rvCall->getArgOperand(0);
  Value *elemVal = requestScalarValue(rvCall->getArgOperand(2));
  auto * laneId = requestScalarValue(rvCall->getArgOperand(1));

// uniform arg
  if (getVectorShape(*vecPtr).isUniform()) {
    auto * uniVal = requestScalarValue(vecPtr);
    auto addressSpace = uniVal->getType()->getPointerAddressSpace();
    auto * castPtr = builder.CreatePointerCast(uniVal, PointerType::get(builder.getFloatTy(), addressSpace));
    auto * gepPtr = builder.CreateGEP(castPtr, laneId );
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

  assert(rvCall->getNumArgOperands() == 2 && "expected 2 arguments for rv_shuffle(vec, shift)");

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
NatBuilder::createVectorMaskSummary(Mask vecMask, RVIntrinsic mode, Type * indexTy) {
//  Module *mod = vecInfo.getMapping().vectorFn->getParent();

  // TODO finalize

  Value * result = nullptr;
  switch (mode) {
    case RVIntrinsic::Ballot: {
#if 0
      // TODO return (1 << %lavl) - 1 if mask is all true
      if (vecMask.knownAllTruePred()) {
        // avl == number of lanes
        return &vecMask.requestAVLAsValue(builder.getContext());
      } 
#endif

      SmallVector<Constant*, 64> LaneIndices;
      for (int i = 0; i < vectorWidth(); ++i) {
        LaneIndices.push_back(ConstantInt::get(indexTy, 1 << i));
      }
      auto IndexVec = ConstantVector::get(LaneIndices);
      result = &CreateMaskedVectorReduce(config, builder, vecMask, RedKind::Add, *IndexVec, nullptr);
    } break;

    case RVIntrinsic::PopCount: {
      if (vecMask.knownAllTruePred()) {
        // avl == number of lanes
        return &vecMask.requestAVLAsValue(builder.getContext());
      } 

      auto AllOnes = getSplat(ConstantInt::get(indexTy, 1, false));
      result = &CreateMaskedVectorReduce(config, builder, vecMask, RedKind::Add, *AllOnes, nullptr);
    } break;

    case RVIntrinsic::Any:
    case RVIntrinsic::All: {
      if (vecMask.knownAllTrue()) {
        // TODO use UndeadMaskAnalysis
        return builder.getTrue();
      }

      RedKind redKind = (mode == RVIntrinsic::Any) ? RedKind::Or : RedKind::And;
      Mask RedMask = vecMask.getAVL() ? Mask::fromVectorLength(*vecMask.getAVL()) : Mask::getAllTrue();
      result = &CreateMaskedVectorReduce(config, builder, RedMask, redKind, vecMask.requestPredAsValue(builder.getContext()), nullptr);
    } break;

    default: abort();
  }

  return result;
}

void
NatBuilder::vectorizeBallotCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  auto vecWidth = vecInfo.getVectorWidth();
  assert((vecWidth == 4 || vecWidth == 8) && "rv_ballot only supports SSE and AVX instruction sets");
  assert(rvCall->getNumArgOperands() == 1 && "expected 1 argument for rv_ballot(cond)");

  Value *condArg = rvCall->getArgOperand(0);

// non-uniform arg
  Mask vecMask = maskInactiveLanes(*condArg, *rvCall->getParent(), false);
  auto * mask = createVectorMaskSummary(vecMask, RVIntrinsic::Ballot, rvCall->getType());
  mapScalarValue(rvCall, mask);
}

void
NatBuilder::vectorizeIndexCall(CallInst & rvCall) {
  ++numRVIntrinsics;

// avx512vl - expand based implementation
  if (config.useAVX512) {
    auto vecWidth = vecInfo.getVectorWidth();
    assert(vecWidth == 4 || vecWidth == 8);

    Intrinsic::ID id = Intrinsic::x86_avx512_mask_expand;

    assert(rvCall.getNumArgOperands() == 1 && "expected 1 argument for rv_index(mask)");

    Value *condArg = rvCall.getArgOperand(0);

    auto * intLaneTy = IntegerType::getIntNTy(rvCall.getContext(), 512 / vecWidth);
    bool argUniform = hasUniformPredicate(*rvCall.getParent()) && vecInfo.getVectorShape(*condArg).isUniform();

// uniform arg
    if (argUniform) {
      mapScalarValue(&rvCall, createContiguousVector(vecWidth, intLaneTy, 0, 1));
      return;
    }

    Mask vecMask = maskInactiveLanes(*condArg, *rvCall.getParent(), false);
    auto *maskVec = vecMask.getPred();
    assert(!vecMask.getAVL() && "TODO implement for AVL");
    auto * contVec = createContiguousVector(vecWidth, intLaneTy, 0, 1);


    auto * fpLaneTy = Type::getDoubleTy(rvCall.getContext());
    auto * fpVecTy =  FixedVectorType::get(fpLaneTy, vecWidth);
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
  }

// generic implementation
  assert(config.useAVX512 && "TODO generic implementation (avx512vl only)");
  abort(); // "TODO generic implementation (avx512vl only)"
}

void
NatBuilder::vectorizePopCountCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  auto indexTy = rvCall->getType();

  assert(rvCall->getNumArgOperands() == 1 && "expected 1 argument for rv_ballot(cond)");

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
  Mask vecMask = maskInactiveLanes(*condArg, *rvCall->getParent(), false);
  auto * mask = createVectorMaskSummary(vecMask, RVIntrinsic::PopCount, rvCall->getType());
  mapScalarValue(rvCall, mask);
}

void
NatBuilder::vectorizeAlignCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->getNumArgOperands() == 2 && "expected 2 arguments for rv_align(ptr, alignment)");

  Value *vecArg = rvCall->getArgOperand(0);

  if (getVectorShape(*vecArg).isVarying())
    mapVectorValue(rvCall, requestVectorValue(vecArg));
  else
    mapScalarValue(rvCall, requestScalarValue(vecArg));
}

void
NatBuilder::vectorizeCompactCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  abort(); // TODO implement for VP
#if 0
  assert(rvCall->getNumArgOperands() == 2 && "expected 2 arguments for rv_compact(vec, mask)");

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

  auto vecWidth = cast<VectorType>(maskVal->getType())->getVectorNumElements();
  auto tableIndex = createVectorMaskSummary(*rvCall->getType(), vecMask, builder, RVIntrinsic::Ballot);
  auto table = createCompactLookupTable(vecWidth);
  auto indices = builder.CreateLoad(builder.CreateInBoundsGEP(table, { builder.getInt32(0), tableIndex }), "rv_compact_indices");
  Value * compacted = UndefValue::get(vecVal->getType());
  for (size_t i = 0; i < vecWidth; ++i) {
    auto index = builder.CreateExtractElement(indices, builder.getInt32(i), "rv_compact_index");
    auto elem = builder.CreateExtractElement(vecVal, index, "rv_compact_elem");
    compacted = builder.CreateInsertElement(compacted, elem, builder.getInt32(i), "rv_compact");
  }
  mapVectorValue(rvCall, compacted);
#endif
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
  auto attribSet = srcFunc.getAttributes().getFnAttributes();

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
  for (int i = 0; i < (int) scalCall->getNumArgOperands(); ++i) {
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
  auto scaMask = vecInfo.getMask(scaBlock);
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

    auto & vecCall = createAnyGuard(needsGuardedCall, *scalCall->getParent(), producesValue,
      [&](IRBuilder<> & builder) {
        return builder.CreateCall(&simdFunc, vectorArgs, callName);
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
      //if (scalCall->getNumArgOperands() > 0) {
        // doublePrecision = scalCall->getArgOperand(0)->getType()->isDoubleTy();
      // }
      Function &simdFunc = funcResolver->requestVectorized();
      CopyTargetAttributes(simdFunc, vecInfo.getScalarFunction());

      ShuffleBuilder appender(vectorWidth());
      ShuffleBuilder extractor(vecWidth);

      // prepare the extract shuffler
      for (unsigned i = 0; i < scalCall->getNumArgOperands(); ++i) {
        Value *const arg = scalCall->getArgOperand(i);
        Value *mappedArg = requestVectorValue(arg);
        extractor.add(mappedArg);
      }

      // replicate vector call
      for (unsigned i = 0; i < replicationFactor; ++i) {
        CallInst *call = cast<CallInst>(scalCall->clone());
        call->setCalledFunction(&simdFunc);
        call->mutateType(simdFunc.getReturnType());

        // insert arguments into call
        for (unsigned j = 0; j < scalCall->getNumArgOperands(); ++j) {
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
    auto &ScaBlock = *scalCall->getParent();
    bool needsPredicate = vecInfo.hasMask(ScaBlock) && !vecInfo.getMask(ScaBlock).knownAllTrue();
    bool needCascade = needsPredicate && scalCall->mayHaveSideEffects();

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
  for (unsigned i = 0; i < scalCall->getNumArgOperands(); ++i) {
    Value *scalArg = scalCall->getArgOperand(i);
    Value *laneArg = requestScalarValue(scalArg, laneIdx);
    args.push_back(laneArg);
  }

  Value *call = builder.CreateCall(scalCall->getFunctionType(), callee, args,
                                   scalCall->getName());
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

  assert(accessedType == cast<PointerType>(accessedPtr->getType())->getElementType() &&
         "accessed type and pointed object type differ!");
  assert(vecInfo.hasKnownShape(*accessedPtr) && "no shape for accessed pointer!");
  VectorShape addrShape = getVectorShape(*accessedPtr);
  Type *vecType = getVectorType(accessedType, vectorWidth());

  Mask scaMask = vecInfo.getMask(*inst->getParent());
  bool needsMask = !scaMask.knownAllTrue() && !vecInfo.getVectorShape(scaMask).isUniform();

  // uniform loads from allocations do not need a mask!
  if (needsMask && load && GetUnderlyingAlloca(accessedPtr)) {
    needsMask = false;
  }

  Mask vecMask;
  if (needsMask) {
    vecMask = requestVectorMask(*inst->getParent());
  } else {
    vecMask = Mask::getAllTrue();
  }

  // generate the address for the memory instruction now
  // uniform: uniform GEP
  // contiguous: contiguous GEP
  // interleaved: multiple contiguous GEPs
  // varying: varying vector GEP

  unsigned alignment;
  int byteSize = static_cast<int>(layout.getTypeStoreSize(accessedType));

  // determine alignment
  if (!addrShape.isVarying()) {
    // scalar access
    alignment = addrShape.getAlignmentFirst();

  } else {
    alignment = addrShape.getAlignmentGeneral();
  }
  unsigned origAlignment = load ? load->getAlignment() : store->getAlignment();
  alignment = std::max<unsigned>(alignment, origAlignment);

  Value *vecMem = nullptr;
  if (load) {
    if (addrShape.isUniform()) {
      // proper uniform access
      Value *vecBasePtr = requestScalarValue(accessedPtr);
      vecMem = createUniformMaskedMemory(load, accessedType, Align(alignment), vecBasePtr, nullptr);

    } else if (addrShape.isStrided(byteSize)) {
      // proper contiguous access
      Value *vecBasePtr = requestScalarValue(accessedPtr);
      vecMem = createContiguousLoad(vecBasePtr, Align(alignment), vecMask, UndefValue::get(vecType));

      addrShape.isUniform() ? ++numUniLoads : needsMask ? ++numContMaskedLoads : ++numContLoads;

    } else {
      // otw
      auto *vecAddr = requestVectorValue(accessedPtr);
      vecMem = createVaryingMemory(vecType, Align(alignment), vecAddr, vecMask, nullptr);
    }


  } else {
    // store
    if (addrShape.isUniform()) {
      // proper uniform
      auto valShape = vecInfo.getVectorShape(*store->getValueOperand());
      Value *vecBasePtr = requestScalarValue(accessedPtr);
      if (!valShape.isUniform()) {
        Value *vecValue = requestVectorValue(storedValue);
        vecMem = createVaryingToUniformStore(store, accessedType, alignment, vecBasePtr, vecMask, vecValue);
      } else {
        assert((vecInfo.getVectorShape(*storedValue).isUniform()) && "trying to store a varying value to a uniform ptr!");
        Value *vecValue = requestScalarValue(storedValue);
        vecMem = createUniformMaskedMemory(store, accessedType, Align(alignment), vecBasePtr, vecValue);
      }

    } else if (addrShape.isStrided(byteSize)) {
      // proper contiguous
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
      Value *vecBasePtr = requestScalarValue(accessedPtr);
      vecMem = createContiguousStore(mappedStoredVal, vecBasePtr, Align(alignment), vecMask);

      addrShape.isUniform() ? ++numUniStores : needsMask ? ++numContMaskedStores : ++numContStores;

    } else {
      // otw
      Value *vecPtr = requestVectorValue(accessedPtr);
      Value *vecStoreVal = requestVectorValue(storedValue);
      vecMem = createVaryingMemory(vecType, Align(alignment), vecPtr, vecMask, vecStoreVal);
    }
  }

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


Value *
NatBuilder::createVaryingToUniformStore(Instruction *scaInst, Type *accessedType, unsigned int alignment, Value *addr, Mask vecMask, Value *values) {
  assert(!vecMask.getAVL() && "TODO implement AVL");

  bool needsGuard = true;
  return &createAnyGuard(needsGuard, *scaInst->getParent(), isa<LoadInst>(scaInst),
      [&](IRBuilder<> & builder)
  {
    auto & ctx = builder.getContext();
    Value * indexVal = nullptr;

    if (vecMask.knownAllTrue()) {
      // under uniform contexts store the value of the last lane
      indexVal = ConstantInt::get(Type::getInt32Ty(ctx), vectorWidth() - 1);
    } else {
      auto * nativeIntTy = Type::getInt32Ty(ctx);

      // SExt to full width int
      auto * vecLaneTy = FixedVectorType::get(nativeIntTy, vectorWidth());
      auto * sxMask = builder.CreateSExt(vecMask.getPred(), vecLaneTy);

      // AND with lane index vector
      auto * laneIdxConst = createContiguousVector(vectorWidth(), nativeIntTy, 0, 1);
      auto * activeLaneVec = builder.CreateAnd(sxMask, laneIdxConst);

      // horizontal MAX reduction
      indexVal = &CreateVectorReduce(config, builder, RedKind::UMax, *activeLaneVec, nullptr);
    }

    // extract and materialize store
    auto * lastLaneVal = builder.CreateExtractElement(values, indexVal, "xt.lastlane");
    auto * vecMem = builder.CreateStore(lastLaneVal, addr);
    cast<StoreInst>(vecMem)->setAlignment(Align(alignment));
    return vecMem;
  });
}

Value *NatBuilder::createUniformMaskedMemory(Instruction *scaInst,
                                             Type *accessedType,
                                             MaybeAlign AlignOpt,
                                             Value *vecBasePtr, Value *vecValues) {
  vecValues ? ++numUniMaskedStores : ++numUniMaskedLoads;

  // emit a scalar memory acccess within a any-guarded section
  bool needsGuard = false; // FIXME
  return &createAnyGuard(needsGuard, *scaInst->getParent(), isa<LoadInst>(scaInst),
      [=](IRBuilder<> & builder)
  {
    Instruction* vecMem;
    if (vecValues) {
      vecMem = builder.CreateStore(vecValues, vecBasePtr);
      cast<StoreInst>(vecMem)->setAlignment(AlignOpt.valueOrOne());
    } else {
      vecMem = builder.CreateLoad(vecBasePtr, "scal_mask_mem");
      cast<LoadInst>(vecMem)->setAlignment(AlignOpt.valueOrOne());
    }
    return vecMem;
  });
}

Value *NatBuilder::createVaryingMemory(Type *vecType, Align alignment, Value *addr, Mask mask, Value *values) {
  bool scatter(values != nullptr);
  bool maskNonConst = !mask.knownAllTrue();
  maskNonConst ? (scatter ? ++numMaskedScatter : ++numMaskedGather) : (scatter ? ++numScatter : ++numGather);

#ifdef LLVM_HAVE_VP
  if (config.enableVP) {
    VPBuilder vpBuilder(builder);
    vpBuilder.setStaticVL(vecInfo.getVectorWidth());
    vpBuilder.setMask(mask.getPred());
    vpBuilder.setEVL(mask.getAVL());
    if (scatter) {
       return &vpBuilder.CreateScatter(*values, *addr, alignment);
    } else {
       return &vpBuilder.CreateGather(*addr, alignment);
    }
  }
#endif

  auto * vecPtrTy = addr->getType();

  std::vector<Value *> args;
  if (scatter) args.push_back(values);
  args.push_back(addr);
  args.push_back(ConstantInt::get(i32Ty, alignment.value()));
  args.push_back(mask.getPred());
  if (!scatter) args.push_back(UndefValue::get(vecType));
  Module *mod = vecInfo.getMapping().vectorFn->getParent();
  Function *intr = scatter ? Intrinsic::getDeclaration(mod, Intrinsic::masked_scatter, {vecType, vecPtrTy})
                           : Intrinsic::getDeclaration(mod, Intrinsic::masked_gather, {vecType, vecPtrTy});
  assert(intr && "scatter/gather not found!");
  return builder.CreateCall(intr, args);
}

Value *NatBuilder::createContiguousStore(Value *vecVal, Value *elemPtr, Align alignment, Mask vecMask) {
#ifdef LLVM_HAVE_VP
  if (config.enableVP) {
    VPBuilder vpBuilder(builder);
    vpBuilder.setStaticVL(vecInfo.getVectorWidth());
    vpBuilder.setMask(vecMask.getPred());
    vpBuilder.setEVL(vecMask.getAVL());
    return &vpBuilder.CreateContiguousStore(*vecVal, *elemPtr, alignment);
  }
#endif

  auto vecPtr = builder.CreatePointerCast(elemPtr, vecVal->getType()->getPointerTo(cast<PointerType>(elemPtr->getType())->getAddressSpace()));
  if (!vecMask.knownAllTrue()) {
    return builder.CreateMaskedStore(vecVal, vecPtr, alignment, vecMask.getPred());

  } else {
    StoreInst *store = builder.CreateStore(vecVal, vecPtr);
    store->setAlignment(Align(alignment));
    return store;
  }
}

Value *NatBuilder::createContiguousLoad(Value *elemPtr, Align alignment, Mask vecMask, Value *passThru) {
#ifdef LLVM_HAVE_VP
  if (config.enableVP) {
    VPBuilder vpBuilder(builder);
    vpBuilder.setStaticVL(vecInfo.getVectorWidth());
    vpBuilder.setMask(vecMask.getPred());
    vpBuilder.setEVL(vecMask.getAVL());
    return &vpBuilder.CreateContiguousLoad(*elemPtr, alignment);
  }
#endif

  auto ElemPtrTy = elemPtr->getType();
  auto ScaElemTy = ElemPtrTy->getPointerElementType();
  auto VecElemPtrTy = ScaElemTy->getPointerTo(ElemPtrTy->getPointerAddressSpace());
  auto VecPtr = builder.CreatePointerCast(elemPtr, VecElemPtrTy);
  if (!vecMask.knownAllTrue()) {
    return builder.CreateMaskedLoad(VecPtr, alignment, vecMask.getPred(), passThru, "cont_load");

  } else {
    LoadInst *load = builder.CreateLoad(VecPtr, "cont_load");
    load->setAlignment(Align(alignment));
    return load;
  }
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
    auto * ptrElemTy = GetPointerElementType(scalarPtrTy);

    auto AddrSpace = scalarPtrTy->getAddressSpace();

    // vecValue is a single pointer and has to be broadcasted to a vector of pointers first
    vecValue = builder.CreateVectorSplat(vectorWidth(), &scaValue);
    auto * actualPtrVecTy = vecValue->getType();

    if (!vecShape.isUniform()) { // stride != 0
      assert(ptrElemTy->isSized() && "byte-stride shape on unsized element type");
      int scalarBytes = static_cast<int>(layout.getTypeStoreSize(ptrElemTy));
      if (vecShape.getStride() % scalarBytes == 0) {
        // stride aligned with object size
        Value *contVec = createContiguousVector(vectorWidth(), intTy, 0, vecShape.getStride() / scalarBytes);
        vecValue = builder.CreateGEP(vecValue, contVec, "expand_strided_ptr");
      } else {
        // sub element stride
        auto * charPtrTy = builder.getInt8PtrTy(AddrSpace);
        auto * charPtrVec = builder.CreatePointerCast(vecValue, FixedVectorType::get(charPtrTy, vectorWidth()), "byte_ptr");
        Value *contVec = createContiguousVector(vectorWidth(), intTy, 0, vecShape.getStride());
        auto * bytePtrVec = builder.CreateGEP(charPtrVec, contVec, "expand_byte_ptr");
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
      if (mappedInst){
        SetInsertPointAfterMappedInst(builder, mappedInst);
      }

      Constant *laneInt = type->isFloatingPointTy() ? ConstantFP::get(type, laneIdx * shape.getStride())
                                                    : ConstantInt::get(type, laneIdx * shape.getStride());
      reqVal = type->isFloatingPointTy() ? builder.CreateFAdd(mappedVal, laneInt,
                                                              value->getName() + "lane" + std::to_string(laneIdx))
                                         : builder.CreateAdd(mappedVal, laneInt,
                                                             value->getName() + "_lane" + std::to_string(laneIdx));
      if (mappedInst)
        builder.SetInsertPoint(oldIB, oldIP);
    }
  }

  // if value has a vector mapping -> extract from vector. if not -> clone scalar op
  if (!reqVal) {
    // to avoid dominance problems assume: if we only have a vectorized value and need a scalar one -> do not map
    skipMapping = true;
    mappedVal = getVectorValue(*value);
    Instruction *mappedInst = dyn_cast<Instruction>(mappedVal);
    auto oldIP = builder.GetInsertPoint();
    auto oldIB = builder.GetInsertBlock();
    if (mappedInst) {
      SetInsertPointAfterMappedInst(builder, mappedInst);
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
      reqVal = builder.CreateGEP(mappedVal, ConstantInt::get(indexTy, laneIdx));
    } else {
      // extract from GEPs are not allowed. in that case recreate the scalar instruction and get that new value
      if (isa<GetElementPtrInst>(mappedVal) && isa<GetElementPtrInst>(value)) {
        auto indexTy = getIndexTy(mappedVal);
        reqVal = builder.CreateGEP(mappedVal, ConstantInt::get(indexTy, laneIdx));
      } else {
        reqVal = builder.CreateExtractElement(mappedVal, ConstantInt::get(i32Ty, laneIdx), "extract");
      }
    }


    if (reqVal->getType() != value->getType()) {
      reqVal = builder.CreateBitCast(reqVal, value->getType(), "bc");
    }

    if (mappedInst)
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
  VectorShape basePtrShape = getVectorShape(*basePtr);

  Value *vecBasePtr;
  if (buildScalar || basePtrShape.isUniform())
    vecBasePtr = requestScalarValue(basePtr, laneIdx);
  else
    vecBasePtr = requestVectorValue(basePtr);

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

  Value * vecGEP = builder.CreateGEP(vecBasePtr, idxList, gep->getName());
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

  auto *interGEP = builder.CreateGEP(basePtr, idxList, "inter_gep");
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
    interAddr = builder.CreateGEP(ptr, ConstantInt::get(indexTy, vectorWidth() * interleavedIdx), "inter_gep");
  }

  int AddrSpace = cast<PointerType>(addr->getType())->getAddressSpace();
  PointerType *vecPtrType = vecType->getPointerTo(AddrSpace);
  return builder.CreatePointerCast(interAddr, vecPtrType, "inter_cast");
}

llvm::Value *
NatBuilder::requestCascadeLoad(Value *vecPtr, unsigned alignment, Value *mask) {
  Type *elementPtrType = cast<VectorType>(vecPtr->getType())->getElementType();
  Type *accessedType = cast<PointerType>(elementPtrType)->getElementType();
  unsigned bitWidth = accessedType->getScalarSizeInBits();

  Function *func = getCascadeFunction(bitWidth, false);
  if (!func) {
    func = createCascadeMemory(cast<VectorType>(vecPtr->getType()), alignment, cast<VectorType>(mask->getType()),
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
    func = createCascadeMemory(cast<VectorType>(vecPtr->getType()), alignment, cast<VectorType>(mask->getType()),
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

Function *NatBuilder::createCascadeMemory(VectorType *pointerVectorType, unsigned alignment, VectorType *maskType,
                                          bool store) {
  assert(cast<VectorType>(pointerVectorType)->getElementType()->isPointerTy()
         && "pointerVectorType must be of type vector of pointer!");
  assert(cast<VectorType>(maskType)->getElementType()->isIntegerTy(1)
         && "maskType must be of type vector of i1!");


  Module *mod = vecInfo.getScalarFunction().getParent();
  IRBuilder<> builder(mod->getContext());

  // create function
  Type *accessedType = cast<PointerType>(pointerVectorType->getElementType())->getElementType();
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
      Value *loadInst = builder.CreateLoad(pointerLaneVal, "load_lane_" + std::to_string(i));
      cast<LoadInst>(loadInst)->setAlignment(Align(alignment));
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
  auto * redFunc = platInfo.requestVectorMaskReductionFunc("rv_reduce_or", GetVectorNumElements(vector->getType()));
  ptest = builder.CreateCall(redFunc, vector, "ptest");

  if (isRv_all) {
    ptest = builder.CreateNot(ptest, "rvall_not");
  }

  return ptest;
}

bool
NatBuilder::hasUniformPredicate(const BasicBlock & BB) const {
  // FIXME use getMask instead
  if (!vecInfo.getRegion().contains(&BB) || !vecInfo.getPredicate(BB)) return true;
  else return vecInfo.getVectorShape(*vecInfo.getPredicate(BB)).isUniform();
}

Value*
NatBuilder::requestLanePredicate(const BasicBlock &ScaBlock, int Lane) {
  auto &Ctx = ScaBlock.getContext();

  // FIXME use getMask instead
  if (!vecInfo.hasMask(ScaBlock) || vecInfo.getMask(ScaBlock).knownAllTrue()) {
    return ConstantInt::getTrue(builder.getContext());
  }
  auto ScaMask = vecInfo.getMask(ScaBlock);
  auto ItMaskLane = MaskLaneMap.find(std::make_pair<>(ScaMask, Lane));
  if (ItMaskLane != MaskLaneMap.end()) {
    return ItMaskLane->second;
  }

  Value *LaneP = requestScalarValue(&ScaMask.requestPredAsValue(Ctx));

  // Perform a scalar comparision against the AVL (if set)
  if (ScaMask.getAVL()) {
    auto *VecAVL = requestScalarValue(ScaMask.getAVL());
    auto *LaneConst = ConstantInt::get(ScaMask.getAVL()->getType(), Lane, false);
    auto *LessThanAVL = builder.CreateICmpULT(LaneConst, VecAVL, "avl_check");
    LaneP =  builder.CreateAnd(LaneP, LessThanAVL, "mask_check");
  }
  
  MaskLaneMap[std::make_pair<>(ScaMask, Lane)] = LaneP;
  return LaneP;
}

Mask
NatBuilder::requestVectorized(Mask ScaMask) {
  auto VecPred = ScaMask.getPred() ? requestVectorValue(ScaMask.getPred()) : nullptr;
  auto VecAVL = ScaMask.getAVL() ? requestScalarValue(ScaMask.getAVL()) : nullptr;
  return Mask(VecPred, VecAVL);
}

Mask
NatBuilder::requestVectorMask(const BasicBlock& ScaBlock) {
  if (!vecInfo.hasMask(ScaBlock) || vecInfo.getMask(ScaBlock).knownAllTrue()) {
    return Mask::getAllTrue();
  }
  Mask ScaMask = vecInfo.getMask(ScaBlock);
  return requestVectorized(ScaMask);
}

Mask
NatBuilder::maskInactiveLanes(Value &ScaValue, const BasicBlock &Block, bool NegateArg) {
  Mask VecBlockMask = requestVectorMask(Block);
  auto VecArgMask = requestVectorized(Mask::inferFromPredicate(ScaValue));

  VectorMaskBuilder MBuilder;
  if (NegateArg) {
    return MBuilder.CreateAnd(builder, MBuilder.CreateNot(builder, VecArgMask), VecBlockMask);
  } else {
    return MBuilder.CreateAnd(builder, VecArgMask, VecBlockMask);
  }
}

void
NatBuilder::repairOutsideUses(Instruction & scaChainInst, std::function<Value& (Value &,BasicBlock &)> repairFunc) {
  std::map<BasicBlock*, Value*> fixMap;

  // actually used value
  Value * vecUsed = getScalarValue(scaChainInst);
  if (!vecUsed) vecUsed = getVectorValue(scaChainInst); // FIXME

  assert(vecUsed && "could not infer vector version of scalar instruction");

  // iterate over all uses and invoke @repairFunc for use edges that leave the region
  for (auto itUse = scaChainInst.use_begin(); itUse != scaChainInst.use_end(); ){
    auto & use = *itUse++;

    int opIdx = use.getOperandNo();
    auto & userInst = cast<Instruction>(*use.getUser());

    // user is in the region
    if (vecInfo.inRegion(*userInst.getParent()))
      continue;

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

  // Compute the result of the last iteration
  // * With AVL: multiply with remainder AVL at runtime.
  // * w/o AVL: multiplu with vectorWidth at compile time.
  auto RepairFunc = [&](Value &usedVal, BasicBlock &userBlock) -> Value & {
    auto *insertPt = userBlock.getFirstNonPHI();
    IRBuilder<> builder(&userBlock, insertPt->getIterator());

    // otw, replace with reduced value
    Value *Amount;
    if (vecInfo.getEntryAVL()) {
      // (avl - 1) * [[STRIDE]]
      auto *VecAVL = requestScalarValue(vecInfo.getEntryAVL());
      auto *AdjustedVecAVL = builder.CreateSub(
          VecAVL, ConstantInt::get(VecAVL->getType(), 1, false));
      auto *CastVecAVL =
          builder.CreateZExtOrTrunc(AdjustedVecAVL, usedVal.getType());
      Amount = builder.CreateMul(
          CastVecAVL, ConstantInt::get(CastVecAVL->getType(),
                                       (int64_t)redShape.getStride(), true));

    } else {
      // (vector_width - 1) * [[STRIDE]]
      int64_t amount = (vectorWidth - 1) * redShape.getStride();
      Amount = ConstantInt::getSigned(usedVal.getType(), amount);
    }

    auto *liveOutView = builder.CreateAdd(&usedVal, Amount, ".red");
    return *liveOutView;
  };

  repairOutsideUses(*sp.phi, RepairFunc);
  repairOutsideUses(*sp.reductor, RepairFunc);
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
        PHINode *phi = 
            (!shape.isVarying() || replicate) ? getScalarValueAs<PHINode>(*scalPhi, lane) : getVectorValueAs<PHINode>(*scalPhi);
        for (unsigned i = 0; i < scalPhi->getNumIncomingValues(); ++i) {
          // set insertion point to before Terminator of incoming block
          BasicBlock *incVecBlock = getVectorBlock(*scalPhi->getIncomingBlock(i), true);
          builder.SetInsertPoint(incVecBlock->getTerminator());

          Value *val = (!shape.isVarying() || replicate) ? requestScalarValue(scalPhi->getIncomingValue(i), lane)
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
  } else {
    vectorValueMap[value] = vecValue;
  }
}

Value *NatBuilder::getVectorValue(Value &ScaValue, bool getLastBlock) const {
  if (isa<BasicBlock>(ScaValue)) {
    return getVectorBlock(cast<BasicBlock>(ScaValue), getLastBlock);
  }

  auto vecIt = vectorValueMap.find(&ScaValue);
  if (vecIt != vectorValueMap.end())
    return vecIt->second;
  else
    return nullptr;
}

BasicBlock *NatBuilder::getVectorBlock(BasicBlock &ScaBlock,
                                       bool ReturnLastBlock) const {
  if (!vecInfo.inRegion(ScaBlock)) {
    return &ScaBlock; // preserve BBs outside of the region
  }

  auto blockIt = basicBlockMap.find(&ScaBlock);
  if (blockIt != basicBlockMap.end()) {
    const BasicBlockVector &blocks = blockIt->second;
    return ReturnLastBlock ? blocks.back() : blocks.front();
  }
  return nullptr;
}

void NatBuilder::mapScalarValue(const Value *const value, Value *mapValue, unsigned laneIdx) {
  LaneValueVector &laneValues = scalarValueMap[value];
  if (laneValues.size() < laneIdx) laneValues.resize(laneIdx);
  laneValues.insert(laneValues.begin() + laneIdx, mapValue);
}

Value *NatBuilder::getScalarValue(Value & ScaValue, unsigned laneIdx) const {
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

    const LaneValueVector &laneValues = scalarIt->second;
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
