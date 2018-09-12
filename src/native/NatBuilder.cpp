//===- NatBuilder.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#include <deque>

#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Metadata.h>
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

namespace rv {

unsigned numMaskedGather, numMaskedScatter, numGather, numScatter, numPseudoMaskedLoads, numPseudoMaskedStores,
    numInterMaskedLoads, numInterMaskedStores, numPseudoLoads, numPseudoStores, numInterLoads, numInterStores,
    numContMaskedLoads, numContMaskedStores, numContLoads, numContStores, numUniMaskedLoads, numUniMaskedStores,
    numUniLoads, numUniStores;
unsigned numVecGEPs, numScalGEPs, numInterGEPs, numVecBCs, numScalBCs;
unsigned numVecCalls, numSemiCalls, numFallCalls, numCascadeCalls, numRVIntrinsics;
unsigned numScalarized, numVectorized, numFallbacked, numLazy;

bool DumpStatistics(std::string &file) {
  char * envVal = getenv("NAT_STAT_DUMP");
  if (!envVal) return false;
  else return !(file = envVal).empty();
}

void NatBuilder::printStatistics() {
  // memory statistics
  Report() << "nat memory:\n"
           << "\tscatter/gather: " << numScatter << "/" << numGather << ", masked " << numMaskedScatter << "/" << numMaskedGather << "\n"
           << "\tpsi loads/stores: " << numPseudoLoads << "/" << numPseudoStores << ". masked " << numPseudoMaskedLoads << "/" << numPseudoMaskedStores << "\n"
           << "\tinter load/store: " << numInterLoads << "/" << numInterStores << ", masked " << numInterMaskedLoads << "/" << numInterMaskedStores << "\n"
           << "\tcons load/store: " << numContLoads << "/" << numContStores << ", masked " <<  numContMaskedLoads << "/" << numContMaskedStores << "\n"
           << "\tuni load/store: " << numUniLoads << "/" << numUniStores << ", masked " << numUniMaskedLoads << "/" << numUniMaskedStores << "\n";

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
  file << "pseudointer-masked-load," << numPseudoMaskedLoads << "\n";
  file << "pseudointer-masked-store," << numPseudoMaskedStores << "\n";
  file << "pseudointer-load," << numPseudoLoads << "\n";
  file << "pseudointer-store," << numPseudoStores << "\n";
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
                       const DominatorTree &_dominatorTree, MemoryDependenceResults &_memDepRes,
                       ScalarEvolution &_SE, ReductionAnalysis & _reda) :
    builder(_vecInfo.getMapping().vectorFn->getContext()),
    config(_config),
    platInfo(_platInfo),
    vecInfo(_vecInfo),
    dominatorTree(_dominatorTree),
    memDepRes(_memDepRes),
    SE(_SE),
    reda(_reda),
    undeadMasks(dominatorTree, vecInfo),
    layout(_vecInfo.getScalarFunction().getParent()),
    i1Ty(IntegerType::get(_vecInfo.getMapping().vectorFn->getContext(), 1)),
    i32Ty(IntegerType::get(_vecInfo.getMapping().vectorFn->getContext(), 32)),
    region(_vecInfo.getRegion()),
    keepScalar(),
    cascadeLoadMap(),
    cascadeStoreMap(),
    vectorValueMap(),
    scalarValueMap(),
    basicBlockMap(),
    grouperMap(),
    pseudointerValueMap(),
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
  if (!region->isVectorLoop()) { // TODO wfv mode check
    unsigned i = 0;
    auto sit = func->arg_begin();
    for (auto it = vecFunc->arg_begin(), et = vecFunc->arg_end();
         it != et; ++it, ++sit, ++i) {
      Argument *arg = &*it;
      const Argument *sarg = &*sit;
      arg->setName(sarg->getName());
      VectorShape argShape = vecInfo.getMapping().argShapes[i];
      if (argShape.isVarying() && !arg->getType()->isPointerTy())
        mapVectorValue(sarg, arg);
      else
        mapScalarValue(sarg, arg);
    }
  }

  // visit all memory instructions and check if we can scalarize their index calculation
  if (config.scalarizeIndexComputation)
    visitMemInstructions();

  // create all BasicBlocks first and map them
  for (auto &block : *func) {
    if (!region->contains(&block)) continue;

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
    if (!region->contains(bb)) continue;

    BasicBlock *vecBlock = cast<BasicBlock>(getVectorValue(bb));
    vectorize(bb, vecBlock);

    // populate queue with pre-order dominators
    for (auto it = node->getChildren().begin(), et = node->getChildren().end(); it != et; ++it) {
      nodeQueue.push_back(*it);
    }
  }

  // revisit PHINodes now and add the mapped incoming values
  if (!phiVector.empty()) addValuesToPHINodes();

  // report statistics
  printStatistics();

  if (!region->isVectorLoop()) return;

  // TODO what about outside uses?

  // register vector insts
  if (vecInstMap) {
    for (auto & BB : *vecFunc) {
      if (region->contains(&BB)) {
        (*vecInstMap)[&BB] = getVectorValue(&BB);
      }
      for (auto & I : BB) {
        auto * vecInst = getVectorValue(&I);
        if (vecInst) {
          (*vecInstMap)[&I] = vecInst;
        } else {
          (*vecInstMap)[&I] = getScalarValue(&I, 0);
        }
      }
    }
  }

  if (!embedRegion) return;

  // rewire branches outside the region to go to the region instead
  std::vector<BasicBlock *> oldBlocks;
  for (auto &BB : *vecFunc) {
    if (region->contains(&BB)) {
      oldBlocks.push_back(&BB);
      continue; // keep old region
    }
    auto &termInst = *BB.getTerminator();
    for (unsigned i = 0; i < termInst.getNumOperands(); ++i) {
      auto *termOp = termInst.getOperand(i);
      auto *branchTarget = dyn_cast<BasicBlock>(termOp);
      if (!branchTarget) continue;
      if (region->contains(branchTarget)) {
        termInst.setOperand(i, getVectorValue(branchTarget));
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
      Dump(*getVectorValue(oldBB));
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

    // loads and stores need special treatment (masking, shuffling, etc) (build them lazily)
    if (canVectorize(inst) && (load || store))
      if (config.enableInterleaved) addLazyInstruction(inst);
      else vectorizeMemoryInstruction(inst);
    else if (call) {
      // calls need special treatment
      switch (GetIntrinsicID(*call)) {
        case RVIntrinsic::Any: vectorizeReductionCall(call, false); break;
        case RVIntrinsic::All: vectorizeReductionCall(call, true); break;
        case RVIntrinsic::Extract: vectorizeExtractCall(call); break;
        case RVIntrinsic::Insert: vectorizeInsertCall(call); break;
        case RVIntrinsic::VecLoad: vectorizeLoadCall(call); break;
        case RVIntrinsic::VecStore: vectorizeStoreCall(call); break;
        case RVIntrinsic::Shuffle: vectorizeShuffleCall(call); break;
        case RVIntrinsic::Ballot: vectorizeBallotCall(call); break;
        case RVIntrinsic::PopCount: vectorizePopCountCall(call); break;
        case RVIntrinsic::Index: vectorizeIndexCall(*call); break;
        case RVIntrinsic::Align: vectorizeAlignCall(call); break;
        default: {
          if (config.enableInterleaved) addLazyInstruction(inst);
          else {
            if (shouldVectorize(call)) vectorizeCallInstruction(call);
            else copyCallInstruction(call);
          }
        }
      }
    } else if (phi)
      // phis need special treatment as they might contain not-yet mapped instructions
      vectorizePHIInstruction(phi);
    else if (alloca && shouldVectorize(inst)) {
      // note: this is ONLY allowed IFF
      // (1) no calls that have alloca instructions as arguments OR
      // (2) there exists a function mapping which allows that. e.g.: float * -> <4 x float> *
      // TODO: fix alloca mapping/vectorization
//      if (canVectorize(inst))
//        vectorizeAllocaInstruction(alloca);
//      else {
//        for (unsigned lane = 0; lane < vectorWidth(); ++lane) {
//          copyInstruction(inst, lane);
//        }
//    }
      fallbackVectorize(inst);
    } else if (gep || bc) {
      continue; // skipped
    } else if (canVectorize(inst) && shouldVectorize(inst))
      vectorizeInstruction(inst);
    else if (!canVectorize(inst) && shouldVectorize(inst))
      fallbackVectorize(inst);
    else
      copyInstruction(inst);
  }
}

ValVec
NatBuilder::scalarize(BasicBlock & scaBlock, Instruction & inst, bool packResult, std::function<Value*(IRBuilder<>&,size_t)> genFunc) {
  auto * vecTy = packResult ? VectorType::get(inst.getType(), vectorWidth()) : nullptr;
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
   BasicBlock *vecBlock = cast<BasicBlock>(getVectorValue(&srcBlock));
   BasicBlock *resBlock = createCascadeBlocks(vecBlock->getParent(), vectorWidth(), condBlocks, maskedBlocks);
   condBlocks.push_back(resBlock);

   // branch to our entry block of the cascade
   builder.CreateBr(condBlocks[0]);
   builder.SetInsertPoint(condBlocks[0]);

   // vector aggregate if packing was requested
   auto * vecTy = packResult ? VectorType::get(inst.getType(), vectorWidth()) : nullptr;
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

// TODO don't modify vecCall.. build a new CallInst and build a new parameter vector
CallInst*
NatBuilder::vectorizeCallWithFunction(CallInst & scaCall, Function & vecFunc, int maskPos) {
  assert(maskPos < 0 && "TODO implement predicated calls");
  int scaIdx = 0; // TODO use for mask skipping
  auto itVecArg = vecFunc.arg_begin();

  std::vector<Value*> vectorArgs;
  for (int vecIdx = 0; vecIdx < (int) vecFunc.arg_size(); ++vecIdx, ++scaIdx, ++itVecArg) {
    Value *op = scaCall.getArgOperand(scaIdx);

    bool vecTypeArg = itVecArg->getType()->isVectorTy();
    Value *mappedArg = vecTypeArg ? requestVectorValue(op) : requestScalarValue(op);
    vectorArgs.push_back(mappedArg);
  }
  auto * vecCall = builder.CreateCall(&vecFunc, vectorArgs);
  return vecCall;
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

void NatBuilder::vectorizeAllocaInstruction(AllocaInst *const alloca) {
  Type *allocaType = alloca->getType()->getElementType();

  // if aggregate or vector type: create <vector_width> types. if not: create vector_type
  Type *type;
  Value *numElements = nullptr;
//  bool mapVector = false;
  if (allocaType->isAggregateType() || allocaType->isVectorTy()) {
    type = allocaType;
    numElements = ConstantInt::get(i32Ty, vectorWidth());
  } else {
    type = getVectorType(allocaType, vectorWidth());
//    mapVector = true;
  }

  AllocaInst *vecAlloca = builder.CreateAlloca(type, numElements, alloca->getName());
//  if (mapVector)
    mapVectorValue(alloca, vecAlloca);
//  else
//    mapScalarValue(alloca, vecAlloca);
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
      BasicBlock *succ = cast<BasicBlock>(requestVectorValue(branch->getSuccessor(i)));
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

void NatBuilder::fallbackVectorize(Instruction *const inst) {
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
  assert(rvCall->getNumArgOperands() == 1 && "expected only 1 argument for rv_any");

  Value *predicate = rvCall->getArgOperand(0);
  const VectorShape &shape = getVectorShape(*predicate);
  assert((shape.isVarying() || shape.isUniform()) && "predicate can't be contigious or strided");

  Value *reduction;
  if (shape.isVarying()) {
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
  } else {
    reduction = requestScalarValue(predicate);
  }

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
  if (getVectorShape(*vecPtr).isUniform()) {
    auto * uniVal = requestScalarValue(vecPtr);
    auto addressSpace = uniVal->getType()->getPointerAddressSpace();
    auto * castPtr = builder.CreatePointerCast(uniVal, PointerType::get(builder.getFloatTy(), addressSpace));
    auto * gepPtr = builder.CreateGEP(castPtr, laneId);
    auto * loadVal = builder.CreateLoad(gepPtr);
    mapScalarValue(rvCall, loadVal);
    return;
  }

// non-uniform arg
  auto * vecVal = requestVectorValue(vecPtr);
  auto * lanePtr = builder.CreateExtractElement(vecVal, laneId, "rv_load");
  auto * laneVal = builder.CreateLoad(lanePtr, "rv_load");
  mapScalarValue(rvCall, laneVal);
}

void
NatBuilder::vectorizeStoreCall(CallInst *rvCall) {
  ++numRVIntrinsics;

  assert(rvCall->getNumArgOperands() == 3 && "expected 3 arguments for rv_store(vecPtr, laneId, value)");

  Value *vecPtr  = rvCall->getArgOperand(0);
  assert(getVectorShape(*rvCall->getArgOperand(2)).isUniform());
  Value *elemVal = requestScalarValue(rvCall->getArgOperand(2));
  assert(getVectorShape(*rvCall->getArgOperand(1)).isUniform());
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
  assert(getVectorShape(*rvCall->getArgOperand(1)).isUniform());
  int64_t shiftVal = cast<ConstantInt>(rvCall->getArgOperand(1))->getSExtValue();
  if (shiftVal < 0) {
    shiftVal = vectorWidth() + shiftVal;
  }

  // build shuffle indices
  SmallVector<uint32_t, 32> shflIds(vectorWidth());
  for (int i = 0; i < vectorWidth(); i++) {
    shflIds[i] = (i + shiftVal) % vectorWidth();
  }

  auto * shflVal = builder.CreateShuffleVector(vecVal, vecVal, shflIds, "rv_shfl");
  mapVectorValue(rvCall, shflVal);
}

Value*
NatBuilder::createVectorMaskSummary(Value * vecVal, IRBuilder<> & builder, RVIntrinsic mode) {
  Module *mod = vecInfo.getMapping().vectorFn->getParent();

  auto vecWidth = cast<VectorType>(vecVal->getType())->getVectorNumElements();
  auto * intVecTy = VectorType::get(i32Ty, vecWidth);

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
        auto * lowerBallot = createVectorMaskSummary(lowerHalf, builder, mode);

        auto * upperHalf = builder.CreateShuffleVector(vecVal, UndefValue::get(vecVal->getType()), ConstantVector::get(higherLanes), "higherLanes");
        auto * upperBallot = createVectorMaskSummary(upperHalf, builder, mode);

        // ballot(vecVal) =  upperHalf << (halfWidth) | lowerHalf
        auto * up = builder.CreateShl(upperBallot, ConstantInt::get(i32Ty, halfWidth, false));
        return builder.CreateOr(up, lowerBallot);
      }

      // AVX-specific code path
      if (config.useSSE || config.useAVX || config.useAVX2 || config.useAVX512) {
      // non-uniform arg
        uint32_t bits = 32;
        Intrinsic::ID id;
        switch (vecWidth) {
        case 2: id = Intrinsic::x86_sse2_movmsk_pd; bits = 64; break;
        case 4: id = Intrinsic::x86_sse_movmsk_ps; break;
        case 8: id = Intrinsic::x86_avx_movmsk_ps_256; break;
        default: abort();
          fail("Unsupported vector width in ballot !");
        }

        auto * extVal = builder.CreateSExt(vecVal, VectorType::get(builder.getIntNTy(bits), vecWidth), "rv_ballot");
        auto * simdVal = builder.CreateBitCast(extVal, VectorType::get(bits == 32 ? builder.getFloatTy() : builder.getDoubleTy(), vecWidth), "rv_ballot");

        auto movMaskDecl = Intrinsic::getDeclaration(mod, id);
        result = builder.CreateCall(movMaskDecl, simdVal, "rv_ballot");

      } else {
        // generic emulating code path
        //
      // a[lane] == 1 << lane
        std::vector<Constant*> constants(vecWidth, nullptr);
        for (unsigned i = 0; i < vecWidth; ++i) {
          unsigned int val = 1 << i;
          Constant *constant = ConstantInt::get(i32Ty, val);
          constants[i] = constant;
        }
        auto * flagVec = ConstantVector::get(constants);
        auto * zeroVec = ConstantVector::getNullValue(intVecTy);

      // select (vecVal[l] ? a[l] : 0)
        auto * maskedLaneVec = builder.CreateSelect(vecVal, flagVec, zeroVec);

      // reduce_or
        result = &CreateVectorReduce(builder, RedKind::Or, *maskedLaneVec);
      }
    } break;

    case RVIntrinsic::PopCount: {
      auto * maskedOnes = builder.CreateZExt(vecVal, intVecTy, "rv_ballot");
      result = &CreateVectorReduce(builder, RedKind::Add, *maskedOnes);
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

// uniform arg
  if (getVectorShape(*condArg).isUniform()) {
    auto * uniVal = requestScalarValue(condArg);
    uniVal = builder.CreateSExt(uniVal, i32Ty, "rv_ballot");
    uniVal = builder.CreateAnd(uniVal, builder.getInt32((1 << vecWidth) - 1), "rv_ballot");
    mapScalarValue(rvCall, uniVal);
    return;
  }

// non-uniform arg
  auto * vecVal = maskInactiveLanes(requestVectorValue(condArg), rvCall->getParent(), false);
  auto * mask = createVectorMaskSummary(vecVal, builder, RVIntrinsic::Ballot);
  mapScalarValue(rvCall, mask);
}

void
NatBuilder::vectorizeIndexCall(CallInst & rvCall) {
  ++numRVIntrinsics;

// avx512vl - expand based implementation
  if (config.useAVX512) {
    auto vecWidth = vecInfo.getVectorWidth();
    assert(vecWidth == 4 || vecWidth == 8);

    Intrinsic::ID id = vecWidth == 8 ? Intrinsic::x86_avx512_mask_expand_pd_512 : Intrinsic::x86_avx512_mask_expand_ps_512;

    assert(rvCall.getNumArgOperands() == 1 && "expected 1 argument for rv_index(mask)");

    Value *condArg = rvCall.getArgOperand(0);

    auto * intLaneTy = IntegerType::getIntNTy(rvCall.getContext(), 512 / vecWidth);
    bool argUniform = hasUniformPredicate(*rvCall.getParent()) && vecInfo.getVectorShape(*condArg).isUniform();

//   uniform arg
    if (argUniform) {
      mapScalarValue(&rvCall, createContiguousVector(vecWidth, intLaneTy, 0, 1));
      return;
    }

    auto * maskVec = maskInactiveLanes(requestVectorValue(condArg), rvCall.getParent(), false);
    auto * contVec = createContiguousVector(vecWidth, intLaneTy, 0, 1);


    auto * fpLaneTy = Type::getDoubleTy(rvCall.getContext());
    auto * fpVecTy = VectorType::get(fpLaneTy, vecWidth);
    auto * intVecTy = VectorType::get(intLaneTy, vecWidth);

    auto * fpValVec = builder.CreateBitCast(contVec, fpVecTy);

    auto * expandDecl = Intrinsic::getDeclaration(rvCall.getParent()->getParent()->getParent(), id, {});

//   flatten mask (<W x i1> --> <iW>)
    auto * flatMaskTy = Type::getIntNTy(rvCall.getContext(), vecWidth);
    auto * flatMask = builder.CreateBitCast(maskVec, flatMaskTy, "flatmask");

//   call expand
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

  assert(rvCall->getNumArgOperands() == 1 && "expected 1 argument for rv_ballot(cond)");

  Value *condArg = rvCall->getArgOperand(0);
  auto vecWidth = vecInfo.getVectorWidth();

// uniform arg
  if (getVectorShape(*condArg).isUniform()) {
    auto * uniVal = requestScalarValue(condArg);
    uniVal = builder.CreateSExt(uniVal, i32Ty, "rv_popcount");
    uniVal = builder.CreateAnd(uniVal, ConstantInt::get(i32Ty, vecWidth, false));
    mapScalarValue(rvCall, uniVal);
    return;
  }

  // FIXME mask out inactive threads also for uniform mask
  auto * vecVal = maskInactiveLanes(requestVectorValue(condArg), rvCall->getParent(), false);
  auto * mask = createVectorMaskSummary(vecVal, builder, RVIntrinsic::PopCount);
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
NatBuilder::vectorizeCallInstruction(CallInst *const scalCall) {
  Value * callee = scalCall->getCalledValue();
  StringRef calleeName = callee->getName();
  Function * calledFunction = dyn_cast<Function>(callee);

  VectorShapeVec callArgShapes;
  for (int i = 0; i < (int) scalCall->getNumArgOperands(); ++i) {
    auto argShape = vecInfo.getVectorShape(*scalCall->getArgOperand(i));
    callArgShapes.push_back(argShape);
  }

  // look for (proper) mappings with arg shapes
  if (calledFunction) {
    bool needsPredication = false; // FIXME query block predicate
    VecMappingShortVec matchVec;
    platInfo.getMappingsForCall(matchVec, *calledFunction, callArgShapes, vecInfo.getVectorWidth(), needsPredication);

    if (!matchVec.empty()) {
      VectorMapping mapping = matchVec[0];
      assert((mapping.maskPos < 0) && "TODO implemented predicated mapped calls.");

      const int maskPos = -1; // TODO support predicated functions
      auto * vecCall = vectorizeCallWithFunction(*scalCall, *mapping.vectorFn, maskPos);
      vecCall->setName(scalCall->getName() + ".mapped");
      mapVectorValue(scalCall, vecCall);
      ++numVecCalls;
      return;
    }
  }


  // TODO re-factor the remainder of this function
  // if calledFunction is vectorizable (standard mapping exists for given vector width), create new call to vector calledFunction
  std::unique_ptr<FunctionResolver> funcResolver = nullptr;
  if (calledFunction) funcResolver = platInfo.getResolver(calledFunction->getName(), *calledFunction->getFunctionType(), callArgShapes, vectorWidth());
  if (funcResolver) {
    Function &simdFunc = funcResolver->requestVectorized();
    const int maskPos = -1;
    auto * vecCall = vectorizeCallWithFunction(*scalCall, simdFunc, maskPos);
    vecCall->setName(scalCall->getName() + ".mapped");
    mapVectorValue(scalCall, vecCall);
    ++numVecCalls;

  } else {
    // try if we can semi-vectorize the call by replication a smaller vectorized version
    unsigned vecWidth = vectorWidth() / 2;
    std::unique_ptr<FunctionResolver> funcResolver = nullptr;
    for (; calledFunction && vecWidth >= 2; vecWidth /= 2) {
      // FIXME update alignment in callArgShapes
      funcResolver = platInfo.getResolver(calleeName, *calledFunction->getFunctionType(), callArgShapes, vecWidth);
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
  auto *callee = scalCall->getCalledValue();

  std::vector<Value *> args;
  for (unsigned i = 0; i < scalCall->getNumArgOperands(); ++i) {
    Value *scalArg = scalCall->getArgOperand(i);
    Value *laneArg = requestScalarValue(scalArg, laneIdx);
    args.push_back(laneArg);
  }

  Value *call = builder.CreateCall(callee, args, scalCall->getName());
  mapScalarValue(scalCall, call, laneIdx);

  ++numScalarized;
}

void NatBuilder::vectorizeMemoryInstruction(Instruction *const inst) {
  if (keepScalar.count(inst)) {
    return fallbackVectorize(inst);
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

  Value *mask = nullptr;
  Value *predicate = vecInfo.getPredicate(*inst->getParent());
  assert(predicate && predicate->getType()->isIntegerTy(1) && "predicate must have i1 type!");
  bool needsMask = predicate && !vecInfo.getVectorShape(*predicate).isUniform();

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
  std::vector<Value *> srcs;
  std::vector<Value *> masks;
  unsigned alignment;
  bool interleaved = false;
  bool pseudoInter = false;
  int byteSize = static_cast<int>(layout.getTypeStoreSize(accessedType));

  if (addrShape.isUniform()) {
    // scalar access
    addr.push_back(requestScalarValue(accessedPtr));
    alignment = addrShape.getAlignmentFirst();

  } else if ((addrShape.isContiguous() || addrShape.isStrided(byteSize)) && !(needsMask && !config.enableMaskedMove)) {
    // cast pointer to vector-width pointer
    Value *ptr = requestScalarValue(accessedPtr);
    PointerType *vecPtrType = PointerType::getUnqual(vecType);
    addr.push_back(builder.CreatePointerCast(ptr, vecPtrType, "vec_cast"));
    alignment = addrShape.getAlignmentFirst();

  } else if ((addrShape.isStrided() && isInterleaved(inst, accessedPtr, byteSize, srcs)) && !(needsMask && !config.enableMaskedMove)) {
    // interleaved access. ptrs: base, base+vector, base+2vector, ...
    Value *srcPtr = getPointerOperand(cast<Instruction>(srcs[0]));
    for (unsigned i = 0; i < srcs.size(); ++i) {
      Value *ptr = requestInterleavedAddress(srcPtr, i, vecType);
      addr.push_back(ptr);
      if (needsMask)
        masks.push_back(mask);
    }
    alignment = addrShape.getAlignmentFirst();
    interleaved = true;

  } else if ((addrShape.isStrided() && isPseudointerleaved(inst, accessedPtr, byteSize)) && !(needsMask && !config.enableMaskedMove)) {
    // pseudo-interleaved: same as above. we don't know the array limits, so we skip the last index of the last load
    unsigned stride = (unsigned) addrShape.getStride() / byteSize;
    srcs.push_back(inst);
    Value *srcPtr = getPointerOperand(inst);
    Type *interType = vecType;
    for (unsigned i = 0; i < stride; ++i) {
      if (config.cropPseudoInterleaved && i == (stride - 1)) {
        interType = getVectorType(accessedType, vectorWidth() - (stride-1));
      }
      Value *ptr = requestInterleavedAddress(srcPtr, i, interType);
      addr.push_back(ptr);
    }

    if (store && needsMask) {
      masks.push_back(mask);
      for (unsigned i = 1; i < stride; ++i) {
        unsigned width = i == (stride - 1) && config.cropPseudoInterleaved ? vectorWidth() - (stride - 1) : vectorWidth();
        masks.push_back(getConstantVector(width, i1Ty, 0));
      }
    }

    alignment = addrShape.getAlignmentFirst();
    pseudoInter = true;

  } else {
    addr.push_back(requestVectorValue(accessedPtr));
    alignment = addrShape.getAlignmentGeneral();
  }

  unsigned origAlignment = load ? load->getAlignment() : store->getAlignment();
  alignment = std::max<unsigned>(alignment, origAlignment);

  Value *vecMem = nullptr;
  if (load) {
    if (needsMask && addrShape.isUniform()) {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      vecMem = createUniformMaskedMemory(load, accessedType, alignment, addr[0], predicate, mask, nullptr);

    } else if (((addrShape.isUniform() || addrShape.isContiguous() || addrShape.isStrided(byteSize))) && !(needsMask && !config.enableMaskedMove)) {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      vecMem = createContiguousLoad(addr[0], alignment, needsMask ? mask : nullptr, UndefValue::get(vecType));

      addrShape.isUniform() ? ++numUniLoads : needsMask ? ++numContMaskedLoads : ++numContLoads;

    } else if (interleaved && !(needsMask && !config.enableMaskedMove)) {
      assert(addr.size() > 1 && "only one address for multiple accesses!");
      createInterleavedMemory(vecType, alignment, &addr, &masks, nullptr, &srcs);

    } else if (pseudoInter && !(needsMask && !config.enableMaskedMove)) {
      assert(addr.size() > 1 && "only one address for multiple accesses!");
      createInterleavedMemory(vecType, alignment, &addr, &masks, nullptr, &srcs, true);

    } else {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      vecMem = createVaryingMemory(vecType, alignment, addr[0], mask, nullptr);
    }


  } else {
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
      Value *mappedStoredVal = addrShape.isUniform() ? requestScalarValue(storedValue)
                                                       : requestVectorValue(storedValue);
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
      createInterleavedMemory(vecType, alignment, &addr, &masks, &vals, &srcs);

    } else if (pseudoInter && !(needsMask && !config.enableMaskedMove)) {
      assert(addr.size() > 1 && "only one address for multiple accesses!");
      Value *mappedStoredVal = addrShape.isUniform() ? requestScalarValue(storedValue)
                                                       : requestVectorValue(storedValue);
      std::vector<Value *> vals;
      vals.reserve(addr.size());
      vals.push_back(mappedStoredVal);
      createInterleavedMemory(vecType, alignment, &addr, &masks, &vals, &srcs, true);

    } else {
      assert(addr.size() == 1 && "multiple addresses for single access!");
      Value *mappedStoredVal = addrShape.isUniform() ? requestScalarValue(storedValue)
                                                       : requestVectorValue(storedValue);
      vecMem = createVaryingMemory(vecType, alignment, addr[0], mask, mappedStoredVal);
    }
  }


  // interleaved case creates mapping
  if (!interleaved && !pseudoInter) {
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
NatBuilder::createVaryingToUniformStore(Instruction *inst, Type *accessedType, unsigned int alignment, Value *addr, Value *mask, Value *values) {
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
    auto * vecLaneTy = VectorType::get(nativeIntTy, vectorWidth());
    auto * sxMask = builder.CreateSExt(mask, vecLaneTy);

    // AND with lane index vector
    auto * laneIdxConst = createContiguousVector(vectorWidth(), nativeIntTy, 0, 1);
    auto * activeLaneVec = builder.CreateAnd(sxMask, laneIdxConst);

    // horizontal MAX reduction
    indexVal = &CreateVectorReduce(builder, RedKind::Max, *activeLaneVec);
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
  cast<StoreInst>(vecMem)->setAlignment(alignment);

  // proceed in continue block (if any)
  if (continueBlock) {
    builder.CreateBr(continueBlock);
    builder.SetInsertPoint(continueBlock);
  }

  return vecMem;
}

Value *NatBuilder::createUniformMaskedMemory(Instruction *inst, Type *accessedType, unsigned int alignment,
                                             Value *addr, Value * scalarMask, Value * vectorMask, Value *values) {
  values ? ++numUniMaskedStores : ++numUniMaskedLoads;

  BasicBlock *origBlock = inst->getParent();
  bool needsGuard = !undeadMasks.isUndead(*scalarMask, *origBlock);

  BasicBlock* memBlock, * continueBlock;

  // prologue (guard branch)
  if (needsGuard) {
    // create a mask ptest
    Value * anyMask = createPTest(vectorMask, false);

    assert((values && isa<StoreInst>(inst)) || (!values && isa<LoadInst>(inst)));

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
  Instruction *vecMem;
  if (values) {
    vecMem = builder.CreateStore(values, addr);
    cast<StoreInst>(vecMem)->setAlignment(alignment);
  } else {
    vecMem = builder.CreateLoad(addr, "scal_mask_mem");
    cast<LoadInst>(vecMem)->setAlignment(alignment);
  }

  // epilogue (continue block, joining loaded value)
  if (needsGuard) {
    builder.CreateBr(continueBlock);
    builder.SetInsertPoint(continueBlock);

    BasicBlock *vecOrigBlock = cast<BasicBlock>(getVectorValue(origBlock, true));
    PHINode *phi = values ? nullptr : builder.CreatePHI(accessedType, 2, "scal_mask_mem_phi");

    if (phi) {
      phi->addIncoming(vecMem, memBlock);
      phi->addIncoming(UndefValue::get(accessedType), vecOrigBlock);
      vecMem = phi;
    }

    mapVectorValue(origBlock, continueBlock);
  }

  return vecMem;
}

Value *NatBuilder::createVaryingMemory(Type *vecType, unsigned int alignment, Value *addr, Value *mask,
                                       Value *values) {
  bool scatter(values != nullptr);
  bool maskNonConst(!isa<ConstantVector>(mask));
  maskNonConst ? (scatter ? ++numMaskedScatter : ++numMaskedGather) : (scatter ? ++numScatter : ++numGather);

  if (config.useScatterGatherIntrinsics) {

    auto * vecPtrTy = addr->getType();

    std::vector<Value *> args;
    if (scatter) args.push_back(values);
    args.push_back(addr);
    args.push_back(ConstantInt::get(i32Ty, alignment));
    args.push_back(mask);
    if (!scatter) args.push_back(UndefValue::get(vecType));
    Module *mod = vecInfo.getMapping().vectorFn->getParent();
    Function *intr = scatter ? Intrinsic::getDeclaration(mod, Intrinsic::masked_scatter, {vecType, vecPtrTy})
                             : Intrinsic::getDeclaration(mod, Intrinsic::masked_gather, {vecType, vecPtrTy});
    assert(intr && "scatter/gather not found!");
    return builder.CreateCall(intr, args);

  } else
    return scatter ? requestCascadeStore(values, addr, alignment, mask) : requestCascadeLoad(addr, alignment, mask);
}

void NatBuilder::createInterleavedMemory(Type *vecType, unsigned alignment, std::vector<Value *> *addr, std::vector<Value *> *masks,
                                         std::vector<Value *> *values, std::vector<Value *> *srcs, bool isPseudoInter) {

  unsigned stride = (unsigned) addr->size();
  bool needsMask = masks->size() > 0;
  bool load = values == nullptr;

  if (load) {
    if (needsMask)
      isPseudoInter ? ++numPseudoMaskedLoads : numInterMaskedLoads += stride;
    else
      isPseudoInter ? ++numPseudoLoads : numInterLoads += stride;
  } else {
    if (needsMask)
      isPseudoInter ? ++numPseudoMaskedStores : numInterMaskedStores += stride;
    else
      isPseudoInter ? ++numPseudoStores : numInterStores += stride;
  }

  // search if address was already pseudo interleaved by a load before
  if (isPseudoInter && !load) {
    Value *srcAddr = cast<StoreInst>(srcs->front())->getPointerOperand();
    if (pseudointerValueMap.count(srcAddr)) {
      LaneValueVector &pseudoValues = pseudointerValueMap[srcAddr];
      for (unsigned i = 1; i < pseudoValues.size(); ++i) {
        values->push_back(pseudoValues[i]);
      }

      if (!needsMask) {
        unsigned width = vectorWidth() - (stride - 1);
        std::vector<unsigned> trueMask(width, 1);
        masks->push_back(getConstantVectorPadded(vectorWidth(), i1Ty, trueMask, true));
      }

    } else {
      // fill with undefined else
      Type *interType = vecType;
      if (!needsMask)
        masks->push_back(getConstantVector(vectorWidth(), i1Ty, 1));
      unsigned width = vectorWidth();
      for (unsigned i = 1; i < addr->size(); ++i) {
        if (i == (stride - 1) && config.cropPseudoInterleaved) {
          width = vectorWidth() - (stride - 1);
          interType = getVectorType(vecType->getVectorElementType(), width);
        }

        values->push_back(UndefValue::get(interType));
        masks->push_back(getConstantVector(width, i1Ty, 0));
      }
      needsMask = true;
    }
  } else if (isPseudoInter && load && !config.cropPseudoInterleaved) {
    unsigned width = vectorWidth() - (stride - 1);
    std::vector<unsigned> trueMask(width, 1);
    masks->push_back(getConstantVectorPadded(vectorWidth(), i1Ty, trueMask, true));
  }

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
      vecMem = createContiguousLoad(ptr, alignment, mask, UndefValue::get(vecType));
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
    Value *srcAddr = cast<LoadInst>(srcs->front())->getPointerOperand();
    for (unsigned i = 0; i < stride; ++i) {
      vecMem = transposer.shuffleFromInterleaved(builder, stride, i);
      if (i < srcs->size())
        mapVectorValue((*srcs)[i], vecMem);

      pseudointerValueMap[srcAddr].push_back(vecMem);
    }
  }
}

Value *NatBuilder::createContiguousStore(Value *val, Value *ptr, unsigned alignment, Value *mask) {
  if (mask) {
    return builder.CreateMaskedStore(val, ptr, alignment, mask);

  } else {
    StoreInst *store = builder.CreateStore(val, ptr);
    store->setAlignment(alignment);
    return store;
  }
}

Value *NatBuilder::createContiguousLoad(Value *ptr, unsigned alignment, Value *mask, Value *passThru) {
  if (mask) {
    return builder.CreateMaskedLoad(ptr, alignment, mask, passThru, "cont_load");

  } else {
    LoadInst *load = builder.CreateLoad(ptr, "cont_load");
    load->setAlignment(alignment);
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
    if (getVectorValue(lazyInstr)) {
      lazyInstr = lazyInstructions.front();
      lazyInstructions.pop_front();
      continue;
    }

    assert(!getVectorValue(lazyInstr) && !getScalarValue(lazyInstr) && "instruction already generated!");

    if (isa<CallInst>(lazyInstr)) {
      if (shouldVectorize(lazyInstr))
        vectorizeCallInstruction(cast<CallInst>(lazyInstr));
      else
        copyCallInstruction(cast<CallInst>(lazyInstr));
    } else if (canVectorize(lazyInstr))
        vectorizeMemoryInstruction(lazyInstr);
    else if (shouldVectorize(lazyInstr))
      fallbackVectorize(lazyInstr);
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
  if (getVectorValue(lazyInstr))
    return;

  assert(!getVectorValue(lazyInstr) && !getScalarValue(lazyInstr) && "instruction already generated!");

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
  Value *vecValue = getVectorValue(value);
  if (vecValue)  return vecValue;

  auto oldIP = builder.GetInsertPoint();
  auto oldIB = builder.GetInsertBlock();

  auto shape = getVectorShape(*value);
  if (shape.isVarying()) { // !vecValue
    auto * vecTy = VectorType::get(value->getType(), vectorWidth());
    Value * accu = UndefValue::get(vecTy);
    auto * intTy = Type::getInt32Ty(builder.getContext());

    for (int i = 0; i < vectorWidth(); ++i) {
      auto * laneVal = getScalarValue(value, i);
      auto * laneInst = dyn_cast<Instruction>(laneVal);
      if (laneInst) SetInsertBeforeTerm(builder, *laneInst->getParent());
      accu = builder.CreateInsertElement(accu, laneVal, ConstantInt::get(intTy, i, false), "_revec");
    }
    vecValue = accu;

  } else {
    vecValue = getScalarValue(value);
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
  Value * vecValue = nullptr;

  // create a vector GEP to widen pointers
  if (scaValue.getType()->isPointerTy()) {
    auto * scalarPtrTy = scaValue.getType();
    auto * intTy = builder.getInt32Ty();
    auto * ptrElemTy = GetPointerElementType(scalarPtrTy);

    // vecValue is a single pointer and has to be broadcasted to a vector of pointers first
    vecValue = builder.CreateVectorSplat(vectorWidth(), &scaValue);

    if (!vecShape.isUniform()) { // stride != 0
      assert(ptrElemTy->isSized() && "byte-stride shape on unsized element type");
      int scalarBytes = static_cast<int>(layout.getTypeStoreSize(ptrElemTy));
      assert(vecShape.getStride() % scalarBytes == 0);
      Value *contVec = createContiguousVector(vectorWidth(), intTy, 0, vecShape.getStride() / scalarBytes);
      vecValue = builder.CreateGEP(vecValue, contVec, "widen_ptr");
    }

  } else {
    if (isa<Constant>(scaValue)) {
      vecValue = getConstantVector(vectorWidth(), &cast<Constant>(scaValue));
    } else {
      vecValue = builder.CreateVectorSplat(vectorWidth(), &scaValue);
    }

    if (!vecShape.isUniform()) {
      assert(scaValue.getType()->isIntegerTy() || scaValue.getType()->isFloatingPointTy());

      auto *laneTy = scaValue.getType();
      Value *contVec = createContiguousVector(vectorWidth(), laneTy, 0, vecShape.getStride());
      vecValue = laneTy->isFloatingPointTy() ? builder.CreateFAdd(vecValue, contVec, "contiguous_add")
                                             : builder.CreateAdd(vecValue, contVec, "contiguous_add");
    }
  }

  return *vecValue;
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

  Value *mappedVal = getScalarValue(value, laneIdx);
  if (mappedVal) return mappedVal;

  // if value is integer or floating type, contiguous and has value for lane 0, add laneIdx
  Value *reqVal = nullptr;

  if (vecInfo.hasKnownShape(*value)) {
    VectorShape shape = getVectorShape(*value);
    Type *type = value->getType();
    mappedVal = getScalarValue(value);
    if (mappedVal && (shape.isContiguous() || shape.isStrided()) &&
        (type->isIntegerTy() || type->isFloatingPointTy())) {
      Constant *laneInt = type->isFloatingPointTy() ? ConstantFP::get(type, laneIdx * shape.getStride())
                                                    : ConstantInt::get(type, laneIdx * shape.getStride());
      reqVal = type->isFloatingPointTy() ? builder.CreateFAdd(mappedVal, laneInt,
                                                              value->getName() + "lane" + std::to_string(laneIdx))
                                         : builder.CreateAdd(mappedVal, laneInt,
                                                             value->getName() + "_lane" + std::to_string(laneIdx));
    }
  }

  // if value has a vector mapping -> extract from vector. if not -> clone scalar op
  if (!reqVal) {
    // to avoid dominance problems assume: if we only have a vectorized value and need a scalar one -> do not map
    skipMapping = true;
    mappedVal = getVectorValue(value);
    Instruction *mappedInst = dyn_cast<Instruction>(mappedVal);
    auto oldIP = builder.GetInsertPoint();
    auto oldIB = builder.GetInsertBlock();
    if (mappedInst) {
      Instruction *nextNode = mappedInst->getNextNode();
      while (nextNode && isa<PHINode>(nextNode))
        nextNode = nextNode->getNextNode();
      if (nextNode) {
        builder.SetInsertPoint(nextNode);
      } else if (mappedInst->getParent()->getTerminator())
        builder.SetInsertPoint(mappedInst->getParent()->getTerminator());
      else
        builder.SetInsertPoint(mappedInst->getParent());
    }
    IF_DEBUG {
      errs() << "Extracting a scalar value from a vector:\n";
      errs() << "Original Value: ";
      Dump(*value);
      errs() << "Vector Value: ";
      Dump(*mappedVal);
    };

    // if the mappedVal is a alloca instruction, create a GEP instruction
    if (isa<AllocaInst>(mappedVal))
      reqVal = builder.CreateGEP(mappedVal, ConstantInt::get(i32Ty, laneIdx));
    else {
      // extract from GEPs are not allowed. in that case recreate the scalar instruction and get that new value
      if (isa<GetElementPtrInst>(mappedVal) && isa<GetElementPtrInst>(value)) {
        reqVal = builder.CreateGEP(mappedVal, ConstantInt::get(i32Ty, laneIdx));
      } else {
        assert(!isa<GetElementPtrInst>(mappedVal) && "Extract from GEPs are not allowed!!");
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
  Value *mapped = getVectorValue(gep);
  if (mapped) return mapped;

  ++numVecGEPs;

  mapped = buildGEP(gep, false, 0);
  mapVectorValue(gep, mapped);
  return mapped;
}

llvm::Value*
NatBuilder::requestScalarGEP(llvm::GetElementPtrInst *const gep, unsigned laneIdx, bool skipMapping) {
  Value *mapped = getScalarValue(gep, laneIdx);
  if (mapped) return mapped;

  ++numScalGEPs;

  mapped = buildGEP(gep, true, laneIdx);
  if (!skipMapping)
    mapScalarValue(gep, mapped, laneIdx);
  return mapped;
}

llvm::Value*
NatBuilder::requestVectorBitCast(BitCastInst *const bc) {
  Value *mapped = getVectorValue(bc);
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
  Value *mapped = getScalarValue(bc, laneIdx);
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
    interAddr = builder.CreateGEP(ptr, ConstantInt::get(i32Ty, vectorWidth() * interleavedIdx), "inter_gep");
  }

  PointerType *vecPtrType = PointerType::getUnqual(vecType);
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
      cast<LoadInst>(loadInst)->setAlignment(alignment);
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

  const int laneBits = 32;
  auto * intLaneTy = Type::getIntNTy(vector->getContext(), laneBits);

  Constant *simdFalseConst = ConstantInt::get(vector->getContext(), APInt::getMinValue(vectorWidth() * laneBits));
  if (isRv_all)
    vector = builder.CreateNot(vector, "rvall_cond_not");

  Value * ptest = nullptr;
  // TODO from LLVM >= 5.0 use reduction intrinsics
  // rv_all(x) == !rv_any(!x)
  if (config.enableIRPolish) {
    // this is a workaround until LLVM reduction intrinsics are available
    // emit a rv_ptest intrinsic
    auto * redFunc = platInfo.requestVectorMaskReductionFunc("rv_reduce_or", vector->getType()->getVectorNumElements());
    ptest = builder.CreateCall(redFunc, vector, "ptest");

  } else {
    // idiomatic x86 ptest pattern
    Type *intVecType = VectorType::get(intLaneTy, vectorWidth());
    Type *intSIMDType = Type::getIntNTy(vector->getContext(), vectorWidth() * laneBits);
    Value *zext = builder.CreateSExt(vector, intVecType, "ptest_zext");
    Value *bc = builder.CreateBitCast(zext, intSIMDType, "ptest_bc");
    ptest = builder.CreateICmpNE(bc, simdFalseConst, "ptest_comp");
  }

  if (isRv_all)
    ptest = builder.CreateNot(ptest, "rvall_not");

  return ptest;
}

bool
NatBuilder::hasUniformPredicate(const BasicBlock & BB) const {
  if (!vecInfo.getRegion()->contains(&BB) || !vecInfo.getPredicate(BB)) return true;
  else return vecInfo.getVectorShape(*vecInfo.getPredicate(BB)).isUniform();
}

Value *NatBuilder::maskInactiveLanes(Value *const value, const BasicBlock* const block, bool invert) {
  auto pred = requestVectorValue(vecInfo.getPredicate(*block));
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
  Value * vecUsed = getScalarValue(&scaChainInst);
  if (!vecUsed) vecUsed = getVectorValue(&scaChainInst, 0);

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
  auto & vecPhi = *cast<PHINode>(getScalarValue(sp.phi, 0));
  auto & vecReductor = *cast<Instruction>(getScalarValue(sp.reductor, 0));

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
  auto * vecLatch = cast<BasicBlock>(getVectorValue(sp.phi->getIncomingBlock(loopOpIdx)));
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

void
NatBuilder::materializeRecurrence(Reduction & red, PHINode & scaPhi) {
  const int vectorWidth = vecInfo.getVectorWidth();
  assert(vecInfo.getVectorShape(scaPhi).isVarying());

// construct new (vectorized) initial value
  auto * vecPhi = cast<PHINode>(getVectorValue(&scaPhi));
  auto * vecTy = vecPhi->getType();

  auto * inAtZero = dyn_cast<Instruction>(scaPhi.getIncomingValue(0));
  int latchIdx = (inAtZero && vecInfo.inRegion(*inAtZero)) ? 0 : 1;
  int initIdx = 1 - latchIdx;

  BasicBlock * vecInitInputBlock = scaPhi.getIncomingBlock(initIdx);
  BasicBlock * vecLoopInputBlock = cast<BasicBlock>(getVectorValue(scaPhi.getIncomingBlock(latchIdx)));

// broadcast initial value to all lanes
  Value * scaInitValue = scaPhi.getIncomingValue(initIdx);
  IRBuilder<> phBuilder(vecInitInputBlock, vecInitInputBlock->getTerminator()->getIterator());
  auto * intTy = Type::getInt32Ty(scaPhi.getContext());
  auto * vecFirstLane = phBuilder.CreateInsertElement(UndefValue::get(vecTy), scaInitValue, ConstantInt::get(intTy, 0, false));
  auto * vecInitVal = CreateBroadcast(phBuilder, *vecFirstLane, 0);
  vecPhi->addIncoming(vecInitVal, vecInitInputBlock);

// add latch update (extract last lane)
  Instruction * scaLatchInst = cast<Instruction>(scaPhi.getIncomingValue(latchIdx));
  auto * vecLatchInst = cast<Instruction>(getVectorValue(scaLatchInst));
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
NatBuilder::materializeVaryingReduction(Reduction & red, PHINode & scaPhi) {
  assert((red.kind != RedKind::Top) && (red.kind != RedKind::Bot));

  const auto vectorWidth = vecInfo.getVectorWidth();
  auto * vecPhi = cast<PHINode>(getVectorValue(&scaPhi));
  auto redShape = red.getShape(vectorWidth);
  assert(redShape.isVarying()); (void) redShape;

// construct new (vectorized) initial value
  // TODO generalize to multi phi reductions
  Value * vecNeutral = ConstantVector::getSplat(vectorWidth, &GetNeutralElement(red.kind, *scaPhi.getType()));

  auto * inAtZero = dyn_cast<Instruction>(scaPhi.getIncomingValue(0));
  int latchIdx = (inAtZero && vecInfo.inRegion(*inAtZero)) ? 0 : 1;
  int initIdx = 1 - latchIdx;

  BasicBlock * vecInitInputBlock = scaPhi.getIncomingBlock(initIdx);
  BasicBlock * vecLoopInputBlock = cast<BasicBlock>(getVectorValue(scaPhi.getIncomingBlock(latchIdx)));

// materialize initial input (insert init value into last lane)
  Value * scaInitValue = scaPhi.getIncomingValue(initIdx);
  IRBuilder<> phBuilder(vecInitInputBlock, vecInitInputBlock->getTerminator()->getIterator());
  auto * intTy = Type::getInt32Ty(scaPhi.getContext());
  auto * vecInitVal = phBuilder.CreateInsertElement(vecNeutral, scaInitValue, ConstantInt::get(intTy, vectorWidth - 1, false));

// attach inputs (neutral elem and reduction inst)
  vecPhi->addIncoming(vecInitVal, vecInitInputBlock);

// add latch update
  Instruction * scaLatchInst = cast<Instruction>(scaPhi.getIncomingValue(latchIdx));
  auto * vecLatchInst = cast<Instruction>(getVectorValue(scaLatchInst));
  vecPhi->addIncoming(vecLatchInst, vecLoopInputBlock);

// reduce reduction phi for outside users
  repairOutsideUses(*scaLatchInst,
                    [&](Value & usedVal, BasicBlock & userBlock) ->Value& {
                      // otw, replace with reduced value
                      auto * insertPt = userBlock.getFirstNonPHI();
                      IRBuilder<> builder(&userBlock, insertPt->getIterator());
                      auto & reducedVector = CreateVectorReduce(builder, red.kind, *vecLatchInst);
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

    auto& vecElem = *cast<Instruction>(getVectorValue(elem));

    // reduce outside uses on demand
    repairOutsideUses(*elem,
                      [&](Value & usedVal, BasicBlock& userBlock) ->Value& {
                      auto * insertPt = userBlock.getFirstNonPHI();
                      IRBuilder<> builder(&userBlock, insertPt->getIterator());
                      // reduce all end-of-iteration values and request value of last iteration
                      auto & foldVec = *builder.CreateSelect(selMask, vecLatchInst, &vecElem, ".red");
                      auto & reducedVector = CreateVectorReduce(builder, red.kind, foldVec);
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
      materializeVaryingReduction(*red, *scalPhi);

    } else if (isVectorLoopHeader && red && red->kind == RedKind::Bot && shape.isVarying()) {
      // reduction phi handling
      IF_DEBUG_NAT { errs() << "-- materializing "; red->dump(); errs() << "\n"; }
      materializeRecurrence(*red, *scalPhi);

    } else {
      // default phi handling (includes fully uniform recurrences)
      for (unsigned lane = 0; lane < loopEnd; ++lane) {
        PHINode *phi = cast<PHINode>(
            !shape.isVarying() || replicate ? getScalarValue(scalPhi, lane) : getVectorValue(scalPhi));
        for (unsigned i = 0; i < scalPhi->getNumIncomingValues(); ++i) {
          // set insertion point to before Terminator of incoming block
          BasicBlock *incVecBlock = cast<BasicBlock>(getVectorValue(scalPhi->getIncomingBlock(i), true));
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

Value *NatBuilder::getVectorValue(Value *const value, bool getLastBlock) {
  if (isa<BasicBlock>(value)) {
    if (!region->contains(cast<BasicBlock>(value))) {
      return value; // preserve BBs outside of the region
    }

    BasicBlock *const block = cast<BasicBlock>(value);
    auto blockIt = basicBlockMap.find(block);
    if (blockIt != basicBlockMap.end()) {
      BasicBlockVector &blocks = blockIt->second;
      return getLastBlock ? blocks.back() : blocks.front();
    }
  }

  auto vecIt = vectorValueMap.find(value);
  if (vecIt != vectorValueMap.end()) return vecIt->second;
  else return nullptr;
}

void NatBuilder::mapScalarValue(const Value *const value, Value *mapValue, unsigned laneIdx) {
  LaneValueVector &laneValues = scalarValueMap[value];
  if (laneValues.size() < laneIdx) laneValues.resize(laneIdx);
  laneValues.insert(laneValues.begin() + laneIdx, mapValue);
}

Value *NatBuilder::getScalarValue(Value *const value, unsigned laneIdx) {
  if (isa<MetadataAsValue>(value)) {
    // as used in "llvm.dbg.value" calls
    return value;
  }

  // in case of regions, keep any values that are live into the region
  // FIXME make this generic through explicit argument mapping
  if (region->isVectorLoop() && isa<Argument>(value)) {
    return value;
  } else if (region->isVectorLoop() && isa<Instruction>(value) && !region->contains(cast<Instruction>(value)->getParent())) {
    return value;
  }

  const Constant *constant = dyn_cast<const Constant>(value);
  if (constant) return const_cast<Constant *>(constant);

  auto scalarIt = scalarValueMap.find(value);
  if (scalarIt != scalarValueMap.end()) {
    VectorShape shape;
    if (vecInfo.hasKnownShape(*value)) {
      shape = getVectorShape(*value);
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
  if (!region->contains(block)) {
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
  if (!config.enableInterleaved)
    return false;

  StructType *st;
  if ((st = isStructAccess(accessedPtr)) && !isHomogeneousStruct(st, layout))
    return false;

  // group memory instructions based on their dependencies
  InstructionGrouper instructionGrouper;
  instructionGrouper.add(inst, memDepRes);
  for (Instruction *instr : lazyInstructions) {
    instructionGrouper.add(instr, memDepRes);
  }

  InstructionGroup instrGroup = instructionGrouper.getInstructionGroup(inst);
  if (instrGroup.size() <= 1)
    return false;

  const VectorShape &addrShape = getVectorShape(*accessedPtr);

  // group our group based on memory layout next
  MemoryAccessGrouper memoryGrouper(SE, static_cast<unsigned>(byteSize));
  std::map<Value *, const SCEV *> addrSCEVMap;
  std::map<const SCEV *, Value *> scevInstrMap;
  for (Instruction *instr : instrGroup) {
    Value *addrVal = getPointerOperand(instr);
    assert(addrVal && "grouped instruction was not a memory instruction!!");
    // only group strided accesses
    VectorShape shape = getVectorShape(*addrVal);
    bool groupByteContiguous = shape.isStrided(static_cast<int>(layout.getTypeStoreSize(cast<PointerType>(addrVal->getType())->getElementType())));
    if (!shape.isStrided() || groupByteContiguous)
      continue;
    const SCEV *scev = memoryGrouper.add(addrVal);
    addrSCEVMap[addrVal] = scev;
    scevInstrMap[scev] = instr;
  }

  // check if there is an interleaved memory group for our base address
  const MemoryGroup &memGroup = memoryGrouper.getMemoryGroup(addrSCEVMap[accessedPtr]);
  int stride = addrShape.getStride() / byteSize;
  bool hasGaps = false;
  for (unsigned i = 0; i < memGroup.size(); ++i) {
    if (!memGroup[i]) {
      hasGaps = true;
      break;
    }
    srcs.push_back(scevInstrMap[memGroup[i]]);
  }

  // we have found a memory group if it has no gaps and the size is bigger than 1
  return !hasGaps && memGroup.size() > 1 && static_cast<int>(memGroup.size()) == stride;
}

bool NatBuilder::isPseudointerleaved(Instruction *inst, Value *addr, int byteSize) {
  if (!config.enablePseudoInterleaved)
    return false;

  VectorShape addrShape = getVectorShape(*addr);
  if (!addrShape.isStrided() || addrShape.isStrided(byteSize))
    return false;

  StructType *st;
  if ((st = isStructAccess(addr)) && !isHomogeneousStruct(st, layout))
    return false;

  if (inst) {
    bool isLoad = isa<LoadInst>(inst);

    // clean up earlier pseudo-interleaved loads if needed. rules:
    // if load && values exist for addr -> delete
    // if store && call or store earlier -> delete
    if (isLoad && pseudointerValueMap.count(addr))
      pseudointerValueMap.erase(addr);
    else if (!isLoad) {
      Value *baseAddr = getBasePointer(addr);
      LoadInst *load;
      bool erase = false;

      Instruction *prevInst = inst;
      while ((prevInst = prevInst->getPrevNode())) {
        CallInst *call = dyn_cast<CallInst>(prevInst);
        StoreInst *store = dyn_cast<StoreInst>(prevInst);
        load = dyn_cast<LoadInst>(prevInst);

        if (call && call->mayHaveSideEffects()) {
          erase = true;
          break;
        }

        if (store) {
          Value *storeBase = getBasePointer(store->getPointerOperand());
          if (storeBase == baseAddr) {
            erase = true;
            break;
          }
        }

        if (load && load->getPointerOperand() == addr) break;
      }

      if (erase)
        pseudointerValueMap.erase(addr);
    }
  }

  int stride = addrShape.getStride() / byteSize;
  // FIXME current implementation breaks for negative strides
  return stride > 0 && stride <= (int) vectorWidth() - 1; // need at least two elements per vector
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
    if (addrShape.isUniform() || addrShape.isContiguous() || addrShape.isStrided(byteSize) ||
        isPseudointerleaved(nullptr, gep, byteSize)) {
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

        if (!(addrShape.isUniform() || addrShape.isContiguous() || addrShape.isStrided(byteSize) ||
            isPseudointerleaved(nullptr, gep, byteSize))) {
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
