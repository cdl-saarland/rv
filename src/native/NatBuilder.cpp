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
#include <llvm/IR/Module.h>

#include "NatBuilder.h"
#include "Utils.h"


#include "rv/analysis/reductionAnalysis.h"

#include "rvConfig.h"
#include "ShuffleBuilder.h"

#define IF_DEBUG_NAT IF_DEBUG

using namespace native;
using namespace llvm;
using namespace rv;

VectorShape
NatBuilder::getShape(const Value & val) {
  if (vectorizationInfo.hasKnownShape(val)) return vectorizationInfo.getVectorShape(val);
  else return VectorShape::uni();
}

NatBuilder::NatBuilder(PlatformInfo &platformInfo, VectorizationInfo &vectorizationInfo,
                       const DominatorTree &dominatorTree, MemoryDependenceAnalysis &memDepAnalysis,
                       ScalarEvolution &SE, ReductionAnalysis & _reda) :
    builder(vectorizationInfo.getMapping().vectorFn->getContext()),
    platformInfo(platformInfo),
    vectorizationInfo(vectorizationInfo),
    dominatorTree(dominatorTree),
    memDepAnalysis(memDepAnalysis),
    SE(SE),
    reda(_reda),
    layout(vectorizationInfo.getScalarFunction().getParent()),
    i1Ty(IntegerType::get(vectorizationInfo.getMapping().vectorFn->getContext(), 1)),
    i32Ty(IntegerType::get(vectorizationInfo.getMapping().vectorFn->getContext(), 32)),
    region(vectorizationInfo.getRegion()),
    useScatterGatherIntrinsics(true),
    vectorizeInterleavedAccess(false),
    cascadeLoadMap(),
    cascadeStoreMap(),
    vectorValueMap(),
    scalarValueMap(),
    basicBlockMap(),
    grouperMap(),
    phiVector(),
    willNotVectorize(),
    lazyInstructions() {}

void NatBuilder::vectorize() {
  const Function *func = vectorizationInfo.getMapping().scalarFn;
  Function *vecFunc = vectorizationInfo.getMapping().vectorFn;

  IF_DEBUG {
    errs() << "VA before vector codegen\n";
    vectorizationInfo.dump();
  }

  // map arguments first
  if (!region) {
    unsigned i = 0;
    auto sit = func->getArgumentList().begin();
    for (auto it = vecFunc->getArgumentList().begin(), et = vecFunc->getArgumentList().end();
         it != et; ++it, ++sit, ++i) {
      Argument *arg = &*it;
      const Argument *sarg = &*sit;
      arg->setName(sarg->getName());
      VectorShape argShape = vectorizationInfo.getMapping().argShapes[i];
      if (argShape.isVarying() && !arg->getType()->isPointerTy())
        mapVectorValue(sarg, arg);
      else
        mapScalarValue(sarg, arg);
    }
  }

  // create all BasicBlocks first and map them
  for (auto &block : *func) {
    if (region && !region->contains(&block)) continue;

    BasicBlock *vecBlock = BasicBlock::Create(vecFunc->getContext(), block.getName() + ".rv", vecFunc);
    mapVectorValue(&block, vecBlock);
  }

  // traverse dominator tree in pre-order to ensure all uses have definitions vectorized first
  std::deque<const DomTreeNode *> nodeQueue;
  const DomTreeNode *rootNode = region ? dominatorTree.getNode(&region->getRegionEntry()) : dominatorTree.getRootNode();
  nodeQueue.push_back(rootNode);
  while (!nodeQueue.empty()) {
    // FIFO for pre-order
    const DomTreeNode *node = nodeQueue.front();
    nodeQueue.pop_front();

    // vectorize
    BasicBlock *bb = node->getBlock();
    if (region && !region->contains(bb)) continue;

    BasicBlock *vecBlock = cast<BasicBlock>(getVectorValue(bb));
    vectorize(bb, vecBlock);

    // populate queue with pre-order dominators
    for (auto it = node->getChildren().begin(), et = node->getChildren().end(); it != et; ++it) {
      nodeQueue.push_back(*it);
    }
  }

  // revisit PHINodes now and add the mapped incoming values
  if (!phiVector.empty()) addValuesToPHINodes();

  if (!region) return;

  // TODO what about outside uses?

  // rewire branches outside the region to go to the region instead
  std::vector<BasicBlock *> oldBlocks;
  for (auto &BB : *vecFunc) {
    if (region->contains(&BB)) {
      oldBlocks.push_back(&BB);
      continue; // keep old region
    }
    auto &termInst = *BB.getTerminator();
    for (uint i = 0; i < termInst.getNumOperands(); ++i) {
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
    oldBB->dropAllReferences();
  }
  for (auto *oldBB : oldBlocks) {
    oldBB->eraseFromParent();
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
    AllocaInst *alloca = dyn_cast<AllocaInst>(inst);

    // loads and stores need special treatment (masking, shuffling, etc) (build them lazily)
    if (canVectorize(inst) && (load || store))
      if (vectorizeInterleavedAccess) lazyInstructions.push_back(inst);
      else vectorizeMemoryInstruction(inst);
    else if (call) {
      // calls need special treatment
      if (call->getCalledFunction()->getName() == "rv_any")
        vectorizeReductionCall(call, false);
      else if (call->getCalledFunction()->getName() == "rv_all")
        vectorizeReductionCall(call, true);
      else if (call->getCalledFunction()->getName() == "rv_extract")
        vectorizeExtractCall(call);
      else if (call->getCalledFunction()->getName() == "rv_ballot")
        vectorizeBallotCall(call);
      else
        if (vectorizeInterleavedAccess) lazyInstructions.push_back(inst);
        else {
          if (shouldVectorize(call))
            vectorizeCallInstruction(call);
          else {
            copyCallInstruction(call);
          }
        }
    } else if (phi)
      // phis need special treatment as they might contain not-yet mapped instructions
      vectorizePHIInstruction(phi);
    else if (alloca && shouldVectorize(inst)) {
      // note: this is ONLY allowed IFF
      // (1) no calls that have alloca instructions as arguments OR
      // (2) there exists a function mapping which allows that. e.g.: float * -> <4 x float> *
      if (canVectorize(inst))
        vectorizeAllocaInstruction(alloca);
      else
        for (unsigned lane = 0; lane < vectorWidth(); ++lane) {
          copyInstruction(inst, lane);
        }
    } else if (gep) {
//      unsigned laneEnd = shouldVectorize(gep) ? vectorWidth() : 1;
//      for (unsigned lane = 0; lane < laneEnd; ++lane) {
//        vectorizeGEPInstruction(gep, lane, laneEnd == vectorWidth());
//      }
      vectorizeGEPInstruction(gep, shouldVectorize(gep));
    } else if (canVectorize(inst) && shouldVectorize(inst))
      vectorize(inst);
    else if (!canVectorize(inst) && shouldVectorize(inst))
      fallbackVectorize(inst);
    else
      copyInstruction(inst);
  }
}

/* expects that builder has valid insertion point set */
void NatBuilder::mapOperandsInto(Instruction *const scalInst, Instruction *inst, bool vectorizedInst,
                                 unsigned laneIdx) {
  assert(inst && "no instruction to map operands into");
  assert(scalInst && "no instruction to map operands from");
  assert(builder.GetInsertBlock() && "no insertion point set");

  unsigned e = isa<CallInst>(scalInst) ? inst->getNumOperands() - 1 : inst->getNumOperands();
  for (unsigned i = 0; i < e; ++i) {
    Value *op = scalInst->getOperand(i);
    Value *mappedOp = (vectorizedInst || isa<BasicBlock>(op)) ? requestVectorValue(op) : requestScalarValue(op,
                                                                                                            laneIdx);
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
  assert(vectorizationInfo.hasKnownShape(*scalPhi) && "no VectorShape for PHINode available!");
  VectorShape shape = getShape(*scalPhi);
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
}

GetElementPtrInst *NatBuilder::vectorizeGEPInstruction(GetElementPtrInst *const gep, bool buildVectorGEP,
                                                        unsigned interleavedIndex,
                                                        bool skipMapping) {
  assert(gep->getNumOperands() - 1 == gep->getNumIndices() && "LLVM Code for GEP changed!");
  Value *scalPtr = gep->getPointerOperand();

  if (isa<Instruction>(scalPtr))
    assert(vectorizationInfo.hasKnownShape(*scalPtr) && "no shape for instruction!!");

  // we need to build an expanded GEP if we are building a vector GEP and the base pointer is also a GEP
  // expanded GEP means: base pointer of used GEP, indices of used GEP, then indices of current GEP

  // ptr expansion
  GetElementPtrInst *baseGEP = nullptr; // don't do gep cascading dyn_cast<GetElementPtrInst>(scalPtr);
  if (buildVectorGEP && baseGEP)
    scalPtr = baseGEP->getPointerOperand();

  VectorShape opShape = getShape(*scalPtr);
  Value *ptr;
  if (opShape.isUniform() || !buildVectorGEP)
    ptr = requestScalarValue(scalPtr);
  else if (isa<AllocaInst>(scalPtr)) {
    ptr = UndefValue::get(getVectorType(scalPtr->getType(), vectorWidth()));
    for (unsigned i = 0; i < vectorWidth(); ++i) {
      Value *insert = requestScalarValue(scalPtr, i);
      ptr = builder.CreateInsertElement(ptr, insert, i);
    }
  } else
    ptr = requestVectorValue(scalPtr);


  // index expansion
  unsigned offset = 0;
  unsigned start = 0;
  unsigned numIndices = (buildVectorGEP && baseGEP) ? (gep->getNumIndices() + baseGEP->getNumIndices() - 1)
                                                  : gep->getNumIndices();
  Value **idxList = new Value *[numIndices];
  if (buildVectorGEP && baseGEP) {
    for (unsigned i = 0; i < gep->getNumIndices() - 1; ++i) {
      Value *operand = baseGEP->getOperand(i + 1);
      opShape = getShape(*operand);
      idxList[i] = opShape.isUniform() ? requestScalarValue(operand) : requestVectorValue(operand);
    }
    offset = baseGEP->getNumIndices() - 1;
    Value *lastOp = baseGEP->getOperand(baseGEP->getNumIndices());
    Value *firstOp = gep->getOperand(1);
    VectorShape shape1 = getShape(*lastOp);
    VectorShape shape2 = getShape(*firstOp);
    if (shape1.isUniform() && shape2.isUniform()) {
      lastOp = requestScalarValue(lastOp);
      firstOp = requestScalarValue(firstOp);
    } else {
      lastOp = requestVectorValue(lastOp);
      firstOp = requestVectorValue(firstOp);
    }

    Type *lastType = lastOp->getType();
    Type *firstType = firstOp->getType();
    if (lastType != firstType) {
      unsigned lastSize = lastType->getScalarSizeInBits(), firstSize = firstType->getScalarSizeInBits();
      if (lastSize > firstSize)
        firstOp = builder.CreateSExt(firstOp, lastType);
      else
        lastOp = builder.CreateSExt(lastOp, firstType);
    }
    idxList[offset] = builder.CreateAdd(lastOp, firstOp);
    start = 1;
  }

  for (unsigned i = start; i < gep->getNumIndices(); ++i) {
    Value *operand = gep->getOperand(i + 1);
    opShape = getShape(*operand);
    Value *index = buildVectorGEP && !opShape.isUniform() ? requestVectorValue(operand) : requestScalarValue(operand);
    idxList[i + offset] = index;

    if (interleavedIndex > 0 && !opShape.isUniform()) {
      assert(!buildVectorGEP && "interleavedIndex > 0 outside of interleaved!");
      Type *lastIndexType = index->getType();
      Constant *offsetConst = ConstantInt::get(lastIndexType, interleavedIndex, true);
      idxList[i + offset] = builder.CreateAdd(index, offsetConst);
    }
  }
  GetElementPtrInst *vgep = cast<GetElementPtrInst>(
      builder.CreateGEP(ptr, ArrayRef<Value *>(idxList, numIndices), gep->getName()));
  vgep->setIsInBounds(gep->isInBounds());

  // might skip mapping
  if (!skipMapping) {
    if (buildVectorGEP)
      mapVectorValue(gep, vgep);
    else
      mapScalarValue(gep, vgep);
  }
  delete [] idxList;

  return vgep;
}

/* expects that builder has valid insertion point set */
void NatBuilder::copyInstruction(Instruction *const inst, unsigned laneIdx) {
  assert(inst && "no instruction to copy");
  assert(builder.GetInsertBlock() && "no insertion point set");
  Instruction *cpInst = inst->clone();
  BranchInst *branch = dyn_cast<BranchInst>(cpInst);
  if (branch && vectorizationInfo.hasKnownShape(*inst))
    assert(getShape(*inst).isUniform() && "branch not uniform");
  if (branch && branch->isConditional()) {
    Value *cond = branch->getCondition();
    VectorShape shape = getShape(*cond);
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
}

void NatBuilder::fallbackVectorize(Instruction *const inst) {
  // fallback for vectorizing varying instructions:
  // if !void: create result vector
  // clone instruction
  // map operands into instruction
  // if !void: insert into result vector
  // repeat from line 3 for all lanes
  Type *type = inst->getType();
  bool notVectorTy = type->isVoidTy() || !(type->isIntegerTy() || type->isFloatingPointTy());
  Value *resVec = notVectorTy ? nullptr : UndefValue::get(
      getVectorType(inst->getType(), vectorWidth()));
  for (unsigned lane = 0; lane < vectorWidth(); ++lane) {
    Instruction *cpInst = inst->clone();
    mapOperandsInto(inst, cpInst, false, lane);
    builder.Insert(cpInst, inst->getName());
    if (notVectorTy) mapScalarValue(inst, cpInst, lane);
    if (resVec) resVec = builder.CreateInsertElement(resVec, cpInst, lane, "fallBackInsert");
  }
  if (resVec) mapVectorValue(inst, resVec);
}

/* expects that builder has valid insertion point set */
void NatBuilder::vectorize(Instruction *const inst) {
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
}

void NatBuilder::vectorizeReductionCall(CallInst *rvCall, bool isRv_all) {
  assert(rvCall->getNumArgOperands() == 1 && "expected only 1 argument for rv_any");

  Value *predicate = rvCall->getArgOperand(0);
  const VectorShape &shape = getShape(*predicate);
  assert((shape.isVarying() || shape.isUniform()) && "predicate can't be contigious or strided");

  Value *reduction;
  if (shape.isVarying()) {
    Value *vecPredicate = maskInactiveLanes(requestVectorValue(predicate), rvCall->getParent(), isRv_all);
    reduction = createPTest(vecPredicate, isRv_all);
  } else {
    reduction = requestScalarValue(predicate);
  }

  mapScalarValue(rvCall, reduction);
}

void
NatBuilder::vectorizeExtractCall(CallInst *rvCall) {
  assert(rvCall->getNumArgOperands() == 2 && "expected 2 arguments for rv_extract(vec, laneId)");

  Value *vecArg = rvCall->getArgOperand(0);

// uniform arg
  if (getShape(*vecArg).isUniform()) {
    auto * uniVal = requestScalarValue(vecArg);
    mapScalarValue(rvCall, uniVal);
    return;
  }

// non-uniform arg
  auto * vecVal = requestVectorValue(vecArg);
  int laneId = cast<ConstantInt>(rvCall->getArgOperand(1))->getZExtValue();

  auto * laneVal = builder.CreateExtractElement(vecVal, laneId, "rv_ext");
  mapScalarValue(rvCall, laneVal);
}

void
NatBuilder::vectorizeBallotCall(CallInst *rvCall) {
  assert(rvCall->getNumArgOperands() == 1 && "expected 1 argument for rv_ballot(cond)");

  Value *condArg = rvCall->getArgOperand(0);

// uniform arg
  if (getShape(*condArg).isUniform()) {
    auto * uniVal = requestScalarValue(condArg);
    uniVal = builder.CreateZExt(uniVal, i32Ty, "rv_ballot");
    mapScalarValue(rvCall, uniVal);
    return;
  }

  Module *mod = vectorizationInfo.getMapping().vectorFn->getParent();

  auto vecWidth = vectorizationInfo.getVectorWidth();
  assert((vecWidth == 4 || vecWidth == 8) && "rv_ballot only supports SSE and AVX instruction sets");

// non-uniform arg
  auto * vecVal = maskInactiveLanes(requestVectorValue(condArg), rvCall->getParent(), false);
  auto * intVecTy = VectorType::get(i32Ty, vecWidth);

  auto * extVal = builder.CreateSExt(vecVal, intVecTy, "rv_ballot");
  auto * simdVal = builder.CreateBitCast(extVal, VectorType::get(builder.getFloatTy(), vecWidth), "rv_ballot");

  Intrinsic::ID id = vecWidth == 4 ? Intrinsic::x86_sse_movmsk_ps : Intrinsic::x86_avx_movmsk_ps_256;
  auto movMaskDecl = Intrinsic::getDeclaration(mod, id);
  auto * mask = builder.CreateCall(movMaskDecl, simdVal, "rv_ballot");
  mapScalarValue(rvCall, mask);
}

static bool HasSideEffects(CallInst &call) {
  return call.mayHaveSideEffects();
}

void NatBuilder::vectorizeCallInstruction(CallInst *const scalCall) {
  Function *callee = scalCall->getCalledFunction();
  StringRef calleeName = callee->getName();

  // is func is vectorizable (standard mapping exists for given vector width), create new call to vector func
  if (platformInfo.isFunctionVectorizable(calleeName, vectorWidth())) {

    CallInst *call = cast<CallInst>(scalCall->clone());
    bool doublePrecision = false;
    if (call->getNumArgOperands() > 0)
      doublePrecision = call->getArgOperand(0)->getType()->isDoubleTy();
    Module *mod = vectorizationInfo.getMapping().vectorFn->getParent();
    Function *simdFunc = platformInfo.requestVectorizedFunction(calleeName, vectorWidth(), mod, doublePrecision);
    call->setCalledFunction(simdFunc);
    call->mutateType(simdFunc->getReturnType());
    mapOperandsInto(scalCall, call, true);
    mapVectorValue(scalCall, call);
    builder.Insert(call, scalCall->getName());

  } else {

    // check if we need cascade first
    Value *predicate = vectorizationInfo.getPredicate(*scalCall->getParent());
    assert(predicate && "expected predicate!");
    assert(predicate->getType()->isIntegerTy(1) && "predicate must be i1 type!");
    bool needCascade = !isa<Constant>(predicate) && HasSideEffects(*scalCall);

    // if we need cascading, we need the vectorized predicate and the cascading blocks
    std::vector<BasicBlock *> condBlocks;
    std::vector<BasicBlock *> maskedBlocks;
    BasicBlock *resBlock = nullptr;
    if (needCascade) {
      BasicBlock *vecBlock = cast<BasicBlock>(getVectorValue(scalCall->getParent()));
      resBlock = createCascadeBlocks(vecBlock->getParent(), vectorWidth(), condBlocks, maskedBlocks);

      // branch to our entry block of the cascade
      builder.CreateBr(condBlocks[0]);
      builder.SetInsertPoint(condBlocks[0]);
    }

    // type of the call. we don't need to construct a result if void, vector or struct
    Type *callType = scalCall->getType();
    Value *resVec = (callType->isVoidTy() || callType->isVectorTy() || callType->isStructTy())
                    ? nullptr
                    : UndefValue::get(getVectorType(callType, vectorWidth()));

    // create <vector_width> scalar calls
    for (unsigned lane = 0; lane < vectorWidth(); ++lane) {
      BasicBlock *condBlock = nullptr;
      BasicBlock *maskedBlock = nullptr;
      BasicBlock *nextBlock = nullptr;

      // if predicated, extract from mask and conditionally branch
      if (needCascade) {
        condBlock = condBlocks[lane];
        maskedBlock = maskedBlocks[lane];
        nextBlock = lane == vectorWidth() - 1 ? resBlock : condBlocks[lane + 1];

        assert(builder.GetInsertBlock() == condBlock);
        Value *mask = requestScalarValue(predicate, lane,
                                         needCascade); // do not map this value if it's fresh to avoid dominance violations
        builder.CreateCondBr(mask, maskedBlock, nextBlock);
        builder.SetInsertPoint(maskedBlock);
      }

      // (masked block or not cascaded)
      // for each argument, get lane value of argument, do the call, (if !voidTy, !vectorTy, !structTy) insert to resVec
      std::vector<Value *> args;
      for (unsigned i = 0; i < scalCall->getNumArgOperands(); ++i) {
        Value *scalArg = scalCall->getArgOperand(i);
        Value *laneArg = requestScalarValue(scalArg, lane,
                                            needCascade); // do not map this value if it's fresh to avoid dominance violations
        args.push_back(laneArg);
      }
      Twine suffix = callType->isVoidTy() ? "" : "_lane_" + std::to_string(lane);
      auto scalCallName = scalCall->getName();
      auto vecCallName = scalCallName.empty() ? suffix : scalCallName + suffix;
      Value *call = builder.CreateCall(callee, args, vecCallName);
      if (!needCascade)
        mapScalarValue(scalCall, call, lane); // might proof useful. but only if not predicated

      Value *insert = nullptr;
      if (!(callType->isVoidTy() || callType->isVectorTy() || callType->isStructTy())) {
        insert = builder.CreateInsertElement(resVec, call, ConstantInt::get(i32Ty, lane),
                                             "insert_lane_" + std::to_string(lane));
      }

      // if predicated, branch to nextBlock and create phi which will become resVec. else, insert is resVec
      if (needCascade) {
        builder.CreateBr(nextBlock);
        builder.SetInsertPoint(nextBlock);

        if (!callType->isVoidTy()) {
          PHINode *phi = builder.CreatePHI(resVec->getType(), 2);
          phi->addIncoming(resVec, condBlock);
          phi->addIncoming(insert, maskedBlock);
          resVec = phi;
        }
      } else if (!callType->isVoidTy()) {
        resVec = insert;
      }
    }

    // map resVec as vector value for scalCall and remap parent block of scalCall with resBlock
    mapVectorValue(scalCall, resVec);
    if (resBlock) mapVectorValue(scalCall->getParent(), resBlock);
  }
}

void NatBuilder::copyCallInstruction(CallInst *const scalCall, unsigned laneIdx) {
  // copying call instructions:
  // 1) get scalar callee
  // 2) construct arguments
  // 3) create call instruction
  Function *callee = scalCall->getCalledFunction();

  std::vector<Value *> args;
  for (unsigned i = 0; i < scalCall->getNumArgOperands(); ++i) {
    Value *scalArg = scalCall->getArgOperand(i);
    Value *laneArg = requestScalarValue(scalArg, laneIdx);
    args.push_back(laneArg);
  }

  Value *call = builder.CreateCall(callee, args, scalCall->getName());
  mapScalarValue(scalCall, call, laneIdx);
}

void NatBuilder::vectorizeMemoryInstruction(Instruction *const inst) {
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

  assert(accessedType == cast<PointerType>(accessedPtr->getType())->getElementType() && "accessed type and pointed object type differ!");
  assert(vectorizationInfo.hasKnownShape(*accessedPtr) && "no shape for accessed pointer!");
  VectorShape addrShape = getShape(*accessedPtr);
  VectorShape instrShape = load ? getShape(*load) : getShape(*store);

  // address: uniform -> scalar op. contiguous -> scalar from vector-width address. varying -> scatter/gather
  Value *vecPtr = nullptr;

  // check if predicate is non-trivial and set needsMask flag accordingly
  Value *predicate = vectorizationInfo.getPredicate(*inst->getParent());
  assert(predicate && predicate->getType()->isIntegerTy(1) && "predicate must have i1 type!");
  bool needsMask = !isa<Constant>(predicate);

  Type *vecType = addrShape.isUniform() ? accessedType : getVectorType(accessedType, vectorWidth());

  // alignments and contiguous check
  unsigned origAlignment = load ? load->getAlignment() : store->getAlignment();
  unsigned alignment = 0;
  bool byteContiguous = addrShape.isStrided(static_cast<int>(layout.getTypeStoreSize(accessedType)));

  // used for memory-interleaving
  bool isInterleaved = false;
  MemoryGroup memGroup;
  std::map<const SCEV *, Instruction *> scevInstrMap;
  std::vector<Value *> sourceAddrs;
  std::vector<Value *> sources;

  if (addrShape.isContiguous() || byteContiguous) {
    // cast pointer to vector-width pointer
    Value *mappedPtr = requestScalarValue(accessedPtr);
    PointerType *vecPtrType = PointerType::getUnqual(vecType);
    vecPtr = builder.CreatePointerCast(mappedPtr, vecPtrType, "vec_cast");
    alignment = instrShape.getAlignmentFirst();

  } else if (addrShape.isUniform()) {
    vecPtr = requestScalarValue(accessedPtr);
    alignment = instrShape.getAlignmentFirst();

  } else if (addrShape.isStrided()) {
    // group memory instructions based on their dependencies
    InstructionGrouper instructionGrouper;
    instructionGrouper.add(inst, memDepAnalysis);
    for (Instruction *instr : lazyInstructions) {
      instructionGrouper.add(instr, memDepAnalysis);
    }
    InstructionGroup instrGroup = instructionGrouper.getInstructionGroup(inst);
    if (instrGroup.size() > 1) {
      // group our group based on memory layout next
      MemoryAccessGrouper memoryGrouper(SE, static_cast<unsigned>(layout.getTypeStoreSize(accessedType)));
      std::map<Value *, const SCEV *> addrSCEVMap;
      for (Instruction *instr : instrGroup) {
        Value *addrVal = getPointerOperand(instr);
        assert(addrVal && "grouped instruction was not a memory instruction!!");
        // only group strided accesses
        VectorShape shape = getShape(*addrVal);
        bool groupByteContiguous = addrShape.isStrided(static_cast<int>(layout.getTypeStoreSize(cast<PointerType>(accessedPtr->getType())->getElementType())));
        if (!shape.isStrided() || groupByteContiguous)
          continue;
        const SCEV *scev = memoryGrouper.add(addrVal);
        addrSCEVMap[addrVal] = scev;
        scevInstrMap[scev] = instr;
      }

      // check if there is an interleaved memory group for our base address
      memGroup = memoryGrouper.getMemoryGroup(addrSCEVMap[accessedPtr]);
      int stride = addrShape.getStride() / (accessedType->getScalarSizeInBits() / 8);
      bool hasGaps = false;
      for (unsigned i = 0; i < memGroup.size(); ++i) {
        if (!memGroup[i]) {
          hasGaps = true;
          break;
        }
      }

      // we have found a memory group if it has no gaps and the size is bigger than 1
      isInterleaved = !hasGaps && memGroup.size() > 1 && static_cast<int>(memGroup.size()) == stride;

      // TODO: support interleaved for structs
      if (isStructAccess(accessedPtr))
        isInterleaved = false;
    }

    if (isInterleaved) {
      // build as many contiguous GEPs as there are members of the group and shuffle them
      unsigned offset = 0;
      unsigned maskIdx = 0;
      for (auto SCEV : memGroup) {
        Instruction *sourceMem = scevInstrMap[SCEV];
        sources.push_back(sourceMem);
        assert(sourceMem && "no source instruction available for this SCEV");
        if (!offset)
          accessedPtr = getPointerOperand(sourceMem);
        Value *mappedPtr;

        // generate scalar gep on the fly (because we need offsets)
        if (isa<GetElementPtrInst>(accessedPtr))
          mappedPtr = vectorizeGEPInstruction(cast<GetElementPtrInst>(accessedPtr), false, offset, true);
        else {
          mappedPtr = requestScalarValue(accessedPtr);
          if (offset > 0)
            mappedPtr = builder.CreateGEP(mappedPtr, ConstantInt::get(i32Ty, offset, true));
        }
        PointerType *vecPtrType = PointerType::getUnqual(vecType);
        vecPtr = builder.CreatePointerCast(mappedPtr, vecPtrType, "vec_cast");
        sourceAddrs.push_back(vecPtr);

        offset += vectorWidth();
        ++maskIdx;
      }
    }


    alignment = instrShape.getAlignmentGeneral();
  }

  if (addrShape.isVarying() || (!byteContiguous && addrShape.isStrided() && !isInterleaved)) {
    // varying or non-interleaved strided. gather the addresses for the lanes
    vecPtr = isa<Argument>(accessedPtr) ? getScalarValue(accessedPtr) : getVectorValue(accessedPtr);
    if (!vecPtr) {
      vecPtr = UndefValue::get(getVectorType(accessedPtr->getType(), vectorWidth()));
      for (unsigned i = 0; i < vectorWidth(); ++i) {
        Value *lanePtr = requestScalarValue(accessedPtr, i);
        vecPtr = builder.CreateInsertElement(vecPtr, lanePtr, ConstantInt::get(i32Ty, i));
      }
      mapVectorValue(accessedPtr, vecPtr);
    }
    alignment = instrShape.getAlignmentGeneral();
  }

  // take greatest available alignment
  alignment = std::max<uint>(origAlignment, alignment);

  Value *mask = nullptr;
  Value *vecMem = nullptr;
  if (load) {
    if (isInterleaved) {
      ShuffleBuilder maskShuffler(vectorWidth());
      if (needsMask) {
        mask = requestVectorValue(predicate);
        for (unsigned i = 0; needsMask && i < memGroup.size(); ++i) {
          maskShuffler.add(mask);
        }
      }

      assert(sourceAddrs.size() == sources.size() && "too few or too many sources!");

      std::vector<Value *> loads;
      for (unsigned i = 0; i < sources.size(); ++i) {
        Value *sourceLoad = sources[i];

        // need to recompute alignment
        alignment = getShape(*sourceLoad).getAlignmentFirst();
        origAlignment = cast<LoadInst>(sourceLoad)->getAlignment();
        alignment = std::max<uint>(origAlignment, alignment);

        vecPtr = sourceAddrs[i];
        if (needsMask) {
          mask = maskShuffler.shuffleToInterleaved(builder, memGroup.size(), i);
          loads.push_back(builder.CreateMaskedLoad(vecPtr, alignment, mask, nullptr, "interleaved_load"));
        } else {
          Value *interLoad = builder.CreateLoad(vecPtr, "interleaved_load");
          cast<LoadInst>(interLoad)->setAlignment(alignment);
          loads.push_back(interLoad);
        }
      }

      assert(loads.size() == sources.size() && "not enough interleaved loads");

      // create as many shuffles as there are loads
      ShuffleBuilder shuffleBuilder(loads, vectorWidth());
      unsigned stride = static_cast<unsigned>(loads.size());
      for (unsigned i = 0; i < sources.size(); ++i) {
        // start = 0, stride = sources.size
        Value *shuffle = shuffleBuilder.shuffleFromInterleaved(builder, stride, i);
        mapVectorValue(sources[i], shuffle);
      }

      // early return because everything is done
      return;

    } else if (addrShape.isUniform() && needsMask) {
      // create two new basic blocks
      mask = createPTest(requestVectorValue(predicate), false);
      BasicBlock *loadBlock = BasicBlock::Create(vectorizationInfo.getVectorFunction().getContext(), "load_block",
                                                 &vectorizationInfo.getVectorFunction());
      BasicBlock *continueBlock = BasicBlock::Create(vectorizationInfo.getVectorFunction().getContext(), "cont_block",
                                                     &vectorizationInfo.getVectorFunction());
      BasicBlock *origBlock = load->getParent();

      // conditionally branch to both
      builder.CreateCondBr(mask, loadBlock, continueBlock);
      builder.SetInsertPoint(loadBlock);

      vecMem = builder.CreateLoad(vecPtr, "scal_mask_load");
      cast<LoadInst>(vecMem)->setAlignment(alignment);

      builder.CreateBr(continueBlock);
      builder.SetInsertPoint(continueBlock);

      BasicBlock *vecOrigBlock = cast<BasicBlock>(getVectorValue(origBlock, true));
      PHINode *phi = builder.CreatePHI(accessedType, 2, "scal_mask_load_phi");
      phi->addIncoming(vecMem, loadBlock);
      phi->addIncoming(UndefValue::get(accessedType), vecOrigBlock);

      vecMem = phi;
      mapVectorValue(origBlock, loadBlock);
      mapVectorValue(origBlock, continueBlock);

    } else if ((addrShape.isUniform() || addrShape.isContiguous() || byteContiguous) && !needsMask) {
      std::string name = addrShape.isUniform() ? "scal_load" : "vec_load";
      vecMem = builder.CreateLoad(vecPtr, name);
      cast<LoadInst>(vecMem)->setAlignment(alignment);

    } else {

      if (needsMask) mask = requestVectorValue(predicate);
      else mask = builder.CreateVectorSplat(vectorWidth(), ConstantInt::get(i1Ty, 1), "true_mask");

      if (addrShape.isVarying() || (addrShape.isStrided() && !byteContiguous)) {
        if (useScatterGatherIntrinsics) {
          std::vector<Value *> args;
          args.push_back(vecPtr);
          args.push_back(ConstantInt::get(i32Ty, alignment));
          args.push_back(mask);
          args.push_back(UndefValue::get(vecType));
          Module *mod = vectorizationInfo.getMapping().vectorFn->getParent();
          Function *gatherIntr = Intrinsic::getDeclaration(mod, Intrinsic::masked_gather, vecType);
          assert(gatherIntr && "masked gather not found!");
          vecMem = builder.CreateCall(gatherIntr, args, "gather");
        } else
          vecMem = requestCascadeLoad(vecPtr, alignment, mask);

      } else
        vecMem = builder.CreateMaskedLoad(vecPtr, alignment, mask, 0, "masked_vec_load");
    }
  } else {

    Value *mappedStoredVal = addrShape.isUniform() ? requestScalarValue(storedValue)
                                                   : requestVectorValue(storedValue);

    if (isInterleaved) {
      ShuffleBuilder maskShuffler(vectorWidth());
      if (needsMask) {
        mask = requestVectorValue(predicate);
        for (unsigned i = 0; needsMask && i < memGroup.size(); ++i) {
          maskShuffler.add(mask);
        }
      }

      assert(sourceAddrs.size() == sources.size() && "too few or too many sources!");

      // shuffle the source values
      ShuffleBuilder shuffleBuilder(vectorWidth());
      unsigned stride = static_cast<unsigned>(sources.size());
      for (unsigned i = 0; i < sources.size(); ++i) {
        Value *sourceStore = sources[i];
        storedValue = cast<StoreInst>(sourceStore)->getValueOperand();
        mappedStoredVal = requestVectorValue(storedValue);
        shuffleBuilder.add(mappedStoredVal);
      }

      std::vector<Value *> valShuffles;
      for (unsigned i = 0; i < sources.size(); ++i) {
        // start = 0, stride = sources.size
        valShuffles.push_back(shuffleBuilder.shuffleToInterleaved(builder, stride, i));
      }

      for (unsigned i = 0; i < sources.size(); ++i) {
        Value *sourceStore = sources[i];

        // need to recompute alignment
        alignment = getShape(*sourceStore).getAlignmentFirst();
        origAlignment = cast<StoreInst>(sourceStore)->getAlignment();
        alignment = std::max<uint>(origAlignment, alignment);

        vecPtr = sourceAddrs[i];
        mappedStoredVal = valShuffles[i];
        if (needsMask) {
          mask = maskShuffler.shuffleToInterleaved(builder, stride, i);
          vecMem = builder.CreateMaskedStore(mappedStoredVal, vecPtr, alignment, mask);
        } else {
          vecMem = builder.CreateStore(mappedStoredVal, vecPtr);
          cast<StoreInst>(vecMem)->setAlignment(alignment);
        }

        mapVectorValue(sourceStore, vecMem);
      }

      // early return because everything is done
      return;

    } else if (addrShape.isUniform() && needsMask) {
      // create two new basic blocks
      mask = createPTest(requestVectorValue(predicate), false);
      BasicBlock *storeBlock = BasicBlock::Create(vectorizationInfo.getVectorFunction().getContext(), "store_block",
                                                  &vectorizationInfo.getVectorFunction());
      BasicBlock *continueBlock = BasicBlock::Create(vectorizationInfo.getVectorFunction().getContext(), "cont_block",
                                                     &vectorizationInfo.getVectorFunction());
      BasicBlock *origBlock = store->getParent();

      // conditionally branch to both
      builder.CreateCondBr(mask, storeBlock, continueBlock);
      builder.SetInsertPoint(storeBlock);

      vecMem = builder.CreateStore(mappedStoredVal, vecPtr);
      cast<StoreInst>(vecMem)->setAlignment(alignment);

      builder.CreateBr(continueBlock);
      builder.SetInsertPoint(continueBlock);

      mapVectorValue(origBlock, storeBlock);
      mapVectorValue(origBlock, continueBlock);

    } else if ((addrShape.isUniform() || addrShape.isContiguous() || byteContiguous) && !needsMask) {
      vecMem = builder.CreateStore(mappedStoredVal, vecPtr);
      cast<StoreInst>(vecMem)->setAlignment(alignment);

    } else {
      if (needsMask) mask = requestVectorValue(predicate);
      else mask = builder.CreateVectorSplat(vectorWidth(), ConstantInt::get(i1Ty, 1), "true_mask");

      if (addrShape.isVarying() || (addrShape.isStrided() && !byteContiguous)) {
        if (useScatterGatherIntrinsics) {
          std::vector<Value *> args;
          args.push_back(mappedStoredVal);
          args.push_back(vecPtr);
          args.push_back(ConstantInt::get(i32Ty, alignment));
          args.push_back(mask);
          Module *mod = vectorizationInfo.getMapping().vectorFn->getParent();
          Function *scatterIntr = Intrinsic::getDeclaration(mod, Intrinsic::masked_scatter, vecType);
          assert(scatterIntr && "masked scatter not found!");
          vecMem = builder.CreateCall(scatterIntr, args);
        } else
          vecMem = requestCascadeStore(mappedStoredVal, vecPtr, alignment, mask);

      } else
        vecMem = builder.CreateMaskedStore(mappedStoredVal, vecPtr, alignment, mask);
    }
  }

  if (addrShape.isUniform())
    mapScalarValue(inst, vecMem);
  else
    mapVectorValue(inst, vecMem);
}

void NatBuilder::requestLazyInstructions(Instruction *const upToInstruction) {
  assert(!lazyInstructions.empty() && "no lazy instructions to generate!");

  Instruction *lazyInstr = lazyInstructions.front();
  lazyInstructions.pop_front();

  while (lazyInstr != upToInstruction) {
    // skip if already generated (only happens for interleaving)
    if (getVectorValue(lazyInstr))
      continue;

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

Value *NatBuilder::requestVectorValue(Value *const value) {
  if (isa<Instruction>(value)) {
    Instruction *lazyMemInstr = cast<Instruction>(value);
    if (std::find(lazyInstructions.begin(), lazyInstructions.end(), lazyMemInstr) != lazyInstructions.end())
      requestLazyInstructions(lazyMemInstr);
  }

  Value *vecValue = getVectorValue(value);
  if (!vecValue) {
    vecValue = getScalarValue(value);
    // check shape for value. if there is one and it is contiguous, cast to vector and add <0,1,2,...,n-1>
    VectorShape shape = getShape(*value);

    Instruction *vecInst = dyn_cast<Instruction>(vecValue);
    auto oldIP = builder.GetInsertPoint();
    auto oldIB = builder.GetInsertBlock();

    if (vecInst) {
      if (vecInst->getParent()->getTerminator())
        builder.SetInsertPoint(vecInst->getParent()->getTerminator());
      else
        builder.SetInsertPoint(vecInst->getParent());

    } else {
      // insert in header
      auto * oldInsertBlock = builder.GetInsertBlock();
      auto * insertFunc = oldInsertBlock->getParent();
      BasicBlock & entryBlock = insertFunc->getEntryBlock();
      if (oldInsertBlock != &entryBlock) {
        builder.SetInsertPoint(entryBlock.getTerminator());
      }
    }

    // create a vector GEP to widen pointers
    if (value->getType()->isPointerTy()) {
      auto * scalarPtrTy = vecValue->getType();
      auto * intTy = builder.getInt32Ty();
      auto * ptrElemTy = scalarPtrTy->getPointerElementType();
      int scalarBytes = static_cast<int>(layout.getTypeStoreSize(ptrElemTy));

      Value *contVec = createContiguousVector(vectorWidth(), intTy, 0, shape.getStride() / scalarBytes);
      vecValue = builder.CreateGEP(vecValue, contVec, "widen_ptr");

    } else {
      vecValue = builder.CreateVectorSplat(vectorWidth(), vecValue);
      assert(value->getType()->isIntegerTy() || value->getType()->isFloatingPointTy());

      if (shape.isContiguous() || shape.isStrided()) {
        auto *laneTy = vecValue->getType()->getVectorElementType();
        Value *contVec = createContiguousVector(vectorWidth(), laneTy, 0, shape.getStride());
        vecValue = laneTy->isFloatingPointTy() ? builder.CreateFAdd(vecValue, contVec, "contiguous_add")
                                               : builder.CreateAdd(vecValue, contVec, "contiguous_add");
      }
    }

    // if (vecInst)
      builder.SetInsertPoint(oldIB, oldIP);

    mapVectorValue(value, vecValue);
  }
  return vecValue;
}

Value *NatBuilder::requestScalarValue(Value *const value, unsigned laneIdx, bool skipMappingWhenDone) {
  if (isa<Instruction>(value)) {
    Instruction *lazyMemInstr = cast<Instruction>(value);
    if (std::find(lazyInstructions.begin(), lazyInstructions.end(), lazyMemInstr) != lazyInstructions.end())
      requestLazyInstructions(lazyMemInstr);
  }

  Value *mappedVal = getScalarValue(value, laneIdx);
  if (mappedVal) return mappedVal;

  // if value is integer or floating type, contiguous and has value for lane 0, add laneIdx
  Value *reqVal = nullptr;

  // if value has a vector mapping -> extract from vector. if not -> clone scalar op
  if (!reqVal) {
    mappedVal = getVectorValue(value);
    if (mappedVal) {
      // to avoid dominance problems assume: if we only have a vectorized value and need a scalar one -> do not map!
      skipMappingWhenDone = true;
      Instruction *mappedInst = dyn_cast<Instruction>(mappedVal);
      auto oldIP = builder.GetInsertPoint();
      auto oldIB = builder.GetInsertBlock();
      if (mappedInst) {
        if (mappedInst->getParent()->getTerminator())
          builder.SetInsertPoint(mappedInst->getParent()->getTerminator());
        else
          builder.SetInsertPoint(mappedInst->getParent());
      }
      IF_DEBUG {
        errs() << "Extracting a scalar value from a vector:\n";
        errs() << "Original Value: ";
        value->dump();
        errs() << "Vector Value: ";
        mappedVal->dump();
      };

      // if the mappedVal is a alloca instruction, create a GEP instruction
      if (isa<AllocaInst>(mappedVal))
        reqVal = builder.CreateGEP(mappedVal, ConstantInt::get(i32Ty, laneIdx));
      else {
        // extract from GEPs are not allowed. in that case recreate the scalar instruction and get that new value
        if (isa<GetElementPtrInst>(mappedVal) && isa<GetElementPtrInst>(value)) {
          reqVal = vectorizeGEPInstruction(cast<GetElementPtrInst>(value), false, laneIdx, true);
          mapScalarValue(value, reqVal, laneIdx);
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
    } else {
      if (vectorizationInfo.hasKnownShape(*value)) {
        VectorShape shape = getShape(*value);
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

      if (!reqVal) {
        Instruction *inst = cast<Instruction>(value);
        Instruction *mapInst;
        reqVal = mapInst = inst->clone();
        mapOperandsInto(inst, mapInst, false);
        builder.Insert(mapInst, inst->getName());
      }
    }
  }

  // only map if normal request. fresh requests will not get mapped
  if (!skipMappingWhenDone) mapScalarValue(value, reqVal, laneIdx);
  return reqVal;
}

Value *NatBuilder::requestCascadeLoad(Value *vecPtr, unsigned alignment, Value *mask) {
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
  Argument *ptrArg = &*func->getArgumentList().begin();
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
  auto argIt = func->getArgumentList().begin();
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


  Module *mod = vectorizationInfo.getScalarFunction().getParent();
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

  auto argIt = func->getArgumentList().begin();
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
  for (unsigned i = 0; i < vectorWidth(); ++i) {
    BasicBlock *cond = condBlocks[i];
    BasicBlock *masked = loadBlocks[i];
    BasicBlock *nextBlock = i == vectorWidth() - 1 ? ret : condBlocks[i + 1];

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

void NatBuilder::mapCascadeFunction(unsigned bitWidth, llvm::Function *function, bool store) {
  if (store) cascadeStoreMap[bitWidth] = function;
  else cascadeLoadMap[bitWidth] = function;
}

llvm::Function *NatBuilder::getCascadeFunction(unsigned bitWidth, bool store) {
  auto mapIt = store ? cascadeStoreMap.find(bitWidth) : cascadeLoadMap.find(bitWidth);
  auto mapEt = store ? cascadeStoreMap.end() : cascadeLoadMap.end();
  if (mapIt != mapEt) return mapIt->second;
  return nullptr;
}

llvm::Value *NatBuilder::createPTest(llvm::Value *vector, bool isRv_all) {
  assert(vector->getType()->isVectorTy() && "given value is no vector type!");
  assert(cast<VectorType>(vector->getType())->getElementType()->isIntegerTy(1) &&
         "vector elements must have i1 type!");

  // rv_all(x) == !rv_any(!x)
  Type *i32VecType = VectorType::get(i32Ty, vectorWidth());
  Type *intSIMDType = Type::getIntNTy(vector->getContext(), vectorWidth() * 32);
  Constant *simdFalseConst = ConstantInt::get(vector->getContext(), APInt::getMinValue(vectorWidth() * 32));
  if (isRv_all)
    vector = builder.CreateNot(vector, "rvall_cond_not");
  Value *zext = builder.CreateZExt(vector, i32VecType, "ptest_zext");
  Value *bc = builder.CreateBitCast(zext, intSIMDType, "ptest_bc");
  Value *ptest = builder.CreateICmpNE(bc, simdFalseConst, "ptest_comp");

  if (isRv_all)
    ptest = builder.CreateNot(ptest, "rvall_not");

  return ptest;
}

llvm::Value *NatBuilder::maskInactiveLanes(llvm::Value *const value, const BasicBlock* const block, bool invert) {
    auto pred = requestVectorValue(vectorizationInfo.getPredicate(*block));
    if (invert) {
        return builder.CreateOr(value, builder.CreateNot(pred));
    } else {
        return builder.CreateAnd(value, pred);
    }
}

Value&
NatBuilder::materializeVectorReduce(IRBuilder<> & builder, Value & initVal, Value & vecVal, Instruction & reductOp) {
  Value * accu = &initVal;
  for (int i = 0; i < vectorizationInfo.getVectorWidth(); ++i) {
    auto * laneVal = builder.CreateExtractElement(&vecVal, i, "red_ext");

    Instruction * copy = reductOp.clone();
    copy->setOperand(0, accu);
    copy->setOperand(1, laneVal);
    builder.Insert(copy, "red");
    accu = copy;
  }

  return *accu;
}

void
NatBuilder::materializeReduction(Reduction & red) {
  const int vectorWidth = vectorizationInfo.getVectorWidth();
  auto * vecPhi = cast<PHINode>(getVectorValue(&red.phi));

// infer (mapped) initial value
  auto & scalInitVal = red.getInitValue();
  auto & phiInitVal = scalInitVal; // FIXME only valid in input-out-of-region case

// construct new (vectorized) initial value
  Value * vecNeutral = ConstantVector::getSplat(vectorWidth, &red.neutralElem);

  BasicBlock * vecInitInputBlock = red.phi.getIncomingBlock(red.initInputIndex);
  BasicBlock * vecLoopInputBlock = cast<BasicBlock>(getVectorValue(red.phi.getIncomingBlock(red.loopInputIndex)));

// attach inputs (neutral elem and reduction inst)
  vecPhi->addIncoming(vecNeutral, vecInitInputBlock);
  vecPhi->addIncoming(getVectorValue(&red.getReductInst()), vecLoopInputBlock);

  auto & reductInst = red.getReductInst();
  auto & vecReductInst = *cast<Instruction>(getVectorValue(&reductInst));

// reduce reduction phi for outside users
  for (auto & use : red.phi.uses()) {
    int opIdx = use.getOperandNo();
    auto & userInst = cast<Instruction>(*use.getUser());

    if (vectorizationInfo.inRegion(userInst)) {
      continue; // regular remapping
    }

    auto * userPhi = dyn_cast<PHINode>(&userInst);
    assert((!userPhi || (userPhi->getNumIncomingValues() == 1)) && "expected an LCSSA phi");

    // otw, replace with reduced value
    IRBuilder<> builder(userInst.getParent(), userInst.getIterator());
    auto & reducedVector = materializeVectorReduce(builder, phiInitVal, *vecPhi, reductInst);

    if (userPhi) {
      // LCSSA phi (purge)
      userPhi->replaceAllUsesWith(&reducedVector);
      userPhi->eraseFromParent();

    } else {
      // regular use
      userInst.setOperand(opIdx, &reducedVector);
    }
  }

// reduct result of reduction operation for outside users
  for (auto & use : reductInst.uses()) {
    int opIdx = use.getOperandNo();
    auto & userInst = cast<Instruction>(*use.getUser());

    if (vectorizationInfo.inRegion(userInst)) {
      continue; // regular remapping
    }

    auto * userPhi = dyn_cast<PHINode>(&userInst);
    assert((!userPhi || (userPhi->getNumIncomingValues() == 1)) && "expected an LCSSA phi");

    // otw, replace with reduced value
    IRBuilder<> builder(userInst.getParent(), userInst.getIterator());
    auto & reducedVector = materializeVectorReduce(builder, phiInitVal, vecReductInst, reductInst);

    if (userPhi) {
      // LCSSA phi (purge)
      userPhi->replaceAllUsesWith(&reducedVector);
      userPhi->eraseFromParent();

    } else {
      // regular use
      userInst.setOperand(opIdx, &reducedVector);
    }
  }
}

void NatBuilder::addValuesToPHINodes() {
  // save current insertion point before continuing
//  auto IB = builder.GetInsertBlock();
//  auto IP = builder.GetInsertPoint();

  for (PHINode *scalPhi : phiVector) {
    assert(vectorizationInfo.hasKnownShape(*scalPhi) && "no VectorShape for PHINode available!");
    VectorShape shape = getShape(*scalPhi);
    Type *scalType = scalPhi->getType();

    // replicate phi <vector_width> times if type is not vectorizable
    bool replicate = shape.isVarying() && (scalType->isVectorTy() || scalType->isStructTy());
    unsigned loopEnd = replicate ? vectorWidth() : 1;

    auto *red = reda.getReductionInfo(*scalPhi);

    bool isVectorLoopHeader = region && &region->getRegionEntry() == scalPhi->getParent();
    if (isVectorLoopHeader && shape.isVarying() && red) {
      // reduction phi handling
      IF_DEBUG_NAT { errs() << "-- materializing "; red->dump(); errs() << "\n"; }
      materializeReduction(*red);

    } else {
      // default phi handling
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
    if (region && !region->contains(cast<BasicBlock>(value))) {
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
  // in case of regions, keep any values that are live into the region
  if (region && isa<Argument>(value)) {
    return value;
  } else if (region && isa<Instruction>(value) && !region->contains(cast<Instruction>(value)->getParent())) {
    return value;
  }

  const Constant *constant = dyn_cast<const Constant>(value);
  if (constant) return const_cast<Constant *>(constant);

  auto scalarIt = scalarValueMap.find(value);
  if (scalarIt != scalarValueMap.end()) {
    VectorShape shape;
    if (vectorizationInfo.hasKnownShape(*value)) {
      shape = getShape(*value);
      if (shape.isUniform()) laneIdx = 0;
    }

    LaneValueVector &laneValues = scalarIt->second;
    if (laneValues.size() > laneIdx) return laneValues[laneIdx];
    else return nullptr;
  } else return nullptr;
}

BasicBlockVector &NatBuilder::getAllBasicBlocksFor(llvm::BasicBlock *basicBlock) {
  auto blockIt = basicBlockMap.find(basicBlock);
  assert(blockIt != basicBlockMap.end() && "blocks must already exist for basicBlock!");
  return blockIt->second;
}

unsigned NatBuilder::vectorWidth() {
  return vectorizationInfo.getMapping().vectorWidth;
}

bool NatBuilder::canVectorize(Instruction *const inst) {
  // whitelisting approach. for direct vectorization we support:
  // binary operations (normal & bitwise), memory access operations, conversion operations and other operations

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
  if (!instTy->isVoidTy() && !instTy->isIntegerTy() && !instTy->isFloatingPointTy()) return false;

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
  // 1) varying vector shape OR
  // 2) Alloca AND contiguous
  // 3) no vector shape && one or more operands varying
  // 4) GEP that is strided or varying
  // EXCEPTION: GEP with vector-pointer base
  // 5) return instruction and function return-type is vector type
  // 6) has an operand that will be vectorized

  if (isa<GetElementPtrInst>(inst)) {
    GetElementPtrInst *gep = cast<GetElementPtrInst>(inst);
    Value *pointer = gep->getPointerOperand();
    Value *mappedPtr = getScalarValue(pointer);
    if (mappedPtr) {
      PointerType *pty = cast<PointerType>(mappedPtr->getType());
      if (pty->getElementType()->isVectorTy()) {
        willNotVectorize.push_back(inst);
        return false;
      }
    }
    VectorShape shape = getShape(*gep);
    if (shape.isStrided(gep->getResultElementType()->getPrimitiveSizeInBits() / 8)) {
      willNotVectorize.push_back(inst);
      return false;
    } else if (shape.isStrided() || shape.isVarying())
      return true;
  }

  if (isa<ReturnInst>(inst)) {
    Function &func = vectorizationInfo.getVectorFunction();
    if (func.getReturnType()->isVectorTy()) {
      IF_DEBUG {
        if (getShape(*inst).isUniform()) {
          errs() << "Warning: Uniform return in Function with Vector Type!\n";
          inst->dump();
        }
      };
      return true; // THIS SHOULD NEVER HAPPEN!
    }
  }

  if (vectorizationInfo.hasKnownShape(*inst)) {
    VectorShape shape = getShape(*inst);
    if (isa<AllocaInst>(inst) || isa<LoadInst>(inst) ? !shape.isUniform() : shape.isVarying())
      return true;
    else if (shape.isUniform()) {
      willNotVectorize.push_back(inst);
      return false;
    }

  }

  for (unsigned i = 0; i < inst->getNumOperands(); ++i) {
    // operands are either constants or have shapes
    Value *val = inst->getOperand(i);
    assert((isa<Constant>(val) || vectorizationInfo.hasKnownShape(*val)) &&
           "expected either a constant or a known shape!");
    if (isa<Constant>(val)) continue;
    else {
      VectorShape shape = getShape(*val);
      if (shape.isVarying() || (isa<StoreInst>(inst) && !shape.isUniform())) return true;
    }

    // if we already checked this instruction, return the last value. by construction, all operands that are
    // instructions will have been checked already. therefore we do not need to save the concrete <true/false> value
    // and instead will insert into a vector if we should not vectorize and then check if this instruction is inside
    if (isa<Instruction>(val) && std::find(willNotVectorize.begin(), willNotVectorize.end(), cast<Instruction>(val)) == willNotVectorize.end())
      return true;
  }
  // all operands uniform, should not be vectorized
  willNotVectorize.push_back(inst);
  return false;
}
