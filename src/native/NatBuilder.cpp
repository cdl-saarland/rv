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


#include "rvConfig.h"

using namespace native;
using namespace llvm;
using namespace rv;

NatBuilder::NatBuilder(PlatformInfo &platformInfo, VectorizationInfo &vectorizationInfo,
                       const DominatorTree &dominatorTree) :
    builder(vectorizationInfo.getMapping().vectorFn->getContext()),
    platformInfo(platformInfo),
    vectorizationInfo(vectorizationInfo),
    dominatorTree(dominatorTree),
    layout(vectorizationInfo.getScalarFunction().getParent()),
    i1Ty(IntegerType::get(vectorizationInfo.getMapping().vectorFn->getContext(), 1)),
    i32Ty(IntegerType::get(vectorizationInfo.getMapping().vectorFn->getContext(), 32)),
    region(vectorizationInfo.getRegion()),
    useScatterGatherIntrinsics(false),
    cascadeLoadMap(),
    cascadeStoreMap(),
    vectorValueMap(),
    scalarValueMap(),
    basicBlockMap(),
    phiVector(),
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

#if 0
  // traverse func in reverse-post order to ensure all uses have definition vectorized first
  ReversePostOrderTraversal<Function *> RPOT(func);
  for (ReversePostOrderTraversal<Function *>::rpo_iterator it = RPOT.begin(); it != RPOT.end(); ++it) {
      BasicBlock *bb = *it;
      BasicBlock *vecBlock = cast<BasicBlock>(getVectorValue(bb));
      vectorize(bb, vecBlock);
  }
#endif
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
    if (load || store)
//      vectorizeMemoryInstruction(inst);
      lazyInstructions.push_back(inst);
    else if (call) {
      // calls need special treatment
      if (call->getCalledFunction()->getName() == "rv_any")
        vectorizeReductionCall(call, false);
      else if (call->getCalledFunction()->getName() == "rv_all")
        vectorizeReductionCall(call, true);
      else
        lazyInstructions.push_back(inst);
//      else if (shouldVectorize(call))
//        vectorizeCallInstruction(call);
//      else {
//        copyCallInstruction(call);
//      }
    } else if (phi)
      // phis need special treatment as they might contain not-yet mapped instructions
      vectorizePHIInstruction(phi);
    else if (alloca && shouldVectorize(inst)) {
      // note: this is ONLY allowed IFF
      // (1) no calls that have alloca instructions as arguments OR
      // (2) there exists a function mapping which allows that. e.g.: float * -> <4 x float> *
      if (canVectorize(inst))
        vectorize(inst);
      else
        for (unsigned lane = 0; lane < vectorWidth(); ++lane) {
          copyInstruction(inst, lane);
        }
    } else if (gep) {
      unsigned laneEnd = shouldVectorize(gep) ? vectorWidth() : 1;
      for (unsigned lane = 0; lane < laneEnd; ++lane) {
        copyGEPInstruction(gep, lane, laneEnd == vectorWidth());
      }
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

void NatBuilder::vectorizePHIInstruction(PHINode *const scalPhi) {
  assert(vectorizationInfo.hasKnownShape(*scalPhi) && "no VectorShape for PHINode available!");
  VectorShape shape = vectorizationInfo.getVectorShape(*scalPhi);
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

void NatBuilder::copyGEPInstruction(GetElementPtrInst *const gep, unsigned laneIdx, bool vectorizeOperands) {
  assert(gep->getNumOperands() - 1 == gep->getNumIndices() && "LLVM Code for GEP changed!");
  Value *ptr = requestScalarValue(gep->getPointerOperand(), laneIdx);
  Value **idxList = new Value *[gep->getNumIndices()];
  for (unsigned i = 0; i < gep->getNumIndices(); ++i) {
    // vectorize operands once
    Value *operand = gep->getOperand(i + 1);
    if (laneIdx == 0 && vectorizeOperands)
      requestVectorValue(operand);
    idxList[i] = requestScalarValue(operand, laneIdx);
  }
  GetElementPtrInst *cgep = cast<GetElementPtrInst>(
      builder.CreateGEP(ptr, ArrayRef<Value *>(idxList, gep->getNumIndices()), gep->getName()));
  cgep->setIsInBounds(gep->isInBounds());
  mapScalarValue(gep, cgep, laneIdx);
  delete [] idxList;
}

/* expects that builder has valid insertion point set */
void NatBuilder::copyInstruction(Instruction *const inst, unsigned laneIdx) {
  assert(inst && "no instruction to copy");
  assert(builder.GetInsertBlock() && "no insertion point set");
  Instruction *cpInst = inst->clone();
  BranchInst *branch = dyn_cast<BranchInst>(cpInst);
  if (branch && vectorizationInfo.hasKnownShape(*inst))
    assert(vectorizationInfo.getVectorShape(*inst).isUniform() && "branch not uniform");
  if (branch && branch->isConditional()) {
    Value *cond = branch->getCondition();
    VectorShape shape = vectorizationInfo.hasKnownShape(*cond) ? vectorizationInfo.getVectorShape(*cond)
                                                               : VectorShape::uni();
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
  Value *resVec = type->isVoidTy() || type->isVectorTy() || type->isStructTy() ? nullptr : UndefValue::get(
      getVectorType(inst->getType(), vectorWidth()));
  for (unsigned lane = 0; lane < vectorWidth(); ++lane) {
    Instruction *cpInst = inst->clone();
    mapOperandsInto(inst, cpInst, false, lane);
    builder.Insert(cpInst, inst->getName());
    if (type->isStructTy() || type->isVectorTy()) mapScalarValue(inst, cpInst, lane);
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
  assert(vectorizationInfo.hasKnownShape(*predicate) && "predicate has no shape");
  const VectorShape &shape = vectorizationInfo.getVectorShape(*predicate);
  assert((shape.isVarying() || shape.isUniform()) && "predicate can't be contigious or strided");

  Value *reduction;
  if (shape.isVarying()) {
    Value *vecPredicate = requestVectorValue(predicate);
    reduction = createPTest(vecPredicate, isRv_all);
  } else {
    reduction = requestScalarValue(predicate);
  }

  mapScalarValue(rvCall, reduction);
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
      Value *call = builder.CreateCall(callee, args, scalCall->getName() + suffix);
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

  assert(vectorizationInfo.hasKnownShape(*accessedPtr) && "no shape for accessed pointer!");
  VectorShape addrShape = vectorizationInfo.getVectorShape(*accessedPtr);

  // address: uniform -> scalar op. contiguous -> scalar from vector-width address. varying -> scatter/gather
  Value *vecPtr = nullptr;

  Value *predicate = vectorizationInfo.getPredicate(*inst->getParent());
  assert(predicate && predicate->getType()->isIntegerTy(1) && "predicate must have i1 type!");
  bool needsMask = !isa<Constant>(predicate);

  Type *vecType = getVectorType(accessedType, addrShape.isUniform() ? 1 : vectorWidth());

#if 0
  uint scalarBytes = accessedType->getPrimitiveSizeInBits() / 8;

  if (addrShape.hasStridedShape(scalarBytes)) {
    if (accessedType->isPointerTy())
      vecPtr = requestScalarValue(accessedPtr);
    else {
      // cast pointer to vector-width pointer
      Value *mappedPtr = requestScalarValue(accessedPtr);
      PointerType *vecPtrType = PointerType::getUnqual(vecType);
      vecPtr = builder.CreatePointerCast(mappedPtr, vecPtrType, "vec_cast");
    }
  } else
#endif
  bool byteContiguous = addrShape.isStrided(static_cast<int>(layout.getTypeStoreSize(accessedType)));
  if (addrShape.isContiguous() || byteContiguous) {
    // cast pointer to vector-width pointer
    // uniform-with-mask case needs to be included as scalar masked load/store is not allowed!
    Value *mappedPtr = requestScalarValue(accessedPtr);
    PointerType *vecPtrType = PointerType::getUnqual(vecType);
    vecPtr = builder.CreatePointerCast(mappedPtr, vecPtrType, "vec_cast");
  } else if (addrShape.isUniform()) {
    vecPtr = requestScalarValue(accessedPtr);
  } else {
    // varying or strided. gather the addresses for the lanes
    vecPtr = isa<Argument>(accessedPtr) ? getScalarValue(accessedPtr) : getVectorValue(accessedPtr);
    if (!vecPtr) {
      vecPtr = UndefValue::get(getVectorType(accessedPtr->getType(), vectorWidth()));
      for (unsigned i = 0; i < vectorWidth(); ++i) {
        Value *lanePtr = requestScalarValue(accessedPtr, i);
        vecPtr = builder.CreateInsertElement(vecPtr, lanePtr, ConstantInt::get(i32Ty, i));
      }
      mapVectorValue(accessedPtr, vecPtr);
    }
  }

  Value *mask = nullptr;
  Value *vecMem = nullptr;
  if (load) {
    if (addrShape.isUniform() && needsMask) {
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
      cast<LoadInst>(vecMem)->setAlignment(load->getAlignment());

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
      cast<LoadInst>(vecMem)->setAlignment(load->getAlignment());

    } else {

      if (needsMask) mask = requestVectorValue(predicate);
      else mask = builder.CreateVectorSplat(vectorWidth(), ConstantInt::get(i1Ty, 1), "true_mask");

      if (addrShape.isVarying() || (addrShape.isStrided() && !byteContiguous)) {
        if (useScatterGatherIntrinsics) {
          std::vector<Value *> args;
          args.push_back(vecPtr);
          args.push_back(ConstantInt::get(i32Ty, load->getAlignment()));
          args.push_back(mask);
          args.push_back(UndefValue::get(vecType));
          Module *mod = vectorizationInfo.getMapping().vectorFn->getParent();
          Function *gatherIntr = Intrinsic::getDeclaration(mod, Intrinsic::masked_gather, vecType);
          assert(gatherIntr && "masked gather not found!");
          vecMem = builder.CreateCall(gatherIntr, args, "gather");
        } else
          vecMem = requestCascadeLoad(vecPtr, load->getAlignment(), mask);

      } else
        vecMem = builder.CreateMaskedLoad(vecPtr, load->getAlignment(), mask, 0, "masked_vec_load");
    }
  } else {

    Value *mappedStoredVal = addrShape.isUniform() ? requestScalarValue(storedValue)
                                                   : requestVectorValue(storedValue);

    if (addrShape.isUniform() && needsMask) {
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
      cast<StoreInst>(vecMem)->setAlignment(store->getAlignment());

      builder.CreateBr(continueBlock);
      builder.SetInsertPoint(continueBlock);

      mapVectorValue(origBlock, storeBlock);
      mapVectorValue(origBlock, continueBlock);

    } else if ((addrShape.isUniform() || addrShape.isContiguous() || byteContiguous) && !needsMask) {
      vecMem = builder.CreateStore(mappedStoredVal, vecPtr);
      cast<StoreInst>(vecMem)->setAlignment(store->getAlignment());

    } else {
      if (needsMask) mask = requestVectorValue(predicate);
      else mask = builder.CreateVectorSplat(vectorWidth(), ConstantInt::get(i1Ty, 1), "true_mask");

      if (addrShape.isVarying() || (addrShape.isStrided() && !byteContiguous)) {
        if (useScatterGatherIntrinsics) {
          std::vector<Value *> args;
          args.push_back(mappedStoredVal);
          args.push_back(vecPtr);
          args.push_back(ConstantInt::get(i32Ty, store->getAlignment()));
          args.push_back(mask);
          Module *mod = vectorizationInfo.getMapping().vectorFn->getParent();
          Function *scatterIntr = Intrinsic::getDeclaration(mod, Intrinsic::masked_scatter, vecType);
          assert(scatterIntr && "masked scatter not found!");
          vecMem = builder.CreateCall(scatterIntr, args);
        } else
          vecMem = requestCascadeStore(mappedStoredVal, vecPtr, store->getAlignment(), mask);

      } else
        vecMem = builder.CreateMaskedStore(mappedStoredVal, vecPtr, store->getAlignment(), mask);
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
    if (isa<CallInst>(lazyInstr)) {
      if (shouldVectorize(lazyInstr))
        vectorizeCallInstruction(cast<CallInst>(lazyInstr));
      else
        copyCallInstruction(cast<CallInst>(lazyInstr));
    } else
      vectorizeMemoryInstruction(lazyInstr);

    assert(!lazyInstructions.empty() && "no more lazy instructions left to generate!");

    lazyInstr = lazyInstructions.front();
    lazyInstructions.pop_front();
  }

  assert(lazyInstr == upToInstruction && "something went wrong during lazy generation!");

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
    VectorShape shape = vectorizationInfo.hasKnownShape(*value) ? vectorizationInfo.getVectorShape(*value)
                                                                : VectorShape::uni();

    Instruction *vecInst = dyn_cast<Instruction>(vecValue);
    auto oldIP = builder.GetInsertPoint();
    auto oldIB = builder.GetInsertBlock();
    if (vecInst) {
      if (vecInst->getParent()->getTerminator())
        builder.SetInsertPoint(vecInst->getParent()->getTerminator());
      else
        builder.SetInsertPoint(vecInst->getParent());
    }

    vecValue = builder.CreateVectorSplat(vectorWidth(), vecValue);
    if (shape.isContiguous() || shape.isStrided()) {
      auto *laneTy = vecValue->getType()->getVectorElementType();
      Value *contVec = createContiguousVector(vectorWidth(), laneTy, 0, shape.getStride());
      vecValue = laneTy->isFloatingPointTy() ? builder.CreateFAdd(vecValue, contVec, "contiguous_add")
                                             : builder.CreateAdd(vecValue, contVec, "contiguous_add");
    }

    if (vecInst) builder.SetInsertPoint(oldIB, oldIP);

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

      reqVal = builder.CreateExtractElement(mappedVal, ConstantInt::get(i32Ty, laneIdx), "extract");

      if (reqVal->getType() != value->getType()) {
        reqVal = builder.CreateBitCast(reqVal, value->getType(), "bc");
      }

      if (mappedInst)
        builder.SetInsertPoint(oldIB, oldIP);
    } else {
      if (vectorizationInfo.hasKnownShape(*value)) {
        VectorShape shape = vectorizationInfo.getVectorShape(*value);
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

  Type *i32VecType = VectorType::get(i32Ty, vectorWidth());
  Type *intSIMDType = Type::getIntNTy(vector->getContext(), vectorWidth() * 32);
  Constant *simdFalseConst = ConstantInt::get(vector->getContext(), APInt::getMinValue(vectorWidth() * 32));
  Constant *simdTrueConst = ConstantInt::get(i32VecType, 1);
  Value *sext = builder.CreateSExt(vector, i32VecType, "ptest_sext");
  Value *bc = builder.CreateBitCast(sext, intSIMDType, "ptest_bc");

  if (isRv_all) {
    Value *trueConst = builder.CreateBitCast(simdTrueConst, intSIMDType);
    return builder.CreateICmpEQ(bc, trueConst, "ptest_comp");
  } else
    return builder.CreateICmpNE(bc, simdFalseConst, "ptest_comp");
}

void NatBuilder::addValuesToPHINodes() {
  // save current insertion point before continuing
//  auto IB = builder.GetInsertBlock();
//  auto IP = builder.GetInsertPoint();

  for (PHINode *scalPhi : phiVector) {
    assert(vectorizationInfo.hasKnownShape(*scalPhi) && "no VectorShape for PHINode available!");
    VectorShape shape = vectorizationInfo.getVectorShape(*scalPhi);
    Type *scalType = scalPhi->getType();

    // replicate phi <vector_width> times if type is not vectorizable
    bool replicate = shape.isVarying() && (scalType->isVectorTy() || scalType->isStructTy());
    unsigned loopEnd = replicate ? vectorWidth() : 1;

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
      shape = vectorizationInfo.getVectorShape(*value);
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
#if 0
  if (isa<CallInst>(inst))
    return !inst->getType()->isVectorTy() && !inst->getType()->isStructTy();
  return !(isa<TerminatorInst>(inst) && !isa<ReturnInst>(inst));
#endif

  // whitelisting approach. for direct vectorization we support:
  // binary operations (normal & bitwise), memory access operations, conversion operations and other operations

  // for AllocaInst: vectorize if not used in calls. replicate else
  if (isa<AllocaInst>(inst)) {
    for (auto user : inst->users()) {
      if (isa<CallInst>(user) || isa<InvokeInst>(user))
        return false;
    }
    return true;
  } else
    return isSupportedOperation(inst);
}

bool NatBuilder::shouldVectorize(Instruction *inst) {
  // we should vectorize iff
  // 1) varying vector shape OR
  // 2) Alloca AND contiguous
  // 3) no vector shape && one or more operands varying
  // 4) GEP that is strided or varying
  // EXCEPTION: GEP with vector-pointer base

  if (isa<GetElementPtrInst>(inst)) {
    GetElementPtrInst *gep = cast<GetElementPtrInst>(inst);
    Value *pointer = gep->getPointerOperand();
    Value *mappedPtr = getScalarValue(pointer);
    if (mappedPtr) {
      PointerType *pty = cast<PointerType>(mappedPtr->getType());
      if (pty->getElementType()->isVectorTy())
        return false;
    }
    VectorShape shape = vectorizationInfo.getVectorShape(*gep);
    if (shape.isStrided(gep->getResultElementType()->getPrimitiveSizeInBits() / 8))
      return false;
    else if (shape.isStrided() || shape.isVarying())
      return true;
  }

  if (vectorizationInfo.hasKnownShape(*inst)) {
    VectorShape shape = vectorizationInfo.getVectorShape(*inst);
    return isa<AllocaInst>(inst) ? !shape.isUniform() : shape.isVarying();
  } else {
    for (unsigned i = 0; i < inst->getNumOperands(); ++i) {
      // operands are either constants or have shapes
      Value *val = inst->getOperand(i);
      assert((isa<Constant>(val) || vectorizationInfo.hasKnownShape(*val)) &&
             "expected either a constant or a known shape!");
      if (isa<Constant>(val)) continue;
      else if (vectorizationInfo.getVectorShape(*val).isVarying()) return true;
    }
    // all operands uniform, should not be vectorized
    return false;
  }
}


