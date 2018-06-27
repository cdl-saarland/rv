//===- irPolisher.cpp - IR polish pass ----------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// The IR polisher performs:
// - early instruction selection (it replaces IR patterns with intrinsics)
// - bool vector promotion to i32/i64
// to work around code quality issues in LLVM 4.0.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"

#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"

#include "rv/transform/irPolisher.h"
#include "report.h"

#include "rv/LinkAllPasses.h"
#include "rv/passes.h"
#include "rv/config.h"

#include "rvConfig.h"


using namespace llvm;
using namespace rv;


void IRPolisher::enqueueInst(llvm::Instruction* inst, unsigned bitWidth) {
  ExtInst extInst(inst, bitWidth);
  if (visitedInsts.find(extInst) == visitedInsts.end())
    queue.emplace(extInst);
}

bool IRPolisher::isBooleanVector(const Type *type) {
  auto vectorType = dyn_cast<VectorType>(type);
  return vectorType && vectorType->getElementType() == Type::getInt1Ty(vectorType->getContext());
}

bool IRPolisher::canReplaceInst(llvm::Instruction *inst, unsigned& bitWidth) {
  auto instTy = inst->getType();
  if (!instTy->isVectorTy()) return false;
  if (!isa<CmpInst>(inst)) return false;

  // Only support for SSE/AVX with 32 or 64-bit floats
  auto vecLen = instTy->getVectorNumElements();
  if (vecLen != 2 && vecLen != 4 && vecLen != 8) return false;

  bitWidth = inst->getOperand(0)->getType()->getScalarSizeInBits();
  if (vecLen * bitWidth != 128 &&
      vecLen * bitWidth != 256)
    return false;

  return true;
}

inline bool startsWith(const char* str1, const char* str2) {
  return !strncmp(str1, str2, strlen(str2));
}

Value *IRPolisher::mapIntrinsicCall(llvm::IRBuilder<>& builder, llvm::CallInst* callInst, unsigned bitWidth) {
  if (!isa<Function>(callInst->getCalledValue())) return nullptr;
  auto callee = callInst->getCalledFunction();
  auto isReduceOr = startsWith(callee->getName().data(), "rv_reduce_or");
  auto isReduceAnd = startsWith(callee->getName().data(), "rv_reduce_and");
  if (isReduceOr || isReduceAnd) {
    // Use the PTEST instruction for boolean reductions
    auto newArg = getMaskForValueOrInst(builder, callInst->getArgOperand(0), bitWidth);

    auto vecLen = newArg->getType()->getVectorNumElements();
    auto destTy = VectorType::get(builder.getIntNTy(64), bitWidth * vecLen / 64);

    bool useNot = false;
    Value * left = nullptr, * right = nullptr;
    if (auto binOp = dyn_cast<BinaryOperator>(newArg)) {
      if (binOp->getOpcode() == Instruction::And) {
        // Fold PTEST(AND(x, y)) and PTEST(AND(NOT(x), y))
        left = binOp->getOperand(0);
        right = binOp->getOperand(1);

        if (isReduceOr) {
          if (BinaryOperator::isNot(left)) {
            left = BinaryOperator::getNotArgument(left);
            useNot = true;
          }
          if (!useNot && BinaryOperator::isNot(right)) {
            right = BinaryOperator::getNotArgument(right);
            std::swap(left, right);
            useNot = true;
          }
        } else {
          // AND reductions already use PTEST(NOT(x), NOT(x))
          useNot = true;
          right = builder.CreateNot(right);
        }

        left  = builder.CreateBitCast(left, destTy);
        right = builder.CreateBitCast(right, destTy);
      }
    }

    if (!left) {
      left = right = builder.CreateBitCast(isReduceAnd ? builder.CreateNot(newArg) : newArg, destTy);
    }

    auto isAVX = vecLen * bitWidth == 256;
    auto id = isAVX
      ? (useNot ? Intrinsic::x86_avx_ptestc_256 : Intrinsic::x86_avx_ptestz_256)
      : (useNot ? Intrinsic::x86_sse41_ptestc   : Intrinsic::x86_sse41_ptestz);
    auto func = Intrinsic::getDeclaration(callInst->getModule(), id);

    auto ptestCall = builder.CreateCall(func, { left, right });
    return isReduceOr
      ? builder.CreateICmpEQ(ptestCall, builder.getInt32(0))
      : builder.CreateICmpNE(ptestCall, builder.getInt32(0));
  }
  return nullptr;
}

Value *IRPolisher::lowerIntrinsicCall(llvm::CallInst* callInst) {
  if (!isa<Function>(callInst->getCalledValue())) return nullptr;
  auto callee = callInst->getCalledFunction();

  auto isReduceOr = startsWith(callee->getName().data(), "rv_reduce_or");
  auto isReduceAnd = startsWith(callee->getName().data(), "rv_reduce_and");
  auto isGather = startsWith(callee->getName().data(), "llvm.masked.gather");

  // Insert instructions after the current one
  if (isReduceOr || isReduceAnd) {
    IRBuilder<> builder(callInst);
    auto arg = callInst->getArgOperand(0);
    auto castTy = builder.getIntNTy(arg->getType()->getVectorNumElements());
    auto castedArg = builder.CreateBitCast(arg, castTy);
    return isReduceOr
      ? builder.CreateICmpNE(castedArg, Constant::getNullValue(castTy))
      : builder.CreateICmpEQ(castedArg, Constant::getAllOnesValue(castTy));
  }

  // Replace LLVM's gather intrinsic by an AVX2 gather
  if (isGather) {
    // Only support 32bit gathers
    auto vecTy = callInst->getType();
    if (vecTy->getVectorNumElements() != 8 ||
        vecTy->getScalarSizeInBits() != 32)
      return nullptr;

    // Only support pointers of the form base + index
    auto gepVal = dyn_cast<GetElementPtrInst>(callInst->getOperand(0));
    if (!gepVal || gepVal->getNumIndices() != 2)
      return nullptr;
    // First index must be zero
    if (!isa<Constant>(gepVal->getOperand(1)) ||
        !cast<Constant>(gepVal->getOperand(1))->isNullValue())
      return nullptr;

    // Find base pointer
    auto basePtr = gepVal->getPointerOperand();
    auto baseGep = gepVal;
    uint64_t offset = 0;
    uint64_t scale  = 4;
    uint32_t multiplier = 1;
    // Only look for one structure level
    // TODO: handle an indefinite number of structure levels
    if (basePtr->getType()->isVectorTy()) {
      baseGep = dyn_cast<GetElementPtrInst>(basePtr);
      if (!baseGep || baseGep->getNumIndices() != 2 || !gepVal->hasAllConstantIndices())
        return nullptr;
      auto structTy = basePtr->getType()->getVectorElementType()->getPointerElementType();
      if (!structTy->isStructTy() || !cast<Constant>(baseGep->getOperand(1))->isNullValue())
        return nullptr;

      DataLayout dataLayout(callInst->getModule());
      auto structLayout = dataLayout.getStructLayout(cast<StructType>(structTy));
      auto elem = cast<ConstantInt>(gepVal->getOperand(2))->getZExtValue();
      multiplier = structLayout->getSizeInBytes();
      offset  = structLayout->getElementOffset(elem);
      scale   = 1;
      basePtr = baseGep->getOperand(0);
      if (basePtr->getType()->isVectorTy())
        return nullptr;
    }

    // Only support 32bit indices
    auto idxVal = baseGep->getOperand(2);
    if (idxVal->getType()->getScalarSizeInBits() > 32) {
      // Tolerate sign extensions for indices
      if (dyn_cast<CastInst>(idxVal) &&
          cast<CastInst>(idxVal)->getOpcode() == Instruction::CastOps::SExt &&
          cast<CastInst>(idxVal)->getOperand(0)->getType()->getScalarSizeInBits() <= 32) {
        idxVal = cast<CastInst>(idxVal)->getOperand(0);
      } else {
        return nullptr;
      }
    }

    // Convert to an AVX2 gather
    IRBuilder<> builder(callInst);
    auto func = Intrinsic::getDeclaration(callInst->getModule(), Intrinsic::x86_avx2_gather_d_ps_256);
    auto idxTy = VectorType::get(builder.getInt32Ty(), 8);
    auto valTy = VectorType::get(builder.getFloatTy(), 8);
    auto ptrTy = PointerType::get(builder.getInt8Ty(), 0);
    auto maskVal = builder.CreateBitCast(getMaskForValueOrInst(builder, callInst->getOperand(2), 32), valTy);
    auto extIdx = idxVal->getType()->getScalarSizeInBits() != 32
      ? builder.CreateSExt(idxVal, idxTy)
      : idxVal;
    auto gatherVal = builder.CreateCall(func, {
      UndefValue::get(valTy),
      builder.CreatePointerCast(basePtr, ptrTy),
      builder.CreateAdd(
        builder.CreateMul(extIdx, builder.CreateVectorSplat(8, builder.getInt32(multiplier))),
        builder.CreateVectorSplat(8, builder.getInt32(offset))),
      maskVal,
      builder.getInt8(scale)
    });
    return builder.CreateBitCast(gatherVal, callInst->getType());
  }
  return nullptr;
}

Value *IRPolisher::replaceCmpInst(IRBuilder<> &builder, llvm::CmpInst *cmpInst, unsigned bitWidth) {
  auto left  = cmpInst->getOperand(0);
  auto right = cmpInst->getOperand(1);
  auto boolCmp  = isBooleanVector(left->getType());
  auto newLeft  = boolCmp ? getMaskForValueOrInst(builder, left , bitWidth) : left;
  auto newRight = boolCmp ? getMaskForValueOrInst(builder, right, bitWidth) : right;

  auto pred = cmpInst->getPredicate();
  auto newLeftTy = newLeft->getType();
  auto vecLen    = newLeftTy->getVectorNumElements();
  auto scalarTy  = newLeftTy->getScalarType();
  assert(vecLen > 0);

  // Transform a floating point comparison to a cmpps instruction
  if (cmpInst->isFPPredicate() && (vecLen == 2 || vecLen == 4 || vecLen == 8)) {
    Intrinsic::ID id = Intrinsic::not_intrinsic;
    if (scalarTy == builder.getFloatTy()) {
      if (vecLen == 4)      id = Intrinsic::x86_sse_cmp_ps;
      else if (vecLen == 8) id = Intrinsic::x86_avx_cmp_ps_256;
    } else if (scalarTy == builder.getDoubleTy()) {
      if (vecLen == 2)      id = Intrinsic::x86_sse2_cmp_pd;
      else if (vecLen == 4) id = Intrinsic::x86_avx_cmp_pd_256;
    }

    int cmpOp = -1;
    bool invert = false;
    switch (pred) {
      case CmpInst::FCMP_OEQ: cmpOp =  0; break;
      case CmpInst::FCMP_OGT: cmpOp =  1; invert = true; break;
      case CmpInst::FCMP_OGE: cmpOp =  2; invert = true; break;
      case CmpInst::FCMP_OLT: cmpOp =  1; break;
      case CmpInst::FCMP_OLE: cmpOp =  2; break;
      case CmpInst::FCMP_ONE: cmpOp =  4; break;
      case CmpInst::FCMP_ORD: cmpOp =  7; break;
      case CmpInst::FCMP_UNO: cmpOp =  3; break;

      case CmpInst::FCMP_UEQ: cmpOp = 24; break;
      case CmpInst::FCMP_UGT: cmpOp = 22; break;
      case CmpInst::FCMP_UGE: cmpOp = 21; break;
      case CmpInst::FCMP_ULT: cmpOp = 25; break;
      case CmpInst::FCMP_ULE: cmpOp = 26; break;
      case CmpInst::FCMP_UNE: cmpOp = 20; break;
      default: assert(false);
    }

    if (id != Intrinsic::not_intrinsic && cmpOp >= 0) {
      auto func = Intrinsic::getDeclaration(cmpInst->getModule(), id);
      auto cmpCall = builder.CreateCall(func, { invert ? newRight : newLeft, invert ? newLeft : newRight, builder.getInt8(cmpOp) });
      auto vecTy = VectorType::get(builder.getIntNTy(scalarTy->getPrimitiveSizeInBits()), vecLen);
      return builder.CreateBitCast(cmpCall, vecTy);
    }
  }

  auto newCmp = cmpInst->isFPPredicate()
    ? builder.CreateFCmp(cmpInst->getPredicate(), newLeft, newRight)
    : builder.CreateICmp(cmpInst->getPredicate(), newLeft, newRight);
  return getMaskForValue(builder, newCmp, bitWidth);
}

Value *IRPolisher::replaceSelectInst(IRBuilder<> &builder, llvm::SelectInst *selectInst, unsigned bitWidth) {
  // Get a mask from the condition (could come from ANDs)
  auto s1 = selectInst->getOperand(1);
  auto s2 = selectInst->getOperand(2);
  auto boolSelect = isBooleanVector(selectInst->getType());
  auto condMask = getMaskForValueOrInst(builder, selectInst->getOperand(0), bitWidth);
  auto newS1 = boolSelect ? getMaskForValueOrInst(builder, s1, bitWidth) : s1;
  auto newS2 = boolSelect ? getMaskForValueOrInst(builder, s2, bitWidth) : s2;

  auto newS1Ty = newS1->getType();
  auto opBitWidth = newS1Ty->getScalarSizeInBits();
  auto vecLen = newS1Ty->getVectorNumElements();

  // If the select is part of a min/max pattern, try to keep the pattern intact
  auto cmpInst = dyn_cast<CmpInst>(selectInst->getOperand(0));
  if (cmpInst) {
    // Do not break unsafe min/max patterns
    FastMathFlags oldFMF;
    if (isa<FPMathOperator>(cmpInst)) {
        oldFMF = cmpInst->getFastMathFlags();
        FastMathFlags newFMF = oldFMF;
        newFMF.setFast();
        cmpInst->setFastMathFlags(newFMF);
    }

    Value *left, *right;
    Instruction::CastOps castOp;
    auto selectPattern = matchSelectPattern(selectInst, left, right, &castOp);

    if (isa<FPMathOperator>(cmpInst)) cmpInst->setFastMathFlags(oldFMF);

    if (SelectPatternResult::isMinOrMax(selectPattern.Flavor)) {
      auto cmpClone = builder.Insert(cmpInst->clone());
      auto selectClone = builder.Insert(selectInst->clone());
      selectClone->setOperand(0, cmpClone);
      return selectClone;
    }
  }

  // Replace with blendvps/pd when possible, otherwise fall back to a LLVM select
  if ((opBitWidth == bitWidth) &&
      (vecLen == 2 || vecLen == 4 || vecLen == 8) &&
      (vecLen * bitWidth == 256 || vecLen * bitWidth == 128)) {
    Intrinsic::ID id = Intrinsic::not_intrinsic;
    if (vecLen == 2) {
      if (bitWidth == 64) id = Intrinsic::x86_sse41_blendvpd;
    } else if (vecLen == 4) {
      if (bitWidth == 32) id = Intrinsic::x86_sse41_blendvps;
      if (bitWidth == 64) id = Intrinsic::x86_avx_blendv_pd_256;
    } else if (vecLen == 8) {
      if (bitWidth == 16) id = Intrinsic::x86_sse41_pblendvb; // this assumes that the 16-bit masks are either 0 or 0xFFFF
      if (bitWidth == 32) id = Intrinsic::x86_avx_blendv_ps_256;
    }

    if (id != Intrinsic::not_intrinsic) {
      auto func = Intrinsic::getDeclaration(selectInst->getModule(), id);
      auto blendArgTy = func->getReturnType();
      auto blendCall = builder.CreateCall(func, {
        builder.CreateBitCast(newS2, blendArgTy),
        builder.CreateBitCast(newS1, blendArgTy),
        builder.CreateBitCast(condMask, blendArgTy)
      });
      return builder.CreateBitCast(blendCall, newS1->getType());
    }
  }

  auto newCond = getConditionFromMask(builder, condMask);
  return builder.CreateSelect(newCond, newS1, newS2);
}

Value *IRPolisher::getMaskForInst(Instruction *inst, unsigned bitWidth) {
  assert(bitWidth != 1);
  auto instIt = visitedInsts.find(ExtInst(inst, bitWidth));
  if (instIt != visitedInsts.end())
      return instIt->second;

  // Insert instructions after the current one
  IRBuilder<> builder(inst);
  builder.SetInsertPoint(inst->getNextNode());

  Value* newInst = nullptr;
  if (auto cmpInst = dyn_cast<CmpInst>(inst)) {
    auto newCmp = replaceCmpInst(builder, cmpInst, bitWidth);
    newInst = getMaskForValue(builder, newCmp, bitWidth);
  } else if (auto binOp = dyn_cast<BinaryOperator>(inst)) {
    // Find a mask for each of the operands
    auto left  = binOp->getOperand(0);
    auto right = binOp->getOperand(1);

    auto newLeft  = getMaskForValueOrInst(builder, left,  bitWidth);
    auto newRight = getMaskForValueOrInst(builder, right, bitWidth);

    newInst = builder.CreateBinOp(binOp->getOpcode(), newLeft, newRight);
  } else if (auto insert = dyn_cast<InsertElementInst>(inst)) {
    // Creating a boolean vector by repeated insertion is allowed
    auto vec   = insert->getOperand(0);
    auto elem  = insert->getOperand(1);
    auto index = insert->getOperand(2);

    auto mask = getMaskForValueOrInst(builder, vec, bitWidth);
    auto newElem = builder.CreateSExt(elem, builder.getIntNTy(bitWidth));

    newInst = builder.CreateInsertElement(mask, newElem, index);
  } else if (auto insert = dyn_cast<ExtractElementInst>(inst)) {
    // Creating a boolean vector by repeated insertion is allowed
    auto vec   = insert->getOperand(0);
    auto index = insert->getOperand(1);

    auto mask = getMaskForValueOrInst(builder, vec, bitWidth);
    auto extract = builder.CreateExtractElement(mask, index);
    newInst = builder.CreateTrunc(extract, builder.getInt1Ty());
  } else if (auto shuffle = dyn_cast<ShuffleVectorInst>(inst)) {
    // Typical use of this pattern is for broadcasts
    auto v1 = shuffle->getOperand(0);
    auto v2 = shuffle->getOperand(1);
    auto mask = shuffle->getOperand(2);

    newInst = builder.CreateShuffleVector(
      getMaskForValueOrInst(builder, v1, bitWidth),
      getMaskForValueOrInst(builder, v2, bitWidth),
      mask);
  } else if (auto select = dyn_cast<SelectInst>(inst)) {
    newInst = replaceSelectInst(builder, select, bitWidth);
  } else if (auto castInst = dyn_cast<CastInst>(inst)) {
    auto destTy = castInst->getDestTy();
    auto newOp = getMaskForValueOrInst(builder, inst->getOperand(0), bitWidth);
    if (newOp->getType() == destTy && castInst->getOpcode() == Instruction::CastOps::SExt) {
      // For sign extension casts, there is no need to perform any
      // cast if the operand is already of the destination type
      newInst = newOp;
    } else {
      // General case: re-create a vector of booleans, and create a cast on top of it
      newInst = builder.CreateCast(castInst->getOpcode(), getConditionFromMask(builder, newOp), destTy);
    }
  } else if (auto storeInst = dyn_cast<StoreInst>(inst)) {
    auto value = storeInst->getOperand(0);
    auto valueMask = getMaskForValueOrInst(builder, value, bitWidth);
    auto newValue = getConditionFromMask(builder, valueMask);
    auto newStore = builder.CreateStore(newValue, storeInst->getOperand(1));

    newStore->setAlignment(storeInst->getAlignment());
    newStore->setVolatile(storeInst->isVolatile());
    newStore->setOrdering(storeInst->getOrdering());
    newStore->setSyncScopeID(storeInst->getSyncScopeID());

    newInst = newStore;
  } else if (auto loadInst = dyn_cast<LoadInst>(inst)) {
    auto ptr = loadInst->getOperand(0);
    auto newLoad = builder.CreateLoad(ptr);

    newLoad->setAlignment(loadInst->getAlignment());
    newLoad->setVolatile(loadInst->isVolatile());
    newLoad->setOrdering(loadInst->getOrdering());
    newLoad->setSyncScopeID(loadInst->getSyncScopeID());

    newInst = getMaskForValue(builder, newLoad, bitWidth);
  } else if (auto phiNode = dyn_cast<PHINode>(inst)) {
    auto vecLen = phiNode->getType()->getVectorNumElements();
    auto vecTy = VectorType::get(builder.getIntNTy(bitWidth), vecLen);

    auto newPhi = builder.CreatePHI(vecTy, phiNode->getNumIncomingValues());
    // We need to insert the phi node in the map here,
    // as calls to getMaskForValueOrInst may diverge otherwise
    // Redundant values will just be overwritten (it's a map)
    visitedInsts.emplace(ExtInst(inst, bitWidth), newPhi);
    for (size_t i = 0; i < phiNode->getNumIncomingValues(); i++) {
      newPhi->addIncoming(getMaskForValueOrInst(builder, phiNode->getIncomingValue(i), bitWidth),
                          phiNode->getIncomingBlock(i));
    }
    newInst = newPhi;
  } else if (auto callInst = dyn_cast<CallInst>(inst)) {
    // Handle intrinsics
    if (auto mappedInst = mapIntrinsicCall(builder, callInst, bitWidth)) {
      newInst = mappedInst;
    } else {
      // Default path for all other function calls
      std::vector<Value*> newArgs;
      for (auto& arg : callInst->arg_operands()) {
        auto newArg = arg.get();
        // If the argument is a boolean vector, we reconstruct it from the new mask
        if (isBooleanVector(arg->getType()))
          newArg = getConditionFromMask(builder, getMaskForValueOrInst(builder, newArg, bitWidth));
        newArgs.emplace_back(newArg);
      }
      newInst = builder.CreateCall(callInst->getCalledFunction(), newArgs);
      if (isBooleanVector(newInst->getType()))
        newInst = getMaskForValue(builder, newInst, bitWidth);
    }
  }

  assert(newInst);
  visitedInsts.emplace(ExtInst(inst, bitWidth), newInst);

  // Process the users of the instruction
  if (isBooleanVector(inst->getType())) {
    for (auto user : inst->users()) {
      if (auto userInst = dyn_cast<Instruction>(user)) {
        enqueueInst(userInst, bitWidth);
      }
    }
  } else {
    inst->replaceAllUsesWith(newInst);
  }

  return newInst;
}

llvm::Value *IRPolisher::getMaskForValue(IRBuilder<> &builder, llvm::Value *value, unsigned bitWidth) {
  auto scalarType = builder.getIntNTy(bitWidth);
  auto vectorLen = value->getType()->getVectorNumElements();
  if (scalarType == value->getType()->getScalarType()) return value;

  if (value->getType()->getScalarSizeInBits() < bitWidth)
    return builder.CreateSExtOrBitCast(value, VectorType::get(scalarType, vectorLen));
  else
    return builder.CreateTrunc(value, VectorType::get(scalarType, vectorLen));
}

llvm::Value *IRPolisher::getMaskForValueOrInst(IRBuilder<> &builder, llvm::Value *value, unsigned bitWidth) {
  if (auto inst = dyn_cast<Instruction>(value)) return getMaskForInst(inst, bitWidth);
  return getMaskForValue(builder, value, bitWidth);
}

llvm::Value *IRPolisher::getConditionFromMask(IRBuilder<> &builder, llvm::Value* value) {
  auto boolTy = builder.getIntNTy(1);
  if (auto cast = dyn_cast<CastInst>(value)) {
    auto srcTy = cast->getSrcTy();

    if (srcTy->isVectorTy() && srcTy->getScalarType() == boolTy)
      return cast->getOperand(0);
  }
  // The mask is compared to a zero value to get a vector of i1s (better than truncation for codegen)
  return builder.CreateICmpNE(value, Constant::getNullValue(value->getType()));
}

bool IRPolisher::polish() {
  if (!(config.useAVX2 || config.useAVX2)) {
    return false; // requires >= AVX
  }

  IF_DEBUG { errs() << "Starting polishing phase\n"; }

  // Run InstCombine to perform peephole opts
  FunctionPassManager FPM;
  FunctionAnalysisManager FAM;
  PassBuilder builder;
  builder.registerFunctionAnalyses(FAM);
  FPM.addPass(InstCombinePass());
  FPM.run(F, FAM);

  visitedInsts.clear();
  queue = std::queue<ExtInst>();

  // Fill the queue with uses of the result of vector (f)cmps
  for (auto it = inst_begin(F), end = inst_end(F); it != end; ++it) {
    unsigned bitWidth;
    if (canReplaceInst(&*it, bitWidth))
      enqueueInst(&*it, bitWidth);
  }

  while (!queue.empty()) {
    auto extInst = queue.front();
    queue.pop();

    // Extend the instruction to work on vector of integers instead of vectors of i1s
    auto inst = extInst.inst;
    auto bitWidth = extInst.bitWidth;
    getMaskForInst(inst, bitWidth);
  }

  // Remove original versions of transformed instructions
  for (auto &extInst : visitedInsts) {
    auto inst = extInst.first.inst;

    // The same instruction might have been replaced with 2 polished
    // instructions (32 and 64 bits), and we must only remove it once
    if (extInst.first.bitWidth == 64 && visitedInsts.count(ExtInst(inst, 32))) continue;

#ifdef RV_DEBUG
    // Check that all the users of this instruction have been processed before removing it
    for (auto user : inst->users()) {
      if (auto userInst = dyn_cast<Instruction>(user)) {
        // This instruction might be used with a different bit width in different contexts
        assert(visitedInsts.count(ExtInst(userInst, 32)) != 0 ||
               visitedInsts.count(ExtInst(userInst, 64)) != 0);
      }
    }
#endif

    inst->replaceAllUsesWith(UndefValue::get(inst->getType()));
    inst->eraseFromParent();
  }

  // Handle intrinsics that were not removed during mask expansion.
  // This happens when doing comparisons with objects that do not
  // map to SSE/AVX registers (e.g. fcmp <8 x double> ...). We handle
  // this by emitting generic LLVM IR.
  std::vector<CallInst*> loweredCalls;
  for (auto it = inst_begin(F), end = inst_end(F); it != end; ++it) {
    auto inst = &*it;
    if (auto callInst = dyn_cast<CallInst>(inst)) {
      if (auto newInst = lowerIntrinsicCall(callInst)) {
        callInst->replaceAllUsesWith(newInst);
        loweredCalls.push_back(callInst);
      }
    }
  }
  for (auto call : loweredCalls) call->eraseFromParent();

  if (visitedInsts.size() > 0) {
    Report() << "IRPolish: polished " << visitedInsts.size() << " instruction(s)\n";
  }

  return visitedInsts.size() > 0;
}





namespace {

class IRPolisherWrapper : public FunctionPass {
  rv::Config config;

public:
  static char ID;
  IRPolisherWrapper()
  : FunctionPass(ID)
  {}

  IRPolisherWrapper(const Config & _config)
  : FunctionPass(ID)
  , config(_config)
  {}

  bool runOnFunction(Function & F) {
    rv::IRPolisher polisher(F, config);
    bool changed = polisher.polish();
    return changed;
  }
};

}



char IRPolisherWrapper::ID = 0;

FunctionPass *rv::createIRPolisherWrapperPass(rv::Config config) { return new IRPolisherWrapper(config); }

INITIALIZE_PASS_BEGIN(IRPolisherWrapper, "rv-irpolish",
                      "RV - Polish Vector IR", false, false)
INITIALIZE_PASS_END(IRPolisherWrapper, "rv-irpolish", "RV - Polish Vector IR",
                    false, false)
