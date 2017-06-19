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

#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Intrinsics.h"

#include "rv/transform/irPolisher.h"
#include "report.h"

#include "rvConfig.h"

using namespace llvm;

namespace rv {

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

  // Bitwidth the second operand (2nd compared value or one of the select branches)
  bitWidth = inst->getOperand(1)->getType()->getScalarSizeInBits();
  if (vecLen * bitWidth != 128 &&
      vecLen * bitWidth != 256)
    return false;

  // Try to not to modify min/max patterns
  for (auto user : inst->users()) {
    if (auto selectInst = dyn_cast<SelectInst>(user)) {
      Value *left, *right;
      Instruction::CastOps castOp;
      auto selectPattern = matchSelectPattern(selectInst, left, right, &castOp);
      if (SelectPatternResult::isMinOrMax(selectPattern.Flavor))
        return false;
    }
  }

  return true;
}

Value *IRPolisher::replaceCmpInst(IRBuilder<> &builder, llvm::CmpInst *cmpInst, unsigned bitWidth) {
  auto left  = cmpInst->getOperand(0);
  auto right = cmpInst->getOperand(1);

  // Optimizations:
  // cmp eq <n x i1> %x, false => not %x
  // cmp ne <n x i1> %x, true  => not %x
  // cmp eq <n x i1> %x, true  => %x
  // cmp ne <n x i1> %x, false => %x
  auto boolCmp = isBooleanVector(left->getType());
  auto pred = cmpInst->getPredicate();
  if (boolCmp && (pred == CmpInst::ICMP_EQ || pred == CmpInst::ICMP_NE)) {
    if (auto leftCst = dyn_cast<Constant>(left)) {
      if ((leftCst->isAllOnesValue() && pred == CmpInst::ICMP_EQ) ||
          (leftCst->isZeroValue()    && pred == CmpInst::ICMP_NE)) {
        return getMaskForValueOrInst(builder, right, bitWidth);
      } else if ((leftCst->isAllOnesValue() && pred == CmpInst::ICMP_NE) ||
                 (leftCst->isZeroValue()    && pred == CmpInst::ICMP_EQ)) {
        return builder.CreateNot(getMaskForValueOrInst(builder, right, bitWidth));
      }
    } else if (auto rightCst = dyn_cast<Constant>(right)) {
      if ((rightCst->isAllOnesValue() && pred == CmpInst::ICMP_EQ) ||
          (rightCst->isZeroValue()    && pred == CmpInst::ICMP_NE)) {
        return getMaskForValueOrInst(builder, left, bitWidth);
      } else if ((rightCst->isAllOnesValue() && pred == CmpInst::ICMP_EQ) ||
                 (rightCst->isZeroValue()    && pred == CmpInst::ICMP_NE)) {
        return builder.CreateNot(getMaskForValueOrInst(builder, left, bitWidth));
      }
    }
  }

  auto newLeft  = boolCmp ? getMaskForValueOrInst(builder, left,  bitWidth) : left;
  auto newRight = boolCmp ? getMaskForValueOrInst(builder, right, bitWidth) : right;

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

  return cmpInst->isFPPredicate()
    ? builder.CreateFCmp(cmpInst->getPredicate(), newLeft, newRight)
    : builder.CreateICmp(cmpInst->getPredicate(), newLeft, newRight);
}

Value *IRPolisher::replaceSelectInst(IRBuilder<> &builder, llvm::SelectInst *selectInst, unsigned bitWidth) {
  // If the select is part of a min/max pattern, do not replace it
  {
    Value *left, *right;
    Instruction::CastOps castOp;
    auto selectPattern = matchSelectPattern(selectInst, left, right, &castOp);
    if (SelectPatternResult::isMinOrMax(selectPattern.Flavor))
      return builder.Insert(selectInst->clone());
  }

  auto cond = selectInst->getOperand(0);
  auto s1   = selectInst->getOperand(1);
  auto s2   = selectInst->getOperand(2);

  // Optimization: if the select is using a NOT,
  // then invert operands and use original (non-NOT) value.
  bool invertOps = false;
  if (BinaryOperator::isNot(cond)) {
    invertOps = true;
    cond = BinaryOperator::getNotArgument(cond);
  }
  if (invertOps) std::swap(s1, s2);

  // Optimizations:
  // select %x true false  -> cond
  // select %x false true  -> not cond
  auto boolSelect = isBooleanVector(s1->getType());
  if (boolSelect) {
    auto s1Cst = dyn_cast<Constant>(s1);
    auto s2Cst = dyn_cast<Constant>(s2);
    if (s1Cst && s2Cst) {
      if (s1Cst->isAllOnesValue() && s2Cst->isZeroValue())
        return getMaskForValueOrInst(builder, cond, bitWidth);
      if (s1Cst->isZeroValue() && s2Cst->isAllOnesValue())
        return builder.CreateNot(getMaskForValueOrInst(builder, cond, bitWidth));
    }
  }

  // Get a mask from the condition (could come from ANDs)
  auto condMask = getMaskForValueOrInst(builder, cond, bitWidth);
  auto newS1 = boolSelect ? getMaskForValueOrInst(builder, s1, bitWidth) : s1;
  auto newS2 = boolSelect ? getMaskForValueOrInst(builder, s2, bitWidth) : s2;

  // Replace with blendvps/pd when possible, otherwise fall back to a LLVM select
  auto opBitWidth = newS1->getType()->getScalarSizeInBits();
  auto vecLen = cond->getType()->getVectorNumElements();
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
    newInst = builder.CreateCast(castInst->getOpcode(), newOp, destTy);
  } else if (auto storeInst = dyn_cast<StoreInst>(inst)) {
    auto value = storeInst->getOperand(0);
    auto valueMask = getMaskForValueOrInst(builder, value, bitWidth);
    auto newValue = getConditionFromMask(builder, valueMask);
    auto newStore = builder.CreateStore(newValue, storeInst->getOperand(1));

    newStore->setAlignment(storeInst->getAlignment());
    newStore->setVolatile(storeInst->isVolatile());
    newStore->setOrdering(storeInst->getOrdering());
    newStore->setSynchScope(storeInst->getSynchScope());

    newInst = newStore;
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

void IRPolisher::polish() {
  IF_DEBUG { errs() << "Starting polishing phase\n"; }

  visitedInsts.clear();
  queue = std::move(std::queue<ExtInst>());

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
  for (auto& extInst : visitedInsts) {
    auto inst = extInst.first.inst;
#ifndef NDEBUG
    // Check that all the users of this instruction have been processed before removing it
    for (auto user : inst->users()) {
      if (auto userInst = dyn_cast<Instruction>(user)) {
        assert(visitedInsts.find(ExtInst(userInst, extInst.first.bitWidth)) != visitedInsts.end());
      }
    }
#endif
    inst->replaceAllUsesWith(UndefValue::get(inst->getType()));
    inst->eraseFromParent();
  }

  if (visitedInsts.size() > 0) {
    Report() << "IRPolish: polished " << visitedInsts.size() << " instruction(s)\n";
  }
}

}
