//===- Utils.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <utils/rvTools.h>
#include "Utils.h"

using namespace llvm;
using namespace rv;

Type *getVectorType(Type *type, unsigned width) {
  if (type->isVoidTy())
    return type;
  else
    return VectorType::get(type, width);
}

Value *createContiguousVector(unsigned width, Type *type, int start) {
  std::vector<Constant*> constants(width, nullptr);
  for (unsigned i = 0; i < width; ++i) {
    unsigned int val = i + start;
    Constant *constant = type->isFloatingPointTy() ? ConstantFP::get(type, val) : ConstantInt::get(type, val);
    constants[i] = constant;
  }
  return ConstantVector::get(ArrayRef<Constant *>(constants, &width));
}

BasicBlock *createCascadeBlocks(Function *insertInto, unsigned vectorWidth,
                                std::vector<BasicBlock *> &condBlocks,
                                std::vector<BasicBlock *> &maskedBlocks) {
  BasicBlock *cond, *mask;
  for (unsigned lane = 0; lane < vectorWidth; ++lane) {
    cond = BasicBlock::Create(insertInto->getContext(), "cascade_cond_" + std::to_string(lane),
                              insertInto);
    mask = BasicBlock::Create(insertInto->getContext(), "cascade_masked_" + std::to_string(lane),
                              insertInto);
    condBlocks.push_back(cond);
    maskedBlocks.push_back(mask);
  }
  return BasicBlock::Create(insertInto->getContext(), "cascade_end", insertInto);
}

bool isSupportedOperation(Instruction *const inst) {
  // binary operations (normal & bitwise), load / stores, conversion operations, returns, and other operations
  // exception: calls with vector or struct return type are not supported
  CallInst *call = dyn_cast<CallInst>(inst);
  if (call) {
    Type *retType = call->getFunctionType()->getReturnType();
    if (retType->isStructTy() || retType->isVectorTy())
      return false;
  }
  return inst->isBinaryOp() || isa<LoadInst>(inst) || isa<StoreInst>(inst) || inst->isCast() || isa<ReturnInst>(inst) ||
         (!isa<ExtractElementInst>(inst) && !isa<ExtractValueInst>(inst) && !isa<InsertElementInst>(inst) &&
          !isa<InsertValueInst>(inst) && !isa<ShuffleVectorInst>(inst) &&
          (inst->getOpcode() >= Instruction::OtherOpsBegin && inst->getOpcode() <= Instruction::OtherOpsEnd));
}
