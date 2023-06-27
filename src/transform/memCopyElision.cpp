//===- src/transform/maskExpander.cpp - IR generator for edge and block predicates  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/transform/memCopyElision.h"
#include "rv/vectorizationInfo.h"

#include <llvm/IR/Instructions.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/IRBuilder.h>
// #include <llvm/IR/Intrinsics.h>
#include <llvm/IR/IntrinsicInst.h>

#include "rvConfig.h"

using namespace rv;
using namespace llvm;

#if 1
#define IF_DEBUG_MCE IF_DEBUG
#else
#define IF_DEBUG_MCE if (true)
#endif

bool
MemCopyElision::IsDivergent(Instruction & inst) const {
  for (auto & op : inst.operands()) {
    if (!vecInfo.getVectorShape(*op).isUniform()) return true;
  }
  return false;
}

llvm::Value *
MemCopyElision::deriveBase(llvm::Value * ptr, size_t numBytes) {
  IF_DEBUG_MCE { errs() << "derive " << *ptr << " size " << numBytes << "\n"; }

  //TODO: Due to opaque pointers, reconstructing the base dataTy has become somewhat more complex.
  auto * gep = llvm::dyn_cast<llvm::GetElementPtrInst>(ptr);
  if (!gep)
    return nullptr;

  auto * dataTy = gep->getSourceElementType();
  size_t allocSize = layout.getTypeAllocSize(dataTy);
  if (numBytes % allocSize == 0) {
    IF_DEBUG_MCE { errs() << "\thit: pointer aligns with type!\n"; }
    return ptr;
  }

  // look through pointers
  auto * bc = dyn_cast<BitCastInst>(ptr);
  if (bc) {
    return deriveBase(cast<Instruction>(bc->getOperand(0)), numBytes);
  } else {
  }

  IF_DEBUG_MCE { errs() << "\fail: could not derive base from " << *ptr << "\n";}
  return nullptr;
}


llvm::Type *
MemCopyElision::deriveCommonType(llvm::Type * aTy, llvm::Type * bTy, size_t numBytes) {
  IF_DEBUG_MCE { errs() << "\t\t derive common type for " << *aTy << " and " << *bTy << "\n"; }

  if (aTy == bTy) {
    // TODO check datalayout for object size
    return aTy;
  } else if (aTy->isStructTy()) {
    return deriveCommonType(aTy->getStructElementType(0), bTy, numBytes);
  } else if (bTy->isStructTy()) {
    return deriveCommonType(aTy, bTy->getStructElementType(0), numBytes);
  }


  return nullptr;
}


// hint: implementation may assume that @baseTy was derived using @deriveCommonType
llvm::Value *
MemCopyElision::createBaseGEP(llvm::Value * ptrVal, llvm::Type * baseTy, llvm::IRBuilder<> & builder) {
  auto * intTy = IntegerType::getInt32Ty(builder.getContext());

  SmallVector<llvm::Value*, 4> idxList;
  auto * nullInt = ConstantInt::getNullValue(intTy);

  auto * gep = llvm::cast<llvm::GetElementPtrInst>(ptrVal);
  auto * dataTy = gep->getSourceElementType();
  idxList.push_back(nullInt);

  while (dataTy != baseTy) {
    dataTy = dataTy->getStructElementType(0);
    idxList.push_back(nullInt);
  }
  if (idxList.size() == 1) return ptrVal;
  else {
    auto * baseGep = builder.CreateGEP(dataTy, ptrVal, idxList, "basegep");
    vecInfo.setVectorShape(*baseGep, VectorShape::varying());
    return baseGep;
  }
}

void
MemCopyElision::lowerMemCopy(llvm::Value * destBase, llvm::Value * srcBase, llvm::Type * commonTy, llvm::IRBuilder<> & builder, size_t numBytes) {
  // TODO this code is highly specific to Coord<D> lowering
  auto * intTy = IntegerType::getInt32Ty(builder.getContext());

  unsigned AddrSpace = cast<PointerType>(srcBase->getType())->getAddressSpace();
  auto *CommonPtrTy = commonTy->getPointerTo(AddrSpace);

  auto *commonSrcBase =
      builder.CreatePointerCast(srcBase, CommonPtrTy);
  auto *commonDestBase =
      builder.CreatePointerCast(destBase, CommonPtrTy);

  const size_t elemSize = layout.getTypeStoreSize(commonTy);
  assert(numBytes % elemSize == 0);
  const size_t numElems = numBytes / elemSize;

  auto varShape = VectorShape::varying();
  for (size_t i = 0; i < numElems; ++i) {
    auto * idxConst = ConstantInt::get(intTy, i, false);
  // gep to elemens
    auto * srcElemPtr = builder.CreateGEP(commonTy, commonSrcBase, idxConst);
    auto * destElemPtr = builder.CreateGEP(commonTy, commonDestBase, idxConst);
    vecInfo.setVectorShape(*srcElemPtr, varShape);
    vecInfo.setVectorShape(*destElemPtr, varShape);

  // element transfer
    auto * elem = builder.CreateLoad(commonTy, srcElemPtr);
    vecInfo.setVectorShape(*elem, VectorShape::varying());
    auto * store = builder.CreateStore(elem, destElemPtr);
    vecInfo.setVectorShape(*store, varShape);
  }
}

bool
MemCopyElision::run() {
  std::vector<Instruction*> killVec;

  IF_DEBUG_MCE { errs() << "-- memCopy elision log --\n"; }
  for (auto & BB : vecInfo.getScalarFunction()) {
    if (!vecInfo.inRegion(BB)) continue;

    for (auto & Inst : BB) {
      auto *mcInst = dyn_cast<MemTransferInst>(&Inst);
      if (!mcInst) continue;
      if (!IsDivergent(Inst)) continue;
      IF_DEBUG_MCE  { errs() << "Found divergent memcpy: " << *mcInst << "\n"; }

    // analyze eligiblity
      auto srcVal = mcInst->getSource();
      auto destVal = mcInst->getDest();
      auto lenConst = dyn_cast<ConstantInt>(mcInst->getLength());
      if (!lenConst) {
        IF_DEBUG_MCE  { errs() << "\tskip: non-constant length!\n"; }
        continue;
      }
      size_t numBytes = lenConst->getZExtValue();
      auto * srcBase = deriveBase(srcVal, numBytes);
      auto * destBase = deriveBase(destVal, numBytes);
      if (!srcBase || !destBase) {
        IF_DEBUG_MCE  { errs() << "\tskip: could not derive suiteble base pointers!\n"; }
        continue;
      }

    // derive a common field-aligned type of both base pointers
      auto * srcGep = llvm::cast<llvm::GetElementPtrInst>(srcBase);
      auto * dstGep = llvm::cast<llvm::GetElementPtrInst>(destBase);
      auto * commonTy = deriveCommonType(srcGep->getSourceElementType(), dstGep->getSourceElementType(), numBytes);
      if (!commonTy) {
        IF_DEBUG_MCE  { errs() << "\tskip: could not derive a common base type!\n"; }
        continue;
      }
      IF_DEBUG_MCE  { errs() << "\tskip: common base type: " << *commonTy << "\n"; }

    // all checks passed -> create code
      IRBuilder<> builder(&BB, Inst.getIterator());
      auto * srcPtr = createBaseGEP(srcBase, commonTy, builder);
      auto * destPtr = createBaseGEP(destBase, commonTy, builder);
      if (!srcPtr || !destPtr) {
        continue;
      }

      IF_DEBUG_MCE  { errs() << "OK base gep src: " << *srcPtr << "   " << "base gep dest: " << *destPtr << "\n"; }
      assert(srcPtr->getType() == destPtr->getType());

      lowerMemCopy(destPtr, srcPtr, commonTy, builder, numBytes);

      killVec.push_back(mcInst);
    }
  }
  IF_DEBUG_MCE { errs() << "-- end of memCopy elision log --\n"; }

  bool changed = killVec.size() > 0;
  for (auto * mcInst : killVec) {
    // has been lowered -> erase
    mcInst->eraseFromParent();
  }

  return changed;
}
