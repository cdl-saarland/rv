//===- rv/shape/vectorShapeTransformer.h - (s,a)-lattice abstract transformers --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_SHAPE_VECTORSHAPETRANSFORMER_H
#define RV_SHAPE_VECTORSHAPETRANSFORMER_H

#include "rv/shape/vectorShape.h"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/ADT/SmallVector.h>


namespace rv {
  using SmallValVec = llvm::SmallVector<const llvm::Value*, 2>;
  class VectorizationInfo;
  class PlatformInfo;

  struct VectorShapeTransformer {
    const llvm::LoopInfo & LI;
    PlatformInfo & platInfo;
    const VectorizationInfo & vecInfo;
    VectorShape getObservedShape(const llvm::BasicBlock & observerBlock, const llvm::Value & val) const;

  public:
    VectorShapeTransformer(const llvm::LoopInfo & _LI, PlatformInfo & _platInfo, const VectorizationInfo & _vecInfo)
    : LI(_LI)
    , platInfo(_platInfo)
    , vecInfo(_vecInfo)
    {}

    /// compute the shape of the result computed by \p I.
    /// this will mark all pointer operands that are written to as varying.
    VectorShape
    computeShapeForInst(const llvm::Instruction& I, SmallValVec & taintedOps) const;

    VectorShape
    computeShapeForBinaryInst(const llvm::BinaryOperator& I) const;

    VectorShape
    computeShapeForCastInst(const llvm::CastInst& castI) const;

    VectorShape
    computeGenericArithmeticTransfer(const llvm::Instruction & I) const;

    VectorShape
    computeShapeForPHINode(const llvm::PHINode &Phi) const;
  };
}


#endif // RV_SHAPE_VECTORSHAPETRANSFORMER_H
