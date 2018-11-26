#ifndef RV_SHAPE_VECTORSHAPETRANSFORMER_H
#define RV_SHAPE_VECTORSHAPETRANSFORMER_H

#include "rv/shape/vectorShape.h"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Value.h>
#include <llvm/ADT/SmallVector.h>


namespace rv {
  using SmallValVec = llvm::SmallVector<const llvm::Value*, 2>;
  class VectorizationInfo;
  class PlatformInfo;

  struct VectorShapeTransformer {
    PlatformInfo & platInfo;
    const VectorizationInfo & vecInfo;
    VectorShape getShape(const llvm::Value & val) const;

  public:
    VectorShapeTransformer(PlatformInfo & _platInfo, const VectorizationInfo & _vecInfo)
    : vecInfo(_vecInfo)
    , platInfo(_platInfo)
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
  };
}


#endif // RV_SHAPE_VECTORSHAPETRANSFORMER_H
