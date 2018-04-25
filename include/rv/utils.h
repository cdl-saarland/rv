#ifndef RV_UTILS_H
#define RV_UTILS_H

#include "rv/vectorShape.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

namespace rv {

llvm::Type*
vectorizeType(llvm::Type* scalarTy, rv::VectorShape shape, unsigned vectorWidth);

llvm::Function*
createVectorDeclaration(llvm::Function& scalarFn, VectorShape resShape,
                        const VectorShapeVec& argShapes, unsigned vectorWidth,
                        int maskPos = -1);

} // namespace rv

#endif // RV_UTILS_H
