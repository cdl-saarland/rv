#ifndef RV_UTILS_H
#define RV_UTILS_H

#include "rv/vectorShape.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

namespace rv {

class VectorMapping;

llvm::Type*
vectorizeType(llvm::Type* scalarTy, rv::VectorShape shape, unsigned vectorWidth);

llvm::Function*
createVectorDeclaration(llvm::Function& scalarFn, VectorShape resShape,
                        const VectorShapeVec& argShapes, unsigned vectorWidth,
                        int maskPos = -1);

// parse an omp 4 X86DeclareSIMD signature
bool
parseVectorMapping(llvm::Function & scalarFn, llvm::StringRef & attribText, VectorMapping & mapping, bool createMissingDecl);


} // namespace rv

#endif // RV_UTILS_H
