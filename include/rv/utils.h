#ifndef RV_UTILS_H
#define RV_UTILS_H

#include "rv/vectorShape.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"

#include "llvm/Transforms/Utils/ValueMapper.h"

namespace rv {

struct VectorMapping;

llvm::Type*
vectorizeType(llvm::Type* scalarTy, rv::VectorShape shape, unsigned vectorWidth);

llvm::Function*
createVectorDeclaration(llvm::Function& scalarFn, VectorShape resShape,
                        const VectorShapeVec& argShapes, unsigned vectorWidth,
                        int maskPos = -1);

// parse an omp 4 X86DeclareSIMD signature
bool
parseVectorMapping(llvm::Function & scalarFn, llvm::StringRef & attribText, VectorMapping & mapping, bool createMissingDecl);

template<class T>
inline
T&
LookUp(llvm::ValueToValueMapTy & valMap, T& key) {
  return *llvm::cast<T>(valMap[&key]);
}

} // namespace rv

#endif // RV_UTILS_H
