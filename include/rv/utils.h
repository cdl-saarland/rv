//===- rv/utils.h - asorted auxiliary functions --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_UTILS_H
#define RV_UTILS_H

#include "rv/shape/vectorShape.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

namespace rv {

struct VectorMapping;
class PlatformInfo;

void
MaterializeEntryMask(llvm::Function &F, rv::PlatformInfo &platInfo);

llvm::Type*
vectorizeType(llvm::Type* scalarTy, VectorShape shape, unsigned vectorWidth);

llvm::Function*
createVectorDeclaration(llvm::Function& scalarFn, VectorShape resShape,
                        const VectorShapeVec& argShapes, unsigned vectorWidth,
                        int maskPos);

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
