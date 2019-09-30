//===- rv/resolver/listResolver.h - mapping-based resolver --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_RESOLVER_LISTRESOLVER_H
#define RV_RESOLVER_LISTRESOLVER_H

#include "rv/resolver/resolver.h"
#include "rv/vectorMapping.h"
#include "llvm/ADT/SmallVector.h"
#include <memory>
#include <map>

namespace llvm {
  class Function;
}

namespace rv {

// Classic list-based vector function resolver.
using VecMappingShortVec = llvm::SmallVector<VectorMapping, 4>;
#if 0
struct DestFunctions {
  VecMappingShortVec predicated;
  VecMappingShortVec unpredicated;
};
#endif

using VectorFuncMap = std::map<const llvm::Function *, std::unique_ptr<VecMappingShortVec>>;

class ListResolver : public ResolverService {
  VectorMapping inferMapping(llvm::Function &scalarFnc,
                              llvm::Function &simdFnc, int maskPos);
  void ForAll_MappingsForCall(std::function<bool(const VectorMapping &)> MatchFunc, const llvm::Function & scalarFn, const VectorShapeVec & argShapes, unsigned vectorWidth, bool needsPredication);

public:
  ListResolver(llvm::Module & destModule)
  : destModule(destModule)
  {}

  ~ListResolver();

  /// Construction and add a vector mapping from the provided arguments.
  bool addSIMDMapping(const llvm::Function &scalarFunction,
                      const llvm::Function &simdFunction,
                      const int maskPosition, const bool mayHaveSideEffects);

  /// Add mapping \p mapping to vector function mappings.
  bool addMapping(VectorMapping&& mapping);

  /// Drop all listed mapping associated with the scalar function \p scaFunc.
  void forgetAllMappingsFor(const llvm::Function & scaFunc);
  /// Forget one specific mapping.
  bool forgetMapping(const VectorMapping & mapping);

  void print(llvm::raw_ostream & out) const override;

  std::unique_ptr<FunctionResolver> resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, bool hasPredication, llvm::Module & destModule) override;

private:
  llvm::Module & destModule;
  VectorFuncMap funcMappings;
};

} // namespace rv

#endif // RV_RESOLVER_LISTRESOLVER_H
