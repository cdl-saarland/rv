//===- rv/PlatformInfo.h - target info&function environment --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


#ifndef RV_PLATFORMINFO_H
#define RV_PLATFORMINFO_H

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "rv/vectorMapping.h"
#include "rv/resolver/resolver.h"
#include "rv/intrinsics.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Module.h"

namespace rv {

class ListResolver;

class PlatformInfo {
  void registerDeclareSIMDFunction(llvm::Function & F);
  void addIntrinsicMappings();

public:
  PlatformInfo(llvm::Module &mod, llvm::TargetTransformInfo *TTI,
               llvm::TargetLibraryInfo *TLI);
  ~PlatformInfo();

  // insert a new function resolver into the resolver chain
  void addResolverService(std::unique_ptr<ResolverService>&& newResolver, bool givePrecedence);

  std::unique_ptr<FunctionResolver>
  getResolver(llvm::StringRef funcName,
              llvm::FunctionType & scaFuncTy,
              const VectorShapeVec & argShapes,
              int vectorWidth,
              bool hasPredicate) const;

  llvm::Module &getModule() const { return mod; }
  const llvm::DataLayout &getDataLayout() const { return mod.getDataLayout(); }

  // FIXME use RVIntrinsic instead
  // materialize a declaration for \p rvIntrin and register the appropriate mappings.
  llvm::Function &requestRVIntrinsicFunc(RVIntrinsic rvIntrin);
  llvm::Function *requestVectorMaskReductionFunc(const std::string &name, size_t width);

  // allow quick access to the builtin resolver
  void addMapping(VectorMapping&& mapping);
  void addMapping(const VectorMapping& mapping) { addMapping(VectorMapping(mapping)); }
  void forgetAllMappingsFor(const llvm::Function & scaFunc);
  bool forgetMapping(const VectorMapping & mapping);

  void dump() const;
  void print(llvm::raw_ostream & out) const;

  // return the mangled vector function name for this target platform
  std::string createMangledVectorName(llvm::StringRef scaName, const VectorShapeVec & argShapes, int vectorWidth, int maskPos);

  // request an RV intrinsic for this module
  llvm::Function& requestIntrinsic(RVIntrinsic id, llvm::Type * DataTy = nullptr);

private:
  // Direct access to builtin list resolver.
  llvm::Module &mod;

  std::vector<std::unique_ptr<ResolverService>> resolverServices;
  ListResolver * listResolver;
};

} // namespace rv

#endif // RV_PLATFORMINFO_H
