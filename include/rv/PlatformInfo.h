//
// Created by thorsten on 06.10.16.
//

#ifndef RV_PLATFORMINFO_H
#define RV_PLATFORMINFO_H

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "rv/vectorMapping.h"
#include "rv/resolver/resolver.h"
#include "llvm/ADT/SmallVector.h"

namespace rv {

class ListResolver;

class PlatformInfo {
  void registerDeclareSIMDFunction(llvm::Function & F);
  void addIntrinsicMappings();

public:
  PlatformInfo(llvm::Module &mod, llvm::TargetTransformInfo *TTI,
               llvm::TargetLibraryInfo *TLI);
  ~PlatformInfo();

  void setTTI(llvm::TargetTransformInfo *TTI);
  void setTLI(llvm::TargetLibraryInfo *TLI);

  llvm::TargetTransformInfo *getTTI();
  llvm::TargetLibraryInfo *getTLI();

  // insert a new function resolver into the resolver chain
  void addResolverService(std::unique_ptr<ResolverService>&& newResolver, bool givePrecedence);

  std::unique_ptr<FunctionResolver>
  getResolver(llvm::StringRef funcName,
              llvm::FunctionType & scaFuncTy,
              const VectorShapeVec & argShapes,
              int vectorWidth) const;

  llvm::Module &getModule() const { return mod; }
  llvm::LLVMContext &getContext() const { return mod.getContext(); }

  const llvm::DataLayout &getDataLayout() const { return mod.getDataLayout(); }

  llvm::Function *requestMaskReductionFunc(const std::string &name);
  llvm::Function *requestVectorMaskReductionFunc(const std::string &name, size_t width);

  size_t getMaxVectorWidth() const;
  size_t getMaxVectorBits() const;

  // allow quick access to the builtin resolver
  ListResolver& getListResolver() { return *listResolver; }
  void addMapping(VectorMapping&& mapping);
  void addMapping(const VectorMapping& mapping) { addMapping(VectorMapping(mapping)); }
  void forgetAllMappingsFor(const llvm::Function & scaFunc);
  void forgetMapping(const VectorMapping & mapping);

  void dump() const;
  void print(llvm::raw_ostream & out) const;
private:
  // Direct access to builtin list resolver.
  llvm::Module &mod;
  llvm::TargetTransformInfo *mTTI;
  llvm::TargetLibraryInfo *mTLI;
  std::vector<std::unique_ptr<ResolverService>> resolverServices;
  ListResolver * listResolver;
};

} // namespace rv

#endif // RV_PLATFORMINFO_H
