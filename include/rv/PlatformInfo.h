//
// Created by thorsten on 06.10.16.
//

#ifndef RV_PLATFORMINFO_H
#define RV_PLATFORMINFO_H

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include <rv/vectorMapping.h>

namespace rv {

struct VecDesc {
  const char *scalarFnName;
  const char *vectorFnName;
  unsigned vectorWidth;
};

using VectorFuncMap = std::map<const llvm::Function *, const VectorMapping *>;
using VecDescVector = std::vector<VecDesc>;

class PlatformInfo {
public:
  PlatformInfo(llvm::Module &mod, llvm::TargetTransformInfo *TTI,
               llvm::TargetLibraryInfo *TLI);
  ~PlatformInfo();

  void addMapping(const llvm::Function *function, const VectorMapping *mapping);

  void removeMappingIfPresent(const llvm::Function *function);
  const VectorMapping *
  getMappingByFunction(const llvm::Function *function) const;

  void setTTI(llvm::TargetTransformInfo *TTI);
  void setTLI(llvm::TargetLibraryInfo *TLI);

  llvm::TargetTransformInfo *getTTI();
  llvm::TargetLibraryInfo *getTLI();

  // add a batch of SIMD function mappings to this platform
  // these will be used during code generation
  // if @givePrecedence is true prefer these new mappings over existing ones (the opposite if !givePrecedence)
  void addVectorizableFunctions(llvm::ArrayRef<VecDesc> funcs, bool givePrecedence);
  bool isFunctionVectorizable(llvm::StringRef funcName, unsigned vectorWidth);

  llvm::StringRef getVectorizedFunction(llvm::StringRef func,
                                        unsigned vectorWidth,
                                        bool *isInTLI = nullptr);

  llvm::Function *requestVectorizedFunction(llvm::StringRef funcName,
                                            unsigned vectorWidth,
                                            llvm::Module *insertInto,
                                            bool doublePrecision);

  VectorFuncMap &getFunctionMappings() { return funcMappings; }

  llvm::Module &getModule() const { return mod; }
  llvm::LLVMContext &getContext() const { return mod.getContext(); }

  // add a new SIMD function mapping
  bool addSIMDMapping(rv::VectorMapping &mapping);

  bool addSIMDMapping(const llvm::Function &scalarFunction,
                      const llvm::Function &simdFunction,
                      const int maskPosition, const bool mayHaveSideEffects);

  const llvm::DataLayout &getDataLayout() const { return mod.getDataLayout(); }

  llvm::Function *requestMaskReductionFunc(const std::string &name);
  llvm::Function *requestVectorMaskReductionFunc(const std::string &name, size_t width);

private:
  VectorMapping *inferMapping(llvm::Function &scalarFnc,
                              llvm::Function &simdFnc, int maskPos);

  llvm::Module &mod;
  llvm::TargetTransformInfo *mTTI;
  llvm::TargetLibraryInfo *mTLI;
  VectorFuncMap funcMappings;
  VecDescVector commonVectorMappings;
};
}

#endif // RV_PLATFORMINFO_H
