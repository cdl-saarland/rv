//
// Created by thorsten on 06.10.16.
//

#ifndef RV_PLATFORMINFO_H
#define RV_PLATFORMINFO_H

#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include <rv/vectorMapping.h>
#include "llvm/ADT/SmallVector.h"

namespace rv {

// resolver service
class
FunctionResolver {
protected:
  llvm::Module & targetModule;

public:
  static VectorShape ComputeShape(const VectorShapeVec & argShapes);

  FunctionResolver(llvm::Module & _targetModule)
  : targetModule(_targetModule)
  {}

  virtual ~FunctionResolver() {}

  // materialized the vectorized function in the module @insertInto and returns a reference to it
  virtual llvm::Function& requestVectorized() = 0;

  // result shape of function @funcName in target module @module
  virtual VectorShape requestResultShape() = 0;
};

// abstract function resolver interface
class
ResolverService {
public:
  virtual std::unique_ptr<FunctionResolver> resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule) = 0;
};


// used for shape-based call mappings
using VecMappingShortVec = llvm::SmallVector<VectorMapping, 4>;
using VectorFuncMap = std::map<const llvm::Function *, VecMappingShortVec*>;

class PlatformInfo {
  std::vector<std::unique_ptr<ResolverService>> resolverServices;
  void registerDeclareSIMDFunction(llvm::Function & F);

public:
  PlatformInfo(llvm::Module &mod, llvm::TargetTransformInfo *TTI,
               llvm::TargetLibraryInfo *TLI);
  ~PlatformInfo();

  bool addMapping(VectorMapping & mapping);

  const VectorMapping *
  getMappingByFunction(const llvm::Function *function) const;

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

#if 0
  llvm::StringRef getVectorizedFunction(llvm::StringRef func,
                                        unsigned vectorWidth,
                                        bool *isInTLI = nullptr);

  llvm::Function *requestVectorizedFunction(llvm::StringRef funcName,
                                            unsigned vectorWidth,
                                            llvm::Module *insertInto,
                                            bool doublePrecision);
#endif

  // query available vector mappings for a given vector call signature
  bool
  getMappingsForCall(VecMappingShortVec & possibleMappings, const llvm::Function & scalarFn, const VectorShapeVec & argShapes, unsigned vectorWidth, bool needsPredication);

  VectorFuncMap &getFunctionMappings() { return funcMappings; }

  llvm::Module &getModule() const { return mod; }
  llvm::LLVMContext &getContext() const { return mod.getContext(); }

  bool addSIMDMapping(const llvm::Function &scalarFunction,
                      const llvm::Function &simdFunction,
                      const int maskPosition, const bool mayHaveSideEffects);

  const llvm::DataLayout &getDataLayout() const { return mod.getDataLayout(); }

  llvm::Function *requestMaskReductionFunc(const std::string &name);
  llvm::Function *requestVectorMaskReductionFunc(const std::string &name, size_t width);

  size_t getMaxVectorWidth() const;
  size_t getMaxVectorBits() const;

private:
  VectorMapping inferMapping(llvm::Function &scalarFnc,
                              llvm::Function &simdFnc, int maskPos);

  llvm::Module &mod;
  llvm::TargetTransformInfo *mTTI;
  llvm::TargetLibraryInfo *mTLI;
  VectorFuncMap funcMappings;
};
}

#endif // RV_PLATFORMINFO_H
