//
// Created by thorsten on 06.10.16.
//

#ifndef RV_PLATFORMINFO_H
#define RV_PLATFORMINFO_H

#include <rv/vectorMapping.h>
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Analysis/TargetLibraryInfo.h"

using namespace llvm;

namespace rv {

  struct VecDesc {
    const char *scalarFnName;
    const char *vectorFnName;
    unsigned vectorWidth;
  };

  typedef std::map<const Function *, const VectorMapping *> VectorFuncMap;

  class PlatformInfo {
  public:
    PlatformInfo(Module & mod, TargetTransformInfo *TTI, TargetLibraryInfo *TLI);
    ~PlatformInfo();

    void addMapping(const Function *function, const VectorMapping *mapping);

    void removeMappingIfPresent(const Function *function);
    const VectorMapping *getMappingByFunction(const Function *function) const;

    void setTTI(TargetTransformInfo *TTI);
    void setTLI(TargetLibraryInfo *TLI);

    TargetTransformInfo *getTTI();
    TargetLibraryInfo *getTLI();

    void addVectorizableFunctions(ArrayRef<VecDesc> funcs);
    bool isFunctionVectorizable(StringRef funcName, unsigned vectorWidth);
    StringRef getVectorizedFunction(StringRef func, unsigned vectorWidth, bool *isInTLI = nullptr);
    Function *requestVectorizedFunction(StringRef funcName, unsigned vectorWidth, Module *insertInto,
                                            bool doublePrecision);

    VectorFuncMap & getFunctionMappings() { return funcMappings; }

    Module & getModule() const { return mod; }
    LLVMContext & getContext() const { return mod.getContext(); }

    // add a new SIMD function mapping
    bool addSIMDMapping(rv::VectorMapping & mapping);

    bool addSIMDMapping(const Function& scalarFunction,
                        const Function& simdFunction,
                        const int       maskPosition,
                        const bool      mayHaveSideEffects);

    const DataLayout & getDataLayout() const { return mod.getDataLayout(); }
  private:
    VectorMapping * inferMapping(llvm::Function & scalarFnc, llvm::Function & simdFnc, int maskPos);

    Module & mod;
    TargetTransformInfo *mTTI;
    TargetLibraryInfo *mTLI;
    VectorFuncMap funcMappings;
    std::vector<VecDesc> commonVectorMappings;
  };

}

#endif // RV_PLATFORMINFO_H
