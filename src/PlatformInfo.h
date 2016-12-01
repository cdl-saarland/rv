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

  class PlatformInfo {
    typedef std::map<const Function *, const VectorMapping *> FuncToVecMapping;
  public:
    PlatformInfo(TargetTransformInfo *TTI, TargetLibraryInfo *TLI);

    void addMapping(const Function *function, const VectorMapping *mapping);

    void removeMappingIfPresent(const Function *function);
    const VectorMapping *getMappingByFunction(const Function *function) const;

    void setTTI(TargetTransformInfo *TTI);
    void setTLI(TargetLibraryInfo *TLI);

    TargetTransformInfo *getTTI();
    TargetLibraryInfo *getTLI();

    void addVectorizableFunctions(ArrayRef<VecDesc> funcs);
    bool isFunctionVectorizable(StringRef funcName, unsigned vectorWidth);
    StringRef getVectorizedFunction(StringRef func, unsigned vectorWidth);
    Function *requestVectorizedFunction(StringRef funcName, unsigned vectorWidth, Module *insertInto);

  private:
    TargetTransformInfo *mTTI;
    TargetLibraryInfo *mTLI;
    Module *avx2Mod, *avxMod, *sseMod;
    FuncToVecMapping funcMappings;
    std::vector<VecDesc> commonVectorMappings;
  };

}

#endif // RV_PLATFORMINFO_H
