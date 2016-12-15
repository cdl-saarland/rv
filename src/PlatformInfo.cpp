//
// Created by thorsten on 06.10.16.
//

#include "PlatformInfo.h"
#include "rv/sleefLibrary.h"

namespace rv {

  PlatformInfo::PlatformInfo(TargetTransformInfo *TTI, TargetLibraryInfo *TLI) : mTTI(TTI), mTLI(TLI), avx2Mod(0),
                                                                                 avxMod(0), sseMod(0) {}

  void PlatformInfo::addMapping(const Function *function, const rv::VectorMapping *mapping) {
    funcMappings[function] = mapping;
  }

  void PlatformInfo::removeMappingIfPresent(const Function *function) {
    auto found = funcMappings.find(function);

    if (found != funcMappings.end())
      funcMappings.erase(found);
  }

  const rv::VectorMapping *PlatformInfo::getMappingByFunction(const Function *function) const {
    auto found = funcMappings.find(function);

    if (found != funcMappings.end())
      return found->second;

    return nullptr;
  }

  void PlatformInfo::setTTI(TargetTransformInfo *TTI) {
    mTTI = TTI;
  }

  void PlatformInfo::setTLI(TargetLibraryInfo *TLI) {
    mTLI = TLI;
  }

  TargetTransformInfo *PlatformInfo::getTTI() {
    return mTTI;
  }

  TargetLibraryInfo *PlatformInfo::getTLI() {
    return mTLI;
  }

  static bool compareByScalarFnName(const VecDesc &LHS, const VecDesc &RHS) {
    return std::strncmp(LHS.scalarFnName, RHS.scalarFnName, std::strlen(RHS.scalarFnName)) < 0;
  }

  static bool compareWithScalarFnName(const VecDesc &LHS, StringRef S) {
    return std::strncmp(LHS.scalarFnName, S.data(), S.size()) < 0;
  }

  void PlatformInfo::addVectorizableFunctions(ArrayRef<VecDesc> funcs) {
    commonVectorMappings.insert(commonVectorMappings.end(), funcs.begin(), funcs.end());
    std::sort(commonVectorMappings.begin(), commonVectorMappings.end(), compareByScalarFnName);
  }

  bool PlatformInfo::isFunctionVectorizable(StringRef funcName, unsigned vectorWidth) {
    return !getVectorizedFunction(funcName, vectorWidth).empty();
  }

  StringRef PlatformInfo::getVectorizedFunction(StringRef funcName, unsigned vectorWidth) {
    if (funcName.empty())
      return funcName;

    auto I = std::lower_bound(commonVectorMappings.begin(), commonVectorMappings.end(), funcName,
                              compareWithScalarFnName);
    while (I != commonVectorMappings.end() && StringRef(I->scalarFnName) == funcName) {
      if (I->vectorWidth == vectorWidth)
        return I->vectorFnName;
      ++I;
    }
    return StringRef();
  }

  Function *PlatformInfo::requestVectorizedFunction(StringRef funcName, unsigned vectorWidth, Module *insertInto,
                                                      bool doublePrecision) {
    StringRef vecFuncName = getVectorizedFunction(funcName, vectorWidth);
    if (vecFuncName.empty()) return nullptr;
    return requestSleefFunction(funcName, vecFuncName, insertInto, doublePrecision);
  }

}
