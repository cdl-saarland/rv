//
// Created by thorsten on 06.10.16.
//

#include <llvm/Support/SourceMgr.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/IR/InstIterator.h>
#include "PlatformInfo.h"

#define SLEEF_FILES RV_SLEEF_BC_DIR
#define SLEEF_AVX2 SLEEF_FILES"/avx2_sleef.bc"
#define SLEEF_AVX SLEEF_FILES"/avx_sleef.bc"
#define SLEEF_SSE SLEEF_FILES"/sse_sleef.bc"

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

  Module *createModuleFromFile(const std::string &fileName) {
    SMDiagnostic diag;
    auto modPtr = llvm::parseIRFile(fileName, diag, llvm::getGlobalContext());
    return modPtr.release();
  }

  Function *cloneFunctionIntoModule(Function *func, Module *cloneInto, StringRef name) {
    // create function in new module, create the argument mapping, clone function into new function body, return
    Function *clonedFn = Function::Create(func->getFunctionType(), Function::LinkageTypes::ExternalLinkage,
                                          name, cloneInto);

    ValueToValueMapTy VMap;
    auto CI = clonedFn->arg_begin();
    for (auto I = func->arg_begin(), E = func->arg_end(); I != E; ++I, ++CI) {
      Argument *arg = &*I, *carg = &*CI;
      carg->setName(arg->getName());
      VMap[arg] = carg;
    }
    // need to map calls as well
    for (auto I = inst_begin(func), E = inst_end(func); I != E; ++I) {
      if (!isa<CallInst>(&*I)) continue;
      CallInst *callInst = cast<CallInst>(&*I);
      Function *callee = callInst->getCalledFunction();
      Function *clonedCallee;
      if (callee->isIntrinsic())
        clonedCallee = Intrinsic::getDeclaration(cloneInto, callee->getIntrinsicID());
      else
        clonedCallee = cloneInto->getFunction(callee->getName());

      if (!clonedCallee) clonedCallee = cloneFunctionIntoModule(callee, cloneInto, callee->getName());
      VMap[callee] = clonedCallee;
    }

    SmallVector<ReturnInst *, 1> Returns; // unused

    CloneFunctionInto(clonedFn, func, VMap, false, Returns);
    return clonedFn;
  }

  Function *PlatformInfo::requestVectorizedFunction(StringRef funcName, unsigned vectorWidth, Module *insertInto) {
    StringRef vecFuncName = getVectorizedFunction(funcName, vectorWidth);
    if (vecFuncName.empty()) return nullptr;

    // if function already cloned, return
    Function *clonedFn = insertInto->getFunction(vecFuncName);
    if (clonedFn) return clonedFn;

    // load module and function, copy function to insertInto, return copy
    if (vecFuncName.count("avx2")) { // avx2
      if (!avx2Mod) avx2Mod = createModuleFromFile(SLEEF_AVX2);
      Function *vecFunc = avx2Mod->getFunction("x" + funcName.str()); // sleef naming: xlog, xtan, xsin, etc
      assert(vecFunc);
      clonedFn = cloneFunctionIntoModule(vecFunc, insertInto, vecFuncName);

    } else if (vecFuncName.count("avx")) { // avx
      if (!avxMod) avxMod = createModuleFromFile(SLEEF_AVX);
      Function *vecFunc = avxMod->getFunction("x" + funcName.str());
      assert(vecFunc);
      clonedFn = cloneFunctionIntoModule(vecFunc, insertInto, vecFuncName);

    } else if (vecFuncName.count("sse")) { // sse
      if (!sseMod) sseMod = createModuleFromFile(SLEEF_SSE);
      Function *vecFunc = sseMod->getFunction("x" + funcName.str());
      assert(vecFunc);
      clonedFn = cloneFunctionIntoModule(vecFunc, insertInto, vecFuncName);
    }
    return clonedFn;
  }

}
