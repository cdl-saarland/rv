//===- sleefLibrary.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#include "rv/sleefLibrary.h"
#include <llvm/Support/SourceMgr.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/IR/InstIterator.h>

#define SLEEF_FILES RV_SLEEF_BC_DIR
#define SLEEF_AVX2_SP SLEEF_FILES"/avx2_sleef_sp.bc"
#define SLEEF_AVX_SP SLEEF_FILES"/avx_sleef_sp.bc"
#define SLEEF_SSE_SP SLEEF_FILES"/sse_sleef_sp.bc"
#define SLEEF_AVX2_DP SLEEF_FILES"/avx2_sleef_dp.bc"
#define SLEEF_AVX_DP SLEEF_FILES"/avx_sleef_dp.bc"
#define SLEEF_SSE_DP SLEEF_FILES"/sse_sleef_dp.bc"

using namespace llvm;

static Module const *avx2ModSP, *avxModSP, *sseModSP;
static Module const *avx2ModDP, *avxModDP, *sseModDP;

namespace rv {
  bool addSleefMappings(const bool useSSE, const bool useAVX, const bool useAVX2, PlatformInfo &platformInfo,
                          bool useImpreciseFunctions) {
    if (useAVX2) {
      const VecDesc VecFuncs[] = {
//          {"ldexpf", "xldexpf_avx2", 8},
//          {"sinf", "xsinf_avx2", 8},
//          {"cosf", "xcosf_avx2", 8},
//          {"sincosf", "xsincosf_avx2", 8},
//          {"tanf", "xtanf_avx2", 8},
          {"atanf", "xatanf_avx2", 8},
//          {"atan2f", "xatan2f_avx2", 8},
          {"asinf", "xasinf_avx2", 8},
          {"acosf", "xacosf_avx2", 8},
//          {"logf", "xlogf_avx2", 8},
//          {"expf", "xexpf_avx2", 8},
          {"sqrtf", "xsqrtf_avx2", 8},
//          {"cbrtf", "xcbrtf_avx2", 8},
//          {"powf", "xpowf_avx2", 8},
//          {"sinhf", "xsinhf_avx2", 8},
//          {"coshf", "xcoshf_avx2", 8},
          {"tanhf", "xtanhf_avx2", 8},
//          {"asinhf", "xasinhf_avx2", 8},
//          {"acoshf", "xacoshf_avx2", 8},
          {"atanhf", "xatanhf_avx2", 8},
          {"exp2f", "xexp2f_avx2", 8},
          {"exp10f", "xexp10f_avx2", 8},
          {"expm1f", "xexpm1f_avx2", 8},
          {"log10f", "xlog10f_avx2", 8},
          {"log1pf", "xlog1pf_avx2", 8}
      };
      platformInfo.addVectorizableFunctions(VecFuncs);

      if (useImpreciseFunctions) {
        const VecDesc ImprecVecFuncs[] = {
            {"ldexpf", "xldexpf_avx2", 8},
            {"sinf", "xsinf_avx2", 8},
            {"cosf", "xcosf_avx2", 8},
            {"sincosf", "xsincosf_avx2", 8},
            {"tanf", "xtanf_avx2", 8},
            {"atan2f", "xatan2f_avx2", 8},
            {"logf", "xlogf_avx2", 8},
            {"expf", "xexpf_avx2", 8},
            {"cbrtf", "xcbrtf_avx2", 8},
            {"powf", "xpowf_avx2", 8},
            {"sinhf", "xsinhf_avx2", 8},
            {"coshf", "xcoshf_avx2", 8},
            {"asinhf", "xasinhf_avx2", 8},
            {"acoshf", "xacoshf_avx2", 8}
        };
        platformInfo.addVectorizableFunctions(ImprecVecFuncs);
      }
    }

    if (useAVX) {
      const VecDesc VecFuncs[] = {
//          {"ldexpf", "xldexpf_avx", 8},
//          {"sinf", "xsinf_avx", 8},
//          {"cosf", "xcosf_avx", 8},
//          {"sincosf", "xsincosf_avx", 8},
//          {"tanf", "xtanf_avx", 8},
          {"atanf", "xatanf_avx", 8},
//          {"atan2f", "xatan2f_avx", 8},
          {"asinf", "xasinf_avx", 8},
          {"acosf", "xacosf_avx", 8},
//          {"logf", "xlogf_avx", 8},
//          {"expf", "xexpf_avx", 8},
          {"sqrtf", "xsqrtf_avx", 8},
//          {"cbrtf", "xcbrtf_avx", 8},
//          {"powf", "xpowf_avx", 8},
//          {"sinhf", "xsinhf_avx", 8},
//          {"coshf", "xcoshf_avx", 8},
          {"tanhf", "xtanhf_avx", 8},
//          {"asinhf", "xasinhf_avx", 8},
//          {"acoshf", "xacoshf_avx", 8},
          {"atanhf", "xatanhf_avx", 8},
          {"exp2f", "xexp2f_avx", 8},
          {"exp10f", "xexp10f_avx", 8},
          {"expm1f", "xexpm1f_avx", 8},
          {"log10f", "xlog10f_avx", 8},
          {"log1pf", "xlog1pf_avx", 8},
      };
      platformInfo.addVectorizableFunctions(VecFuncs);

      if (useImpreciseFunctions) {
        const VecDesc ImprecVecFuncs[] = {
            {"ldexpf", "xldexpf_avx", 8},
            {"sinf", "xsinf_avx", 8},
            {"cosf", "xcosf_avx", 8},
            {"sincosf", "xsincosf_avx", 8},
            {"tanf", "xtanf_avx", 8},
            {"atan2f", "xatan2f_avx", 8},
            {"logf", "xlogf_avx", 8},
            {"expf", "xexpf_avx", 8},
            {"cbrtf", "xcbrtf_avx", 8},
            {"powf", "xpowf_avx", 8},
            {"sinhf", "xsinhf_avx", 8},
            {"coshf", "xcoshf_avx", 8},
            {"asinhf", "xasinhf_avx", 8},
            {"acoshf", "xacoshf_avx", 8}
        };
        platformInfo.addVectorizableFunctions(ImprecVecFuncs);
      }
    }

    if (useSSE || useAVX || useAVX2) {
      const VecDesc VecFuncs[] = {
//          {"ldexpf", "xldexpf_sse", 4},
//          {"sinf", "xsinf_sse", 4},
//          {"cosf", "xcosf_sse", 4},
//          {"sincosf", "xsincosf_sse", 4},
//          {"tanf", "xtanf_sse", 4},
          {"atanf", "xatanf_sse", 4},
//          {"atan2f", "xatan2f_sse", 4},
          {"asinf", "xasinf_sse", 4},
          {"acosf", "xacosf_sse", 4},
//          {"logf", "xlogf_sse", 4},
//          {"expf", "xexpf_sse", 4},
          {"sqrtf", "xsqrtf_sse", 4},
//          {"cbrtf", "xcbrtf_sse", 4},
//          {"powf", "xpowf_sse", 4},
//          {"sinhf", "xsinhf_sse", 4},
//          {"coshf", "xcoshf_sse", 4},
          {"tanhf", "xtanhf_sse", 4},
//          {"asinhf", "xasinhf_sse", 4},
//          {"acoshf", "xacoshf_sse", 4},
          {"atanhf", "xatanhf_sse", 4},
          {"exp2f", "xexp2f_sse", 4},
          {"exp10f", "xexp10f_sse", 4},
          {"expm1f", "xexpm1f_sse", 4},
          {"log10f", "xlog10f_sse", 4},
          {"log1pf", "xlog1pf_sse", 4}
      };
      platformInfo.addVectorizableFunctions(VecFuncs);

      if (useImpreciseFunctions) {
        const VecDesc ImprecVecFuncs[] = {
            {"ldexpf", "xldexpf_sse", 8},
            {"sinf", "xsinf_sse", 8},
            {"cosf", "xcosf_sse", 8},
            {"sincosf", "xsincosf_sse", 8},
            {"tanf", "xtanf_sse", 8},
            {"atan2f", "xatan2f_sse", 8},
            {"logf", "xlogf_sse", 8},
            {"expf", "xexpf_sse", 8},
            {"cbrtf", "xcbrtf_sse", 8},
            {"powf", "xpowf_sse", 8},
            {"sinhf", "xsinhf_sse", 8},
            {"coshf", "xcoshf_sse", 8},
            {"asinhf", "xasinhf_sse", 8},
            {"acoshf", "xacoshf_sse", 8}
        };
        platformInfo.addVectorizableFunctions(ImprecVecFuncs);
      }
    }
    return useAVX || useAVX2 || useSSE;
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

  Function *
  requestSleefFunction(const StringRef &funcName, StringRef &vecFuncName, Module *insertInto, bool doublePrecision) {
    // if function already cloned, return
    std::string clonedString = vecFuncName.str() + (doublePrecision ? "_dp" : "_sp");
    StringRef clonedName(clonedString); // no idea why we need the std::string first, but otherwise it corrupts
    Function *clonedFn = insertInto->getFunction(clonedName);
    if (clonedFn) return clonedFn;

    // load module and function, copy function to insertInto, return copy
    if (doublePrecision) {
      if (clonedName.count("avx2")) { // avx2
        if (!avx2ModDP) avx2ModDP = createModuleFromFile(SLEEF_AVX2_DP);
        Function *vecFunc = avx2ModDP->getFunction("x" + funcName.str()); // sleef naming: xlog, xtan, xsin, etc
        assert(vecFunc);
        clonedFn = cloneFunctionIntoModule(vecFunc, insertInto, clonedName);

      } else if (clonedName.count("avx")) { // avx
        if (!avxModDP) avxModDP = createModuleFromFile(SLEEF_AVX_DP);
        Function *vecFunc = avxModDP->getFunction("x" + funcName.str());
        assert(vecFunc);
        clonedFn = cloneFunctionIntoModule(vecFunc, insertInto, clonedName);

      } else if (clonedName.count("sse")) { // sse
        if (!sseModDP) sseModDP = createModuleFromFile(SLEEF_SSE_DP);
        Function *vecFunc = sseModDP->getFunction("x" + funcName.str());
        assert(vecFunc);
        clonedFn = cloneFunctionIntoModule(vecFunc, insertInto, clonedName);
      }
    } else {
      if (clonedName.count("avx2")) { // avx2
        if (!avx2ModSP) avx2ModSP = createModuleFromFile(SLEEF_AVX2_SP);
        Function *vecFunc = avx2ModSP->getFunction("x" + funcName.str()); // sleef naming: xlog, xtan, xsin, etc
        assert(vecFunc);
        clonedFn = cloneFunctionIntoModule(vecFunc, insertInto, clonedName);

      } else if (clonedName.count("avx")) { // avx
        if (!avxModSP) avxModSP = createModuleFromFile(SLEEF_AVX_SP);
        Function *vecFunc = avxModSP->getFunction("x" + funcName.str());
        assert(vecFunc);
        clonedFn = cloneFunctionIntoModule(vecFunc, insertInto, clonedName);

      } else if (clonedName.count("sse")) { // sse
        if (!sseModSP) sseModSP = createModuleFromFile(SLEEF_SSE_SP);
        Function *vecFunc = sseModSP->getFunction("x" + funcName.str());
        assert(vecFunc);
        clonedFn = cloneFunctionIntoModule(vecFunc, insertInto, clonedName);
      }
    }
    return clonedFn;
  }
}