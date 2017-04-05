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

#include "utils/rvTools.h"

using namespace llvm;

#ifdef RV_ENABLE_BUILTINS
  extern const unsigned char * avx2_sp_Buffer;
  extern const size_t avx2_sp_BufferLen;

  extern const unsigned char * avx_sp_Buffer;
  extern const size_t avx_sp_BufferLen;

  extern const unsigned char * sse_sp_Buffer;
  extern const size_t sse_sp_BufferLen;

  extern const unsigned char * avx2_dp_Buffer;
  extern const size_t avx2_dp_BufferLen;

  extern const unsigned char * avx_dp_Buffer;
  extern const size_t avx_dp_BufferLen;

  extern const unsigned char * sse_dp_Buffer;
  extern const size_t sse_dp_BufferLen;
#endif


namespace rv {
  enum SleefISA {
    SLEEF_AVX2 = 0,
    SLEEF_AVX  = 1,
    SLEEF_SSE  = 2
  };

  inline int sleefModuleIndex(SleefISA isa, bool doublePrecision) {
    return int(isa) + (doublePrecision ? 3 : 0);
  }

#ifdef RV_ENABLE_BUILTINS
  static const size_t sleefModuleBufferLens[] = {
    avx2_sp_BufferLen,
    avx_sp_BufferLen,
    sse_sp_BufferLen,

    avx2_dp_BufferLen,
    avx_dp_BufferLen,
    sse_dp_BufferLen,
  };

  static const unsigned char** sleefModuleBuffers[] = {
    &avx2_sp_Buffer,
    &avx_sp_Buffer,
    &sse_sp_Buffer,
    &avx2_dp_Buffer,
    &avx_dp_Buffer,
    &sse_dp_Buffer,
  };

#else // ! RV_ENABLE_SLEEF
  static const size_t sleefModuleBufferLens[] = {
    0,0,0,0,0,0
  };

  static const char** sleefModuleBuffers[] = {
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr,
    nullptr
  };
#endif

  static Module const *sleefModules[3 * 2];

  bool addSleefMappings(const bool useSSE,
                        const bool useAVX,
                        const bool useAVX2,
                        PlatformInfo &platformInfo,
                        bool useImpreciseFunctions) {
#ifdef RV_ENABLE_BUILTINS
    if (useAVX2) {
      const VecDesc VecFuncs[] = {
        {"atanf", "xatanf_avx2", 8},
        {"asinf", "xasinf_avx2", 8},
        {"acosf", "xacosf_avx2", 8},
        {"sqrtf", "xsqrtf_avx2", 8},
        {"tanhf", "xtanhf_avx2", 8},
        {"atanhf", "xatanhf_avx2", 8},
        {"exp2f", "xexp2f_avx2", 8},
        {"exp10f", "xexp10f_avx2", 8},
        {"expm1f", "xexpm1f_avx2", 8},
        {"log10f", "xlog10f_avx2", 8},
        {"log1pf", "xlog1pf_avx2", 8},
        {"atan", "xatan_avx2", 4},
        {"asin", "xasin_avx2", 4},
        {"acos", "xacos_avx2", 4},
        {"sqrt", "xsqrt_avx2", 4},
        {"tanh", "xtanh_avx2", 4},
        {"atanh", "xatanh_avx2", 4},
        {"exp2", "xexp2_avx2", 4},
        {"exp10", "xexp10_avx2", 4},
        {"expm1", "xexpm1_avx2", 4},
        {"log10", "xlog10_avx2", 4},
        {"log1p", "xlog1p_avx2", 4},

        {"llvm.sqrt.f32", "xsqrtf_avx2", 8},
        {"llvm.exp2.f32", "xexp2f_avx2", 8},
        {"llvm.log10.f32", "xlog10f_avx2", 8},
        {"llvm.sqrt.f64", "xsqrt_avx2", 4},
        {"llvm.exp2.f64", "xexp2_avx2", 4},
        {"llvm.log10.f64", "xlog10_avx2", 4}
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
          {"acoshf", "xacoshf_avx2", 8},
          {"ldexp", "xldexp_avx2", 4},
          {"sin", "xsin_avx2", 4},
          {"cos", "xcos_avx2", 4},
          {"sincos", "xsincos_avx2", 4},
          {"tan", "xtan_avx2", 4},
          {"atan2", "xatan2_avx2", 4},
          {"log", "xlog_avx2", 4},
          {"exp", "xexp_avx2", 4},
          {"cbrt", "xcbrt_avx2", 4},
          {"pow", "xpow_avx2", 4},
          {"sinh", "xsinh_avx2", 4},
          {"cosh", "xcosh_avx2", 4},
          {"asinh", "xasinh_avx2", 4},
          {"acosh", "xacosh_avx2", 4},

          {"llvm.sin.f32", "xsinf_avx2", 8},
          {"llvm.cos.f32", "xcosf_avx2", 8},
          {"llvm.log.f32", "xlogf_avx2", 8},
          {"llvm.exp.f32", "xexpf_avx2", 8},
          {"llvm.pow.f32", "xpowf_avx2", 8},
          {"llvm.sin.f64", "xsin_avx2", 4},
          {"llvm.cos.f64", "xcos_avx2", 4},
          {"llvm.log.f64", "xlog_avx2", 4},
          {"llvm.exp.f64", "xexp_avx2", 4},
          {"llvm.pow.f64", "xpow_avx2", 4}
        };
        platformInfo.addVectorizableFunctions(ImprecVecFuncs);
      }
    }

    if (useAVX || useAVX2) {
      const VecDesc VecFuncs[] = {
        {"fabsf", "xfabsf_avx", 8},
        {"copysignf", "xcopysignf_avx", 8},
        {"fminf", "xfminf_avx", 8},
        {"fmaxf", "xfmaxf_avx", 8},
        {"fabs", "xfabs_avx", 4},
        {"copysign", "xcopysign_avx", 4},
        {"fmin", "xfmin_avx", 4},
        {"fmax", "xfmax_avx", 4},

        {"llvm.fabs.f32", "xfabsf_avx", 8},
        {"llvm.copysign.f32", "xcopysignf_avx", 8},
        {"llvm.minnum.f32", "xfminf_avx", 8},
        {"llvm.maxnum.f32", "xfmaxf_avx", 8},
        {"llvm.fabs.f64", "xfabs_avx", 4},
        {"llvm.copysign.f64", "xcopysign_avx", 4},
        {"llvm.minnum.f64", "xfmin_avx", 4},
        {"llvm.maxnum.f64", "xfmax_avx", 4}
      };
      platformInfo.addVectorizableFunctions(VecFuncs);
    }

    if (useAVX) {
      const VecDesc VecFuncs[] = {
        {"atanf", "xatanf_avx", 8},
        {"asinf", "xasinf_avx", 8},
        {"acosf", "xacosf_avx", 8},
        {"sqrtf", "xsqrtf_avx", 8},
        {"tanhf", "xtanhf_avx", 8},
        {"atanhf", "xatanhf_avx", 8},
        {"exp2f", "xexp2f_avx", 8},
        {"exp10f", "xexp10f_avx", 8},
        {"expm1f", "xexpm1f_avx", 8},
        {"log10f", "xlog10f_avx", 8},
        {"log1pf", "xlog1pf_avx", 8},
        {"atan", "xatan_avx", 4},
        {"asin", "xasin_avx", 4},
        {"acos", "xacos_avx", 4},
        {"sqrt", "xsqrt_avx", 4},
        {"tanh", "xtanh_avx", 4},
        {"atanh", "xatanh_avx", 4},
        {"exp2", "xexp2_avx", 4},
        {"exp10", "xexp10_avx", 4},
        {"expm1", "xexpm1_avx", 4},
        {"log10", "xlog10_avx", 4},
        {"log1p", "xlog1p_avx", 4},

        {"llvm.sqrt.f32", "xsqrtf_avx", 8},
        {"llvm.exp2.f32", "xexp2f_avx", 8},
        {"llvm.log10.f32", "xlog10f_avx", 8},
        {"llvm.sqrt.f64", "xsqrt_avx", 4},
        {"llvm.exp2.f64", "xexp2_avx", 4},
        {"llvm.log10.f64", "xlog10_avx", 4}
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
          {"acoshf", "xacoshf_avx", 8},
          {"ldexp", "xldexp_avx", 4},
          {"sin", "xsin_avx", 4},
          {"cos", "xcos_avx", 4},
          {"sincos", "xsincos_avx", 4},
          {"tan", "xtan_avx", 4},
          {"atan2", "xatan2_avx", 4},
          {"log", "xlog_avx", 4},
          {"exp", "xexp_avx", 4},
          {"cbrt", "xcbrt_avx", 4},
          {"pow", "xpow_avx", 4},
          {"sinh", "xsinh_avx", 4},
          {"cosh", "xcosh_avx", 4},
          {"asinh", "xasinh_avx", 4},
          {"acosh", "xacosh_avx", 4},

          {"llvm.sin.f32", "xsinf_avx", 8},
          {"llvm.cos.f32", "xcosf_avx", 8},
          {"llvm.log.f32", "xlogf_avx", 8},
          {"llvm.exp.f32", "xexpf_avx", 8},
          {"llvm.pow.f32", "xpowf_avx", 8},
          {"llvm.sin.f64", "xsin_avx", 4},
          {"llvm.cos.f64", "xcos_avx", 4},
          {"llvm.log.f64", "xlog_avx", 4},
          {"llvm.exp.f64", "xexp_avx", 4},
          {"llvm.pow.f64", "xpow_avx", 4}
        };
        platformInfo.addVectorizableFunctions(ImprecVecFuncs);
      }
    }

    if (useSSE || useAVX || useAVX2) {
      const VecDesc VecFuncs[] = {
        {"atanf", "xatanf_sse", 4},
        {"asinf", "xasinf_sse", 4},
        {"acosf", "xacosf_sse", 4},
        {"sqrtf", "xsqrtf_sse", 4},
        {"tanhf", "xtanhf_sse", 4},
        {"atanhf", "xatanhf_sse", 4},
        {"exp2f", "xexp2f_sse", 4},
        {"exp10f", "xexp10f_sse", 4},
        {"expm1f", "xexpm1f_sse", 4},
        {"log10f", "xlog10f_sse", 4},
        {"log1pf", "xlog1pf_sse", 4},
        {"fabsf", "xfabsf_sse", 4},
        {"copysignf", "xcopysignf_sse", 4},
        {"fminf", "xfminf_sse", 4},
        {"fmaxf", "xfmaxf_sse", 4},
        {"atan", "xatan_sse", 2},
        {"asin", "xasin_sse", 2},
        {"acos", "xacos_sse", 2},
        {"sqrt", "xsqrt_sse", 2},
        {"tanh", "xtanh_sse", 2},
        {"atanh", "xatanh_sse", 2},
        {"exp2", "xexp2_sse", 2},
        {"exp10", "xexp10_sse", 2},
        {"expm1", "xexpm1_sse", 2},
        {"log10", "xlog10_sse", 2},
        {"log1p", "xlog1p_sse", 2},
        {"fabs", "xfabs_sse", 2},
        {"copysign", "xcopysign_sse", 2},
        {"fmin", "xfmin_sse", 2},
        {"fmax", "xfmax_sse", 2},

        {"llvm.sqrt.f32", "xsqrtf_sse", 4},
        {"llvm.exp2.f32", "xexp2f_sse", 4},
        {"llvm.log10.f32", "xlog10f_sse", 4},
        {"llvm.fabs.f32", "xfabsf_sse", 4},
        {"llvm.copysign.f32", "xcopysignf_sse", 4},
        {"llvm.fmin.f32", "xfminf_sse", 4},
        {"llvm.fmax.f32", "xfmaxf_sse", 4},
        {"llvm.sqrt.f64", "xsqrt_sse", 2},
        {"llvm.exp2.f64", "xexp2_sse", 2},
        {"llvm.log10.f64", "xlog10_sse", 2},
        {"llvm.fabs.f64", "xfabs_sse", 2},
        {"llvm.copysign.f64", "xcopysign_sse", 2},
        {"llvm.fmin.f64", "xfmin_sse", 2},
        {"llvm.fmax.f64", "xfmax_sse", 2},
      };
      platformInfo.addVectorizableFunctions(VecFuncs);

      if (useImpreciseFunctions) {
        const VecDesc ImprecVecFuncs[] = {
          {"ldexpf", "xldexpf_sse", 4},
          {"sinf", "xsinf_sse", 4},
          {"cosf", "xcosf_sse", 4},
          {"sincosf", "xsincosf_sse", 4},
          {"tanf", "xtanf_sse", 4},
          {"atan2f", "xatan2f_sse", 4},
          {"logf", "xlogf_sse", 4},
          {"expf", "xexpf_sse", 4},
          {"cbrtf", "xcbrtf_sse", 4},
          {"powf", "xpowf_sse", 4},
          {"sinhf", "xsinhf_sse", 4},
          {"coshf", "xcoshf_sse", 4},
          {"asinhf", "xasinhf_sse", 4},
          {"acoshf", "xacoshf_sse", 4},
          {"ldexp", "xldexp_sse", 2},
          {"sin", "xsin_sse", 2},
          {"cos", "xcos_sse", 2},
          {"sincos", "xsincos_sse", 2},
          {"tan", "xtan_sse", 2},
          {"atan2", "xatan2_sse", 2},
          {"log", "xlog_sse", 2},
          {"exp", "xexp_sse", 2},
          {"cbrt", "xcbrt_sse", 2},
          {"pow", "xpow_sse", 2},
          {"sinh", "xsinh_sse", 2},
          {"cosh", "xcosh_sse", 2},
          {"asinh", "xasinh_sse", 2},
          {"acosh", "xacosh_sse", 2},

          {"llvm.sin.f32", "xsinf_sse", 4},
          {"llvm.cos.f32", "xcosf_sse", 4},
          {"llvm.log.f32", "xlogf_sse", 4},
          {"llvm.exp.f32", "xexpf_sse", 4},
          {"llvm.pow.f32", "xpowf_sse", 4},
          {"llvm.sin.f64", "xsin_sse", 2},
          {"llvm.cos.f64", "xcos_sse", 2},
          {"llvm.log.f64", "xlog_sse", 2},
          {"llvm.exp.f64", "xexp_sse", 2},
          {"llvm.pow.f64", "xpow_sse", 2}
        };
        platformInfo.addVectorizableFunctions(ImprecVecFuncs);
      }
    }
    return useAVX || useAVX2 || useSSE;

#else
    return false;
#endif
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
#ifndef RV_ENABLE_BUILTINS
    assert(false && "SLEEF not builtin");
    return nullptr;
#endif
    auto & context = insertInto->getContext();
    // if function already cloned, return
    Function *clonedFn = insertInto->getFunction(vecFuncName);
    if (clonedFn) return clonedFn;

    // remove the trailing _avx2/_avx/_sse
    auto sleefName = vecFuncName.substr(0, vecFuncName.find('_'));

    SleefISA isa;
    if (vecFuncName.count("avx2"))     isa = SLEEF_AVX2;
    else if (vecFuncName.count("avx")) isa = SLEEF_AVX;
    else if (vecFuncName.count("sse")) isa = SLEEF_SSE;
    else return nullptr;

    auto modIndex = sleefModuleIndex(isa, doublePrecision);
    auto& mod = sleefModules[modIndex];
    if (!mod) mod = createModuleFromBuffer(reinterpret_cast<const char*>(sleefModuleBuffers[modIndex]), sleefModuleBufferLens[modIndex], context);
    Function *vecFunc = mod->getFunction(sleefName);
    assert(vecFunc);
    return cloneFunctionIntoModule(vecFunc, insertInto, vecFuncName);
  }
}
