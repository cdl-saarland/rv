//===- sleefLibrary.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "rv/sleefLibrary.h"

#include <llvm/Support/SourceMgr.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/Analysis/BranchProbabilityInfo.h>
#include <llvm/Passes/PassBuilder.h>

#include "rv/PlatformInfo.h"
#include "utils/rvTools.h"
#include "rvConfig.h"
#include "rv/rv.h"
#include "rv/utils.h"
#include "rv/region/FunctionRegion.h"

#include <llvm/IR/Verifier.h>
#include <vector>
#include <sstream>

#if 0
#define IF_DEBUG_SLEEF IF_DEBUG
#else
#define IF_DEBUG_SLEEF if (true)
#endif


// used for on-demand mappings
//
using namespace llvm;

// vector-length agnostic
extern const unsigned char * vla_sp_Buffer;
extern const size_t vla_sp_BufferLen;

extern const unsigned char * vla_dp_Buffer;
extern const size_t vla_dp_BufferLen;

#ifdef RV_ENABLE_ADVSIMD
extern const unsigned char * advsimd_sp_Buffer;
extern const size_t advsimd_sp_BufferLen;

extern const unsigned char * advsimd_dp_Buffer;
extern const size_t advsimd_dp_BufferLen;

extern const unsigned char * advsimd_extras_Buffer;
extern const size_t advsimd_extras_BufferLen;

#else
const unsigned char * advsimd_sp_Buffer = nullptr;
const size_t advsimd_sp_BufferLen = 0;

const unsigned char * advsimd_dp_Buffer = nullptr;
const size_t advsimd_dp_BufferLen = 0;

const unsigned char * advsimd_extras_Buffer = nullptr;
const size_t advsimd_extras_BufferLen = 0;
#endif

#ifdef RV_ENABLE_X86
extern const unsigned char * avx512_sp_Buffer;
extern const size_t avx512_sp_BufferLen;

extern const unsigned char * avx2_sp_Buffer;
extern const size_t avx2_sp_BufferLen;

extern const unsigned char * avx_sp_Buffer;
extern const size_t avx_sp_BufferLen;

extern const unsigned char * sse_sp_Buffer;
extern const size_t sse_sp_BufferLen;

extern const unsigned char * avx512_dp_Buffer;
extern const size_t avx512_dp_BufferLen;

extern const unsigned char * avx2_dp_Buffer;
extern const size_t avx2_dp_BufferLen;

extern const unsigned char * avx_dp_Buffer;
extern const size_t avx_dp_BufferLen;

extern const unsigned char * sse_dp_Buffer;
extern const size_t sse_dp_BufferLen;

extern const unsigned char * avx2_extras_Buffer;
extern const size_t avx2_extras_BufferLen;

extern const unsigned char * avx512_extras_Buffer;
extern const size_t avx512_extras_BufferLen;

#else
const unsigned char * avx512_sp_Buffer = nullptr;
const size_t avx512_sp_BufferLen = 0;

const unsigned char * avx2_sp_Buffer = nullptr;
const size_t avx2_sp_BufferLen = 0;

const unsigned char * avx_sp_Buffer = nullptr;
const size_t avx_sp_BufferLen = 0;

const unsigned char * sse_sp_Buffer = nullptr;
const size_t sse_sp_BufferLen = 0;

const unsigned char * avx512_dp_Buffer = nullptr;
const size_t avx512_dp_BufferLen = 0;

const unsigned char * avx2_dp_Buffer = nullptr;
const size_t avx2_dp_BufferLen = 0;

const unsigned char * avx_dp_Buffer = nullptr;
const size_t avx_dp_BufferLen = 0;

const unsigned char * sse_dp_Buffer = nullptr;
const size_t sse_dp_BufferLen = 0;

const unsigned char * avx2_extras_Buffer = nullptr;
const size_t avx2_extras_BufferLen = 0;

const unsigned char * avx512_extras_Buffer = nullptr;
const size_t avx512_extras_BufferLen = 0;
#endif

#ifdef RV_ENABLE_CRT
extern const unsigned char * crt_Buffer;
extern const size_t crt_BufferLen;
#endif

namespace rv {

  // forward decls
  GlobalValue & cloneGlobalIntoModule(GlobalValue &gv, Module &cloneInto);
  Constant & cloneConstant(Constant& constVal, Module & cloneInto);
  Function &cloneFunctionIntoModule(Function &func, Module &cloneInto, StringRef name);




// internal structures for named mappings (without fancily shaped arguments)
struct PlainVecDesc {
  std::string scalarFnName;
  std::string vectorFnName;
  int vectorWidth;

  PlainVecDesc(std::string _scalarName, std::string _vectorName, int _width)
  : scalarFnName(_scalarName), vectorFnName(_vectorName), vectorWidth(_width)
  {}
};
using PlainVecDescVector = std::vector<PlainVecDesc>;

using AddToListFuncType = std::function<void(const PlainVecDescVector&, bool)>;

  enum SleefISA {
    SLEEF_VLA  = 0,
    SLEEF_SSE  = 1,
    SLEEF_AVX  = 2,
    SLEEF_AVX2 = 3,
    SLEEF_AVX512 = 4,
    SLEEF_ADVSIMD = 5,
    SLEEF_Enum_Entries = 6
  };

  inline int sleefModuleIndex(SleefISA isa, bool doublePrecision) {
    return int(isa) + (doublePrecision ? (int) SLEEF_Enum_Entries : 0);
  }


  static const size_t sleefModuleBufferLens[] = {
      vla_sp_BufferLen,
      sse_sp_BufferLen,
      avx_sp_BufferLen,
      avx2_sp_BufferLen,
      avx512_sp_BufferLen,
      advsimd_sp_BufferLen,

      vla_dp_BufferLen,
      sse_dp_BufferLen,
      avx_dp_BufferLen,
      avx2_dp_BufferLen,
      avx512_dp_BufferLen,
      advsimd_dp_BufferLen,
  };

  static const unsigned char** sleefModuleBuffers[] = {
      &vla_sp_Buffer,
      &sse_sp_Buffer,
      &avx_sp_Buffer,
      &avx2_sp_Buffer,
      &avx512_sp_Buffer,
      &advsimd_sp_Buffer,

      &vla_dp_Buffer,
      &sse_dp_Buffer,
      &avx_dp_Buffer,
      &avx2_dp_Buffer,
      &avx512_dp_Buffer,
      &advsimd_dp_Buffer,
  };

  static const size_t extraModuleBufferLens[] = {
      0, // VLA
      0, // SSE
      0, // AVX
      avx2_extras_BufferLen,
      avx512_extras_BufferLen,
      advsimd_extras_BufferLen,
  };

  static const unsigned char** extraModuleBuffers[] = {
      nullptr, // VLA
      nullptr, // SSE
      nullptr, // AVX
      &avx2_extras_Buffer,
      &avx512_extras_Buffer,
      &advsimd_extras_Buffer,
  };
static
void
AddMappings_SSE(AddToListFuncType addToList, bool allowImprecise) {
      PlainVecDescVector VecFuncs = {
          // {"ldexpf", "xldexpf_sse2", 4},
          {"ilogbf", "xilogbf_sse2", 4},
          {"fmaf", "xfmaf_sse2", 4},
          {"fabsf", "xfabsf_sse2", 4},
          {"copysignf", "xcopysignf_sse2", 4},
          {"fmaxf", "xfmaxf_sse2", 4},
          {"fminf", "xfminf_sse2", 4},
          {"fdimf", "xfdimf_sse2", 4},
          {"truncf", "xtruncf_sse2", 4},
          {"floorf", "xfloorf_sse2", 4},
          {"ceilf", "xceilf_sse2", 4},
          {"roundf", "xroundf_sse2", 4},
          {"rintf", "xrintf_sse2", 4},
          {"nextafterf", "xnextafterf_sse2", 4},
          {"frfrexpf", "xfrfrexpf_sse2", 4},
          {"expfrexpf", "xexpfrexpf_sse2", 4},
          {"fmodf", "xfmodf_sse2", 4},
          {"modff", "xmodff_sse2", 4},

          // {"ldexp", "xldexp_sse2", 2},
          {"ilogb", "xilogb_sse2", 2},
          {"fma", "xfma_sse2", 2},
          {"fabs", "xfabs_sse2", 2},
          {"copysign", "xcopysign_sse2", 2},
          {"fmax", "xfmax_sse2", 2},
          {"fmin", "xfmin_sse2", 2},
          {"fdim", "xfdim_sse2", 2},
          {"trunc", "xtrunc_sse2", 2},
          {"floor", "xfloor_sse2", 2},
          {"ceil", "xceil_sse2", 2},
          {"round", "xround_sse2", 2},
          {"rint", "xrint_sse2", 2},
          {"nextafter", "xnextafter_sse2", 2},
          {"frfrexp", "xfrfrexp_sse2", 2},
          {"expfrexp", "xexpfrexp_sse2", 2},
          {"fmod", "xfmod_sse2", 2},
          {"modf", "xmodf_sse2", 2},

          {"llvm.fabs.f32", "xfabsf_sse", 4},
          {"llvm.floor.f32", "xfloorf_sse", 4},
          {"llvm.copysign.f32", "xcopysignf_sse", 4},
          {"llvm.fmin.f32", "xfminf_sse", 4},
          {"llvm.fmax.f32", "xfmaxf_sse", 4},
          {"llvm.fabs.f64", "xfabs_sse", 2},
          {"llvm.floor.f64", "xfloor_sse", 2},
          {"llvm.copysign.f64", "xcopysign_sse", 2},
          {"llvm.fmin.f64", "xfmin_sse", 2},
          {"llvm.fmax.f64", "xfmax_sse", 2}
      };
      addToList(VecFuncs, true);

      if (allowImprecise) {
        PlainVecDescVector ImprecVecFuncs = {
            {"sinf", "xsinf_sse2", 4},
            {"cosf", "xcosf_sse2", 4},
            {"tanf", "xtanf_sse2", 4},
            {"asinf", "xasinf_sse2", 4},
            {"acosf", "xacosf_sse2", 4},
            {"atanf", "xatanf_sse2", 4},
            {"atan2f", "xatan2f_sse2", 4},
            {"logf", "xlogf_sse2", 4},
            {"cbrtf", "xcbrtf_sse2", 4},
            {"expf", "xexpf_sse2", 4},
            {"powf", "xpowf_sse2", 4},
            {"sinhf", "xsinhf_sse2", 4},
            {"coshf", "xcoshf_sse2", 4},
            {"tanhf", "xtanhf_sse2", 4},
            {"asinhf", "xasinhf_sse2", 4},
            {"acoshf", "xacoshf_sse2", 4},
            {"atanhf", "xatanhf_sse2", 4},
            {"exp2f", "xexp2f_sse2", 4},
            {"exp10f", "xexp10f_sse2", 4},
            {"expm1f", "xexpm1f_sse2", 4},
            {"log10f", "xlog10f_sse2", 4},
            {"log1pf", "xlog1pf_sse2", 4},
            {"sqrtf", "xsqrtf_u35_sse2", 4},
            {"hypotf", "xhypotf_u05_sse2", 4},
            {"lgammaf", "xlgammaf_u1_sse2", 4},
            {"tgammaf", "xtgammaf_u1_sse2", 4},
            {"erff", "xerff_u1_sse2", 4},
            {"erfcf", "xerfcf_u15_sse2", 4},

            {"sin", "xsin_sse2", 2},
            {"cos", "xcos_sse2", 2},
            {"tan", "xtan_sse2", 2},
            {"asin", "xasin_sse2", 2},
            {"acos", "xacos_sse2", 2},
            {"atan", "xatan_sse2", 2},
            {"atan2", "xatan2_sse2", 2},
            {"log", "xlog_sse2", 2},
            {"cbrt", "xcbrt_sse2", 2},
            {"exp", "xexp_sse2", 2},
            {"pow", "xpow_sse2", 2},
            {"sinh", "xsinh_sse2", 2},
            {"cosh", "xcosh_sse2", 2},
            {"tanh", "xtanh_sse2", 2},
            {"asinh", "xasinh_sse2", 2},
            {"acosh", "xacosh_sse2", 2},
            {"atanh", "xatanh_sse2", 2},
            {"exp2", "xexp2_sse2", 2},
            {"exp10", "xexp10_sse2", 2},
            {"expm1", "xexpm1_sse2", 2},
            {"log10", "xlog10_sse2", 2},
            {"log1p", "xlog1p_sse2", 2},
            {"sqrt", "xsqrt_u35_sse2", 2},
            {"hypot", "xhypot_u05_sse2", 2},
            {"lgamma", "xlgamma_u1_sse2", 2},
            {"tgamma", "xtgamma_u1_sse2", 2},
            {"erf", "xerf_u1_sse2", 2},
            {"erfc", "xerfc_u15_sse2", 2},

            {"llvm.sin.f32", "xsinf_sse", 4},
            {"llvm.cos.f32", "xcosf_sse", 4},
            {"llvm.log.f32", "xlogf_sse", 4},
            {"llvm.exp.f32", "xexpf_sse", 4},
            {"llvm.pow.f32", "xpowf_sse", 4},
            {"llvm.sqrt.f32", "xsqrtf_u35_sse", 4},
            {"llvm.exp2.f32", "xexp2f_sse", 4},
            {"llvm.log10.f32", "xlog10f_sse", 4},
            {"llvm.sin.f64", "xsin_sse", 2},
            {"llvm.cos.f64", "xcos_sse", 2},
            {"llvm.log.f64", "xlog_sse", 2},
            {"llvm.exp.f64", "xexp_sse", 2},
            {"llvm.pow.f64", "xpow_sse", 2},
            {"llvm.sqrt.f64", "xsqrt_u35_sse", 2},
            {"llvm.exp2.f64", "xexp2_sse", 2},
            {"llvm.log10.f64", "xlog10_sse", 2}
        };
        addToList(ImprecVecFuncs, true);
      }
}

static
void
AddMappings_AVX(AddToListFuncType addToList, bool allowImprecise) {
      PlainVecDescVector VecFuncs = {
          // {"ldexpf", "xldexpf_avx", 8},
          {"ilogbf", "xilogbf_avx", 8},
          {"fmaf", "xfmaf_avx", 8},
          {"fabsf", "xfabsf_avx", 8},
          {"copysignf", "xcopysignf_avx", 8},
          {"fmaxf", "xfmaxf_avx", 8},
          {"fminf", "xfminf_avx", 8},
          {"fdimf", "xfdimf_avx", 8},
          {"truncf", "xtruncf_avx", 8},
          {"floorf", "xfloorf_avx", 8},
          {"ceilf", "xceilf_avx", 8},
          {"roundf", "xroundf_avx", 8},
          {"rintf", "xrintf_avx", 8},
          {"nextafterf", "xnextafterf_avx", 8},
          {"frfrexpf", "xfrfrexpf_avx", 8},
          {"expfrexpf", "xexpfrexpf_avx", 8},
          {"fmodf", "xfmodf_avx", 8},
          {"modff", "xmodff_avx", 8},

          // {"ldexp", "xldexp_avx", 4},
          {"ilogb", "xilogb_avx", 4},
          {"fma", "xfma_avx", 4},
          {"fabs", "xfabs_avx", 4},
          {"copysign", "xcopysign_avx", 4},
          {"fmax", "xfmax_avx", 4},
          {"fmin", "xfmin_avx", 4},
          {"fdim", "xfdim_avx", 4},
          {"trunc", "xtrunc_avx", 4},
          {"floor", "xfloor_avx", 4},
          {"ceil", "xceil_avx", 4},
          {"round", "xround_avx", 4},
          {"rint", "xrint_avx", 4},
          {"nextafter", "xnextafter_avx", 4},
          {"frfrexp", "xfrfrexp_avx", 4},
          {"expfrexp", "xexpfrexp_avx", 4},
          {"fmod", "xfmod_avx", 4},
          {"modf", "xmodf_avx", 4},

          {"llvm.fabs.f32", "xfabsf_avx", 8},
          {"llvm.floor.f32", "xfloorf_avx", 8},
          {"llvm.copysign.f32", "xcopysignf_avx", 8},
          {"llvm.minnum.f32", "xfminf_avx", 8},
          {"llvm.maxnum.f32", "xfmaxf_avx", 8},
          {"llvm.fabs.f64", "xfabs_avx", 4},
          {"llvm.floor.f64", "xfloor_avx", 4},
          {"llvm.copysign.f64", "xcopysign_avx", 4},
          {"llvm.minnum.f64", "xfmin_avx", 4},
          {"llvm.maxnum.f64", "xfmax_avx", 4}
      };
      addToList(VecFuncs, true);

      if (allowImprecise) {
        PlainVecDescVector ImprecVecFuncs = {
            {"sinf", "xsinf_avx", 8},
            {"cosf", "xcosf_avx", 8},
            {"tanf", "xtanf_avx", 8},
            {"asinf", "xasinf_avx", 8},
            {"acosf", "xacosf_avx", 8},
            {"atanf", "xatanf_avx", 8},
            {"atan2f", "xatan2f_avx", 8},
            {"logf", "xlogf_avx", 8},
            {"cbrtf", "xcbrtf_avx", 8},
            {"expf", "xexpf_avx", 8},
            {"powf", "xpowf_avx", 8},
            {"sinhf", "xsinhf_avx", 8},
            {"coshf", "xcoshf_avx", 8},
            {"tanhf", "xtanhf_avx", 8},
            {"asinhf", "xasinhf_avx", 8},
            {"acoshf", "xacoshf_avx", 8},
            {"atanhf", "xatanhf_avx", 8},
            {"exp2f", "xexp2f_avx", 8},
            {"exp10f", "xexp10f_avx", 8},
            {"expm1f", "xexpm1f_avx", 8},
            {"log10f", "xlog10f_avx", 8},
            {"log1pf", "xlog1pf_avx", 8},
            {"sqrtf", "xsqrtf_u35_avx", 8},
            {"hypotf", "xhypotf_u05_avx", 8},
            {"lgammaf", "xlgammaf_u1_avx", 8},
            {"tgammaf", "xtgammaf_u1_avx", 8},
            {"erff", "xerff_u1_avx", 8},
            {"erfcf", "xerfcf_u15_avx", 8},

            {"sin", "xsin_avx", 4},
            {"cos", "xcos_avx", 4},
            {"tan", "xtan_avx", 4},
            {"asin", "xasin_avx", 4},
            {"acos", "xacos_avx", 4},
            {"atan", "xatan_avx", 4},
            {"atan2", "xatan2_avx", 4},
            {"log", "xlog_avx", 4},
            {"cbrt", "xcbrt_avx", 4},
            {"exp", "xexp_avx", 4},
            {"pow", "xpow_avx", 4},
            {"sinh", "xsinh_avx", 4},
            {"cosh", "xcosh_avx", 4},
            {"tanh", "xtanh_avx", 4},
            {"asinh", "xasinh_avx", 4},
            {"acosh", "xacosh_avx", 4},
            {"atanh", "xatanh_avx", 4},
            {"exp2", "xexp2_avx", 4},
            {"exp10", "xexp10_avx", 4},
            {"expm1", "xexpm1_avx", 4},
            {"log10", "xlog10_avx", 4},
            {"log1p", "xlog1p_avx", 4},
            {"sqrt", "xsqrt_u35_avx", 4},
            {"hypot", "xhypot_u05_avx", 4},
            {"lgamma", "xlgamma_u1_avx", 4},
            {"tgamma", "xtgamma_u1_avx", 4},
            {"erf", "xerf_u1_avx", 4},
            {"erfc", "xerfc_u15_avx", 4},

            {"llvm.sin.f32", "xsinf_avx", 8},
            {"llvm.cos.f32", "xcosf_avx", 8},
            {"llvm.log.f32", "xlogf_avx", 8},
            {"llvm.exp.f32", "xexpf_avx", 8},
            {"llvm.pow.f32", "xpowf_avx", 8},
            {"llvm.sqrt.f32", "xsqrtf_u35_avx", 8},
            {"llvm.exp2.f32", "xexp2f_avx", 8},
            {"llvm.log10.f32", "xlog10f_avx", 8},
            {"llvm.sin.f64", "xsin_avx", 4},
            {"llvm.cos.f64", "xcos_avx", 4},
            {"llvm.log.f64", "xlog_avx", 4},
            {"llvm.exp.f64", "xexp_avx", 4},
            {"llvm.pow.f64", "xpow_avx", 4},
            {"llvm.sqrt.f64", "xsqrt_u35_avx", 4},
            {"llvm.exp2.f64", "xexp2_avx", 4},
            {"llvm.log10.f64", "xlog10_avx", 4}
        };
        addToList(ImprecVecFuncs, true);
      }
}


static
void
AddMappings_AVX2(AddToListFuncType addToList, bool allowImprecise) {
      PlainVecDescVector VecFuncs = {
          // {"ldexpf", "xldexpf_avx2", 8},
          {"ilogbf", "xilogbf_avx2", 8},
          {"fmaf", "xfmaf_avx2", 8},
          {"fabsf", "xfabsf_avx2", 8},
          {"copysignf", "xcopysignf_avx2", 8},
          {"fmaxf", "xfmaxf_avx2", 8},
          {"fminf", "xfminf_avx2", 8},
          {"fdimf", "xfdimf_avx2", 8},
          {"truncf", "xtruncf_avx2", 8},
          {"floorf", "xfloorf_avx2", 8},
          {"ceilf", "xceilf_avx2", 8},
          {"roundf", "xroundf_avx2", 8},
          {"rintf", "xrintf_avx2", 8},
          {"nextafterf", "xnextafterf_avx2", 8},
          {"frfrexpf", "xfrfrexpf_avx2", 8},
          {"expfrexpf", "xexpfrexpf_avx2", 8},
          {"fmodf", "xfmodf_avx2", 8},
          {"modff", "xmodff_avx2", 8},

          // {"ldexp", "xldexp_avx2", 4},
          {"ilogb", "xilogb_avx2", 4},
          {"fma", "xfma_avx2", 4},
          {"fabs", "xfabs_avx2", 4},
          {"copysign", "xcopysign_avx2", 4},
          {"fmax", "xfmax_avx2", 4},
          {"fmin", "xfmin_avx2", 4},
          {"fdim", "xfdim_avx2", 4},
          {"trunc", "xtrunc_avx2", 4},
          {"floor", "xfloor_avx2", 4},
          {"ceil", "xceil_avx2", 4},
          {"round", "xround_avx2", 4},
          {"rint", "xrint_avx2", 4},
          {"nextafter", "xnextafter_avx2", 4},
          {"frfrexp", "xfrfrexp_avx2", 4},
          {"expfrexp", "xexpfrexp_avx2", 4},
          {"fmod", "xfmod_avx2", 4},
          {"modf", "xmodf_avx2", 4},

          {"llvm.fabs.f32", "xfabsf_avx2", 8},
          {"llvm.copysign.f32", "xcopysignf_avx2", 8},
          {"llvm.minnum.f32", "xfminf_avx2", 8},
          {"llvm.maxnum.f32", "xfmaxf_avx2", 8},
          {"llvm.fabs.f64", "xfabs_avx2", 4},
          {"llvm.copysign.f64", "xcopysign_avx2", 4},
          {"llvm.minnum.f64", "xfmin_avx2", 4},
          {"llvm.maxnum.f64", "xfmax_avx2", 4},

        // extras
          {"drand48", "vrand_extra_avx2", 4},
          {"frand48", "vrandf_extra_avx2", 8}
      };
      addToList(VecFuncs, true);






      if (allowImprecise) {
        PlainVecDescVector ImprecVecFuncs = {
            {"sinf", "xsinf_avx2", 8},
            {"cosf", "xcosf_avx2", 8},
            {"tanf", "xtanf_avx2", 8},
            {"asinf", "xasinf_avx2", 8},
            {"acosf", "xacosf_avx2", 8},
            {"atanf", "xatanf_avx2", 8},
            {"atan2f", "xatan2f_avx2", 8},
            {"logf", "xlogf_avx2", 8},
            {"cbrtf", "xcbrtf_avx2", 8},
            {"expf", "xexpf_avx2", 8},
            {"powf", "xpowf_avx2", 8},
            {"sinhf", "xsinhf_avx2", 8},
            {"coshf", "xcoshf_avx2", 8},
            {"tanhf", "xtanhf_avx2", 8},
            {"asinhf", "xasinhf_avx2", 8},
            {"acoshf", "xacoshf_avx2", 8},
            {"atanhf", "xatanhf_avx2", 8},
            {"exp2f", "xexp2f_avx2", 8},
            {"exp10f", "xexp10f_avx2", 8},
            {"expm1f", "xexpm1f_avx2", 8},
            {"log10f", "xlog10f_avx2", 8},
            {"log1pf", "xlog1pf_avx2", 8},
            {"sqrtf", "xsqrtf_u35_avx2", 8},
            {"hypotf", "xhypotf_u05_avx2", 8},
            {"lgammaf", "xlgammaf_u1_avx2", 8},
            {"tgammaf", "xtgammaf_u1_avx2", 8},
            {"erff", "xerff_u1_avx2", 8},
            {"erfcf", "xerfcf_u15_avx2", 8},

            {"sin", "xsin_avx2", 4},
            {"cos", "xcos_avx2", 4},
            {"tan", "xtan_avx2", 4},
            {"asin", "xasin_avx2", 4},
            {"acos", "xacos_avx2", 4},
            {"atan", "xatan_avx2", 4},
            {"atan2", "xatan2_avx2", 4},
            {"log", "xlog_avx2", 4},
            {"cbrt", "xcbrt_avx2", 4},
            {"exp", "xexp_avx2", 4},
            {"pow", "xpow_avx2", 4},
            {"sinh", "xsinh_avx2", 4},
            {"cosh", "xcosh_avx2", 4},
            {"tanh", "xtanh_avx2", 4},
            {"asinh", "xasinh_avx2", 4},
            {"acosh", "xacosh_avx2", 4},
            {"atanh", "xatanh_avx2", 4},
            {"exp2", "xexp2_avx2", 4},
            {"exp10", "xexp10_avx2", 4},
            {"expm1", "xexpm1_avx2", 4},
            {"log10", "xlog10_avx2", 4},
            {"log1p", "xlog1p_avx2", 4},
            {"sqrt", "xsqrt_u35_avx2", 4},
            {"hypot", "xhypot_u05_avx2", 4},
            {"lgamma", "xlgamma_u1_avx2", 4},
            {"tgamma", "xtgamma_u1_avx2", 4},
            {"erf", "xerf_u1_avx2", 4},
            {"erfc", "xerfc_u15_avx2", 4},

            {"llvm.sin.f32", "xsinf_avx2", 8},
            {"llvm.cos.f32", "xcosf_avx2", 8},
            {"llvm.log.f32", "xlogf_avx2", 8},
            {"llvm.exp.f32", "xexpf_avx2", 8},
            {"llvm.pow.f32", "xpowf_avx2", 8},
            {"llvm.sqrt.f32", "xsqrtf_u35_avx2", 8},
            {"llvm.exp2.f32", "xexp2f_avx2", 8},
            {"llvm.log10.f32", "xlog10f_avx2", 8},
            {"llvm.sin.f64", "xsin_avx2", 4},
            {"llvm.cos.f64", "xcos_avx2", 4},
            {"llvm.log.f64", "xlog_avx2", 4},
            {"llvm.exp.f64", "xexp_avx2", 4},
            {"llvm.pow.f64", "xpow_avx2", 4},
            {"llvm.sqrt.f64", "xsqrt_u35_avx2", 4},
            {"llvm.exp2.f64", "xexp2_avx2", 4},
            {"llvm.log10.f64", "xlog10_avx2", 4}
        };
        addToList(ImprecVecFuncs, true);
      }
}

static
void
AddMappings_AVX512(AddToListFuncType addToList, bool allowImprecise) {
      PlainVecDescVector VecFuncs = {
          // {"ldexpf", "xldexpf_avx512", 16},
          {"ilogbf", "xilogbf_avx512", 16},
          {"fmaf", "xfmaf_avx512", 16},
          {"fabsf", "xfabsf_avx512", 16},
          {"copysignf", "xcopysignf_avx512", 16},
          {"fmaxf", "xfmaxf_avx512", 16},
          {"fminf", "xfminf_avx512", 16},
          {"fdimf", "xfdimf_avx512", 16},
          {"truncf", "xtruncf_avx512", 16},
          {"floorf", "xfloorf_avx512", 16},
          {"ceilf", "xceilf_avx512", 16},
          {"roundf", "xroundf_avx512", 16},
          {"rintf", "xrintf_avx512", 16},
          {"nextafterf", "xnextafterf_avx512", 16},
          {"frfrexpf", "xfrfrexpf_avx512", 16},
          {"expfrexpf", "xexpfrexpf_avx512", 16},
          {"fmodf", "xfmodf_avx512", 16},
          {"modff", "xmodff_avx512", 16},

          // {"ldexp", "xldexp_avx512", 8},
          {"ilogb", "xilogb_avx512", 8},
          {"fma", "xfma_avx512", 8},
          {"fabs", "xfabs_avx512", 8},
          {"copysign", "xcopysign_avx512", 8},
          {"fmax", "xfmax_avx512", 8},
          {"fmin", "xfmin_avx512", 8},
          {"fdim", "xfdim_avx512", 8},
          {"trunc", "xtrunc_avx512", 8},
          {"floor", "xfloor_avx512", 8},
          {"ceil", "xceil_avx512", 8},
          {"round", "xround_avx512", 8},
          {"rint", "xrint_avx512", 8},
          {"nextafter", "xnextafter_avx512", 8},
          {"frfrexp", "xfrfrexp_avx512", 8},
          {"expfrexp", "xexpfrexp_avx512", 8},
          {"fmod", "xfmod_avx512", 8},
          {"modf", "xmodf_avx512", 8},

          {"llvm.fabs.f32", "xfabsf_avx512", 16},
          {"llvm.copysign.f32", "xcopysignf_avx512", 16},
          {"llvm.minnum.f32", "xfminf_avx512", 16},
          {"llvm.maxnum.f32", "xfmaxf_avx512", 16},
          {"llvm.fabs.f64", "xfabs_avx512", 8},
          {"llvm.copysign.f64", "xcopysign_avx512", 8},
          {"llvm.minnum.f64", "xfmin_avx512", 8},
          {"llvm.maxnum.f64", "xfmax_avx512", 8},

        // extras
          {"drand48", "vrand_extra_avx512", 8},
          {"frand48", "vrandf_extra_avx512", 16}
      };
      addToList(VecFuncs, true);






      if (allowImprecise) {
        PlainVecDescVector ImprecVecFuncs = {
            {"sinf", "xsinf_avx512", 16},
            {"cosf", "xcosf_avx512", 16},
            {"tanf", "xtanf_avx512", 16},
            {"asinf", "xasinf_avx512", 16},
            {"acosf", "xacosf_avx512", 16},
            {"atanf", "xatanf_avx512", 16},
            {"atan2f", "xatan2f_avx512", 16},
            {"logf", "xlogf_avx512", 16},
            {"cbrtf", "xcbrtf_avx512", 16},
            {"expf", "xexpf_avx512", 16},
            {"powf", "xpowf_avx512", 16},
            {"sinhf", "xsinhf_avx512", 16},
            {"coshf", "xcoshf_avx512", 16},
            {"tanhf", "xtanhf_avx512", 16},
            {"asinhf", "xasinhf_avx512", 16},
            {"acoshf", "xacoshf_avx512", 16},
            {"atanhf", "xatanhf_avx512", 16},
            {"exp2f", "xexp2f_avx512", 16},
            {"exp10f", "xexp10f_avx512", 16},
            {"expm1f", "xexpm1f_avx512", 16},
            {"log10f", "xlog10f_avx512", 16},
            {"log1pf", "xlog1pf_avx512", 16},
            {"sqrtf", "xsqrtf_u35_avx512", 16},
            {"hypotf", "xhypotf_u05_avx512", 16},
            {"lgammaf", "xlgammaf_u1_avx512", 16},
            {"tgammaf", "xtgammaf_u1_avx512", 16},
            {"erff", "xerff_u1_avx512", 16},
            {"erfcf", "xerfcf_u15_avx512", 16},

            {"sin", "xsin_avx512", 8},
            {"cos", "xcos_avx512", 8},
            {"tan", "xtan_avx512", 8},
            {"asin", "xasin_avx512", 8},
            {"acos", "xacos_avx512", 8},
            {"atan", "xatan_avx512", 8},
            {"atan2", "xatan2_avx512", 8},
            {"log", "xlog_avx512", 8},
            {"cbrt", "xcbrt_avx512", 8},
            {"exp", "xexp_avx512", 8},
            {"pow", "xpow_avx512", 8},
            {"sinh", "xsinh_avx512", 8},
            {"cosh", "xcosh_avx512", 8},
            {"tanh", "xtanh_avx512", 8},
            {"asinh", "xasinh_avx512", 8},
            {"acosh", "xacosh_avx512", 8},
            {"atanh", "xatanh_avx512", 8},
            {"exp2", "xexp2_avx512", 8},
            {"exp10", "xexp10_avx512", 8},
            {"expm1", "xexpm1_avx512", 8},
            {"log10", "xlog10_avx512", 8},
            {"log1p", "xlog1p_avx512", 8},
            {"sqrt", "xsqrt_u35_avx512", 8},
            {"hypot", "xhypot_u05_avx512", 8},
            {"lgamma", "xlgamma_u1_avx512", 8},
            {"tgamma", "xtgamma_u1_avx512", 8},
            {"erf", "xerf_u1_avx512", 8},
            {"erfc", "xerfc_u15_avx512", 8},

            {"llvm.sin.f32", "xsinf_avx512", 16},
            {"llvm.cos.f32", "xcosf_avx512", 16},
            {"llvm.log.f32", "xlogf_avx512", 16},
            {"llvm.exp.f32", "xexpf_avx512", 16},
            {"llvm.pow.f32", "xpowf_avx512", 16},
            {"llvm.sqrt.f32", "xsqrtf_u35_avx512", 16},
            {"llvm.exp2.f32", "xexp2f_avx512", 16},
            {"llvm.log10.f32", "xlog10f_avx512", 16},
            {"llvm.sin.f64", "xsin_avx512", 8},
            {"llvm.cos.f64", "xcos_avx512", 8},
            {"llvm.log.f64", "xlog_avx512", 8},
            {"llvm.exp.f64", "xexp_avx512", 8},
            {"llvm.pow.f64", "xpow_avx512", 8},
            {"llvm.sqrt.f64", "xsqrt_u35_avx512", 8},
            {"llvm.exp2.f64", "xexp2_avx512", 8},
            {"llvm.log10.f64", "xlog10_avx512", 8}
        };
        addToList(ImprecVecFuncs, true);
      }
}

static
void
AddMappings_ADVSIMD(AddToListFuncType addToList, bool allowImprecise) {
      PlainVecDescVector VecFuncs = {
          // {"ldexpf", "xldexpf_advsimd", 2},
          {"ilogbf", "xilogbf_advsimd", 2},
          {"fmaf", "xfmaf_advsimd", 2},
          {"fabsf", "xfabsf_advsimd", 2},
          {"copysignf", "xcopysignf_advsimd", 2},
          {"fmaxf", "xfmaxf_advsimd", 2},
          {"fminf", "xfminf_advsimd", 2},
          {"fdimf", "xfdimf_advsimd", 2},
          {"truncf", "xtruncf_advsimd", 2},
          {"floorf", "xfloorf_advsimd", 2},
          {"ceilf", "xceilf_advsimd", 2},
          {"roundf", "xroundf_advsimd", 2},
          {"rintf", "xrintf_advsimd", 2},
          {"nextafterf", "xnextafterf_advsimd", 2},
          {"frfrexpf", "xfrfrexpf_advsimd", 2},
          {"expfrexpf", "xexpfrexpf_advsimd", 2},
          {"fmodf", "xfmodf_advsimd", 2},
          {"modff", "xmodff_advsimd", 2},

          // {"ldexp", "xldexp_advsimd", 8},
          {"ilogb", "xilogb_advsimd", 8},
          {"fma", "xfma_advsimd", 8},
          {"fabs", "xfabs_advsimd", 8},
          {"copysign", "xcopysign_advsimd", 8},
          {"fmax", "xfmax_advsimd", 8},
          {"fmin", "xfmin_advsimd", 8},
          {"fdim", "xfdim_advsimd", 8},
          {"trunc", "xtrunc_advsimd", 8},
          {"floor", "xfloor_advsimd", 8},
          {"ceil", "xceil_advsimd", 8},
          {"round", "xround_advsimd", 8},
          {"rint", "xrint_advsimd", 8},
          {"nextafter", "xnextafter_advsimd", 8},
          {"frfrexp", "xfrfrexp_advsimd", 8},
          {"expfrexp", "xexpfrexp_advsimd", 8},
          {"fmod", "xfmod_advsimd", 8},
          {"modf", "xmodf_advsimd", 8},

          {"llvm.fabs.f32", "xfabsf_advsimd", 2},
          {"llvm.copysign.f32", "xcopysignf_advsimd", 2},
          {"llvm.minnum.f32", "xfminf_advsimd", 2},
          {"llvm.maxnum.f32", "xfmaxf_advsimd", 2},
          {"llvm.fabs.f64", "xfabs_advsimd", 8},
          {"llvm.copysign.f64", "xcopysign_advsimd", 8},
          {"llvm.minnum.f64", "xfmin_advsimd", 8},
          {"llvm.maxnum.f64", "xfmax_advsimd", 8},

        // extras
          {"drand48", "vrand_extra_advsimd", 2},
          {"frand48", "vrand_extra_advsimd", 4}
      };
      addToList(VecFuncs, true);






      if (allowImprecise) {
        PlainVecDescVector ImprecVecFuncs = {
            {"sinf", "xsinf_advsimd", 4},
            {"cosf", "xcosf_advsimd", 4},
            {"tanf", "xtanf_advsimd", 4},
            {"asinf", "xasinf_advsimd", 4},
            {"acosf", "xacosf_advsimd", 4},
            {"atanf", "xatanf_advsimd", 4},
            {"atan2f", "xatan2f_advsimd", 4},
            {"logf", "xlogf_advsimd", 4},
            {"cbrtf", "xcbrtf_advsimd", 4},
            {"expf", "xexpf_advsimd", 4},
            {"powf", "xpowf_advsimd", 4},
            {"sinhf", "xsinhf_advsimd", 4},
            {"coshf", "xcoshf_advsimd", 4},
            {"tanhf", "xtanhf_advsimd", 4},
            {"asinhf", "xasinhf_advsimd", 4},
            {"acoshf", "xacoshf_advsimd", 4},
            {"atanhf", "xatanhf_advsimd", 4},
            {"exp2f", "xexp2f_advsimd", 4},
            {"exp10f", "xexp10f_advsimd", 4},
            {"expm1f", "xexpm1f_advsimd", 4},
            {"log10f", "xlog10f_advsimd", 4},
            {"log1pf", "xlog1pf_advsimd", 4},
            {"sqrtf", "xsqrtf_u35_advsimd", 4},
            {"hypotf", "xhypotf_u05_advsimd", 4},
            {"lgammaf", "xlgammaf_u1_advsimd", 4},
            {"tgammaf", "xtgammaf_u1_advsimd", 4},
            {"erff", "xerff_u1_advsimd", 4},
            {"erfcf", "xerfcf_u15_advsimd", 4},

            {"sin", "xsin_advsimd", 2},
            {"cos", "xcos_advsimd", 2},
            {"tan", "xtan_advsimd", 2},
            {"asin", "xasin_advsimd", 2},
            {"acos", "xacos_advsimd", 2},
            {"atan", "xatan_advsimd", 2},
            {"atan2", "xatan2_advsimd", 2},
            {"log", "xlog_advsimd", 2},
            {"cbrt", "xcbrt_advsimd", 2},
            {"exp", "xexp_advsimd", 2},
            {"pow", "xpow_advsimd", 2},
            {"sinh", "xsinh_advsimd", 2},
            {"cosh", "xcosh_advsimd", 2},
            {"tanh", "xtanh_advsimd", 2},
            {"asinh", "xasinh_advsimd", 2},
            {"acosh", "xacosh_advsimd", 2},
            {"atanh", "xatanh_advsimd", 2},
            {"exp2", "xexp2_advsimd", 2},
            {"exp10", "xexp10_advsimd", 2},
            {"expm1", "xexpm1_advsimd", 2},
            {"log10", "xlog10_advsimd", 2},
            {"log1p", "xlog1p_advsimd", 2},
            {"sqrt", "xsqrt_u35_advsimd", 2},
            {"hypot", "xhypot_u05_advsimd", 2},
            {"lgamma", "xlgamma_u1_advsimd", 2},
            {"tgamma", "xtgamma_u1_advsimd", 2},
            {"erf", "xerf_u1_advsimd", 2},
            {"erfc", "xerfc_u15_advsimd", 2},

            {"llvm.sin.f32", "xsinf_advsimd", 4},
            {"llvm.cos.f32", "xcosf_advsimd", 4},
            {"llvm.log.f32", "xlogf_advsimd", 4},
            {"llvm.exp.f32", "xexpf_advsimd", 4},
            {"llvm.pow.f32", "xpowf_advsimd", 4},
            {"llvm.sqrt.f32", "xsqrtf_u35_advsimd", 4},
            {"llvm.exp2.f32", "xexp2f_advsimd", 4},
            {"llvm.log10.f32", "xlog10f_advsimd", 4},
            {"llvm.sin.f64", "xsin_advsimd", 2},
            {"llvm.cos.f64", "xcos_advsimd", 2},
            {"llvm.log.f64", "xlog_advsimd", 2},
            {"llvm.exp.f64", "xexp_advsimd", 2},
            {"llvm.pow.f64", "xpow_advsimd", 2},
            {"llvm.sqrt.f64", "xsqrt_u35_advsimd", 2},
            {"llvm.exp2.f64", "xexp2_advsimd", 2},
            {"llvm.log10.f64", "xlog10_advsimd", 2}
        };
        addToList(ImprecVecFuncs, true);
      }
}

static
void
AddMappings_VLA(AddToListFuncType addToList, bool allowImprecise) {
      PlainVecDescVector VecFuncs = {
          // {"ldexpf", "xldexpf_vla", 2},
          {"ilogbf", "xilogbf_vla", -1},
          {"fmaf", "xfmaf_vla", -1},
          {"fabsf", "xfabsf_vla", -1},
          {"copysignf", "xcopysignf_vla", -1},
          {"fmaxf", "xfmaxf_vla", -1},
          {"fminf", "xfminf_vla", -1},
          {"fdimf", "xfdimf_vla", -1},
          {"truncf", "xtruncf_vla", -1},
          {"floorf", "xfloorf_vla", -1},
          {"ceilf", "xceilf_vla", -1},
          {"roundf", "xroundf_vla", -1},
          {"rintf", "xrintf_vla", -1},
          {"nextafterf", "xnextafterf_vla", -1},
          {"frfrexpf", "xfrfrexpf_vla",  -1},
          {"expfrexpf", "xexpfrexpf_vla", -1},
          {"fmodf", "xfmodf_vla", -1},
          {"modff", "xmodff_vla", -1},

          // {"ldexp", "xldexp_vla", 8},
          {"ilogb", "xilogb_vla", -1},
          {"fma", "xfma_vla", -1},
          {"fabs", "xfabs_vla", -1},
          {"copysign", "xcopysign_vla", -1},
          {"fmax", "xfmax_vla", -1},
          {"fmin", "xfmin_vla", -1},
          {"fdim", "xfdim_vla", -1},
          {"trunc", "xtrunc_vla", -1},
          {"floor", "xfloor_vla", -1},
          {"ceil", "xceil_vla", -1},
          {"round", "xround_vla", -1},
          {"rint", "xrint_vla", -1},
          {"nextafter", "xnextafter_vla", -1},
          {"frfrexp", "xfrfrexp_vla", -1},
          {"expfrexp", "xexpfrexp_vla", -1},
          {"fmod", "xfmod_vla", -1},
          {"modf", "xmodf_vla", -1},

          {"llvm.fabs.f32", "xfabsf_vla", -1},
          {"llvm.copysign.f32", "xcopysignf_vla", -1},
          {"llvm.minnum.f32", "xfminf_vla", -1},
          {"llvm.maxnum.f32", "xfmaxf_vla", -1},
          {"llvm.fabs.f64", "xfabs_vla", -1},
          {"llvm.copysign.f64", "xcopysign_vla", -1},
          {"llvm.minnum.f64", "xfmin_vla", -1},
          {"llvm.maxnum.f64", "xfmax_vla", -1},

#if 0
        // TODO VLA random number generator
        // extras
          {"drand48", "vrand_extra_vla", 2},
          {"frand48", "vrand_extra_vla", 4}
#endif
      };
      addToList(VecFuncs, true);






      if (allowImprecise) {
        PlainVecDescVector ImprecVecFuncs = {
            {"sinf", "xsinf_vla", -1},
            {"cosf", "xcosf_vla", -1},
            {"tanf", "xtanf_vla", -1},
            {"asinf", "xasinf_vla", -1},
            {"acosf", "xacosf_vla", -1},
            {"atanf", "xatanf_vla", -1},
            {"atan2f", "xatan2f_vla", -1},
            {"logf", "xlogf_vla", -1},
            {"cbrtf", "xcbrtf_vla", -1},
            {"expf", "xexpf_vla", -1},
            {"powf", "xpowf_vla", -1},
            {"sinhf", "xsinhf_vla", -1},
            {"coshf", "xcoshf_vla", -1},
            {"tanhf", "xtanhf_vla", -1},
            {"asinhf", "xasinhf_vla", -1},
            {"acoshf", "xacoshf_vla", -1},
            {"atanhf", "xatanhf_vla", -1},
            {"exp2f", "xexp2f_vla", -1},
            {"exp10f", "xexp10f_vla", -1},
            {"expm1f", "xexpm1f_vla", -1},
            {"log10f", "xlog10f_vla", -1},
            {"log1pf", "xlog1pf_vla", -1},
            {"sqrtf", "xsqrtf_u35_vla", -1},
            {"hypotf", "xhypotf_u05_vla",  -1},
            {"lgammaf", "xlgammaf_u1_vla", -1},
            {"tgammaf", "xtgammaf_u1_vla", -1},
            {"erff", "xerff_u1_vla",       -1},
            {"erfcf", "xerfcf_u15_vla",    -1},

            {"sin", "xsin_vla",            -1},
            {"cos", "xcos_vla",            -1},
            {"tan", "xtan_vla",            -1},
            {"asin", "xasin_vla",          -1},
            {"acos", "xacos_vla",          -1},
            {"atan", "xatan_vla",          -1},
            {"atan2", "xatan2_vla", -1},
            {"log", "xlog_vla", -1},
            {"cbrt", "xcbrt_vla", -1},
            {"exp", "xexp_vla", -1},
            {"pow", "xpow_vla", -1},
            {"sinh", "xsinh_vla", -1},
            {"cosh", "xcosh_vla", -1},
            {"tanh", "xtanh_vla", -1},
            {"asinh", "xasinh_vla", -1},
            {"acosh", "xacosh_vla", -1},
            {"atanh", "xatanh_vla", -1},
            {"exp2", "xexp2_vla",   -1},
            {"exp10", "xexp10_vla", -1},
            {"expm1", "xexpm1_vla", -1},
            {"log10", "xlog10_vla", -1},
            {"log1p", "xlog1p_vla", -1},
            {"sqrt", "xsqrt_u35_vla", -1},
            {"hypot", "xhypot_u05_vla", -1},
            {"lgamma", "xlgamma_u1_vla", -1},
            {"tgamma", "xtgamma_u1_vla", -1},
            {"erf", "xerf_u1_vla",     -1},
            {"erfc", "xerfc_u15_vla", -1},

            {"llvm.sin.f32", "xsinf_vla", -1},
            {"llvm.cos.f32", "xcosf_vla", -1},
            {"llvm.log.f32", "xlogf_vla", -1},
            {"llvm.exp.f32", "xexpf_vla", -1},
            {"llvm.pow.f32", "xpowf_vla", -1},
            {"llvm.sqrt.f32", "xsqrtf_u35_vla", -1},
            {"llvm.exp2.f32", "xexp2f_vla", -1},
            {"llvm.log10.f32", "xlog10f_vla", -1},
            {"llvm.sin.f64", "xsin_vla", -1},
            {"llvm.cos.f64", "xcos_vla", -1},
            {"llvm.log.f64", "xlog_vla", -1},
            {"llvm.exp.f64", "xexp_vla", -1},
            {"llvm.pow.f64", "xpow_vla", -1},
            {"llvm.sqrt.f64", "xsqrt_u35_vla", -1},
            {"llvm.exp2.f64", "xexp2_vla", -1},
            {"llvm.log10.f64", "xlog10_vla", -1}
        };
        addToList(ImprecVecFuncs, true);
      }
}

static Module const *sleefModules[SLEEF_Enum_Entries * 2];
static Module const *extraModules[SLEEF_Enum_Entries * 2];

#ifdef RV_ENABLE_CRT
  static Module* scalarModule; // scalar implementations to be inlined
#endif


class SleefResolverService : public ResolverService {
  PlainVecDescVector commonVectorMappings;
  PlatformInfo & platInfo;
  Config config;

  void addNamedMappings(const PlainVecDescVector & funcs, bool givePrecedence) {
    auto itInsert = givePrecedence ? commonVectorMappings.begin() : commonVectorMappings.end();
    commonVectorMappings.insert(itInsert, funcs.begin(), funcs.end());
  }

public:
  SleefResolverService(PlatformInfo & _platInfo, const Config & _config, bool allowImprecise)
  : platInfo(_platInfo)
  , config(_config)
  {
    AddToListFuncType addToListFunc = [this](const PlainVecDescVector & funcDescs, bool givePrecedence) {
        addNamedMappings(funcDescs, givePrecedence);
    };

    if (config.useADVSIMD) {
      AddMappings_ADVSIMD(addToListFunc, allowImprecise);
    } else {
      // TODO add neon32 mappings
    }

    if (config.useSSE || config.useAVX || config.useAVX2 || config.useAVX512) {
      AddMappings_SSE(addToListFunc, allowImprecise);
    }

    if (config.useAVX) {
      AddMappings_AVX(addToListFunc, allowImprecise);
    }

    if (config.useAVX2 || config.useAVX512) {
      AddMappings_AVX2(addToListFunc, allowImprecise);
    }
    if (config.useAVX512) {
      AddMappings_AVX512(addToListFunc, allowImprecise);
    }

    // add vector-length agnostic mappings
    AddMappings_VLA(addToListFunc, allowImprecise);
  }

  ~SleefResolverService() {}

  std::unique_ptr<FunctionResolver> resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule);

protected:
  bool
  isFunctionVectorizable(llvm::StringRef funcName, llvm::FunctionType & scaFuncType, int vectorWidth) const {
    IF_DEBUG_SLEEF { errs() << "SLEEFResolver: " << funcName << " for width " << vectorWidth << "\n"; }
    // query custom mappings with precedence
    std::string funcNameStr = funcName.str();
    for (const auto & vd : commonVectorMappings) {
       IF_DEBUG_SLEEF { errs() << "LISTED " << vd.scalarFnName << " " << vd.vectorWidth << " : " << vd.vectorFnName << "\n"; }
       if (vd.scalarFnName == funcNameStr && vd.vectorWidth == vectorWidth) {
         return true;
       }
    };
    return false;
  }
};









// existing vectorized function wrapper
class ExistingResolver : public FunctionResolver {
  Function & vecFunc;
  VectorShape retShape;

public:
  ExistingResolver(Module & destModule, Function & _vecFunc, VectorShape _retShape)
  : FunctionResolver(destModule)
  , vecFunc(_vecFunc)
  , retShape(_retShape)
  {}

  llvm::Function& requestVectorized() {
    return vecFunc;
  }

  // result shape of function @funcName in target module @module
  VectorShape requestResultShape() {
    return VectorShape::varying();
  }
};

// simply links-in the pre-vectorized SLEEF function
class SleefLookupResolver : public FunctionResolver {
  Function & vecFunc;
  StringRef destFuncName;

  public:
    SleefLookupResolver(Module & _targetModule, Function & _vecFunc, StringRef _destFuncName)
    : FunctionResolver(_targetModule)
    , vecFunc(_vecFunc)
    , destFuncName(_destFuncName)
  {}

  llvm::Function&
  requestVectorized() {
    auto * existingFunc = targetModule.getFunction(destFuncName);
    if (existingFunc) return *existingFunc;
    return cloneFunctionIntoModule(vecFunc, targetModule, destFuncName);
  }

  // result shape of function @funcName in target module @module
  VectorShape requestResultShape() { return VectorShape::varying(); }
};


static
std::string
MangleFunction(StringRef sleefName, const VectorShapeVec & argShapes, int vectorWidth) {
  std::stringstream ss;
  ss << sleefName.str() << "_v" << vectorWidth << "_";
  for (const auto & argShape : argShapes) {
    ss << argShape.serialize();
  }
  return ss.str();
}

// on-the-fly vectorizing resolver
struct SleefVLAResolver : public FunctionResolver {
  VectorizerInterface vectorizer;
  std::unique_ptr<VectorizationInfo> vecInfo;

  Function & scaFunc;
  Function * clonedFunc;
  Function * vecFunc;
  VectorShapeVec argShapes;
  VectorShape resShape;
  int vectorWidth;

  std::string vecFuncName;

  SleefVLAResolver(PlatformInfo & platInfo, StringRef sleefName, Config config, Function & _scaFunc, const VectorShapeVec & _argShapes, int _vectorWidth)
  : FunctionResolver(platInfo.getModule())
  , vectorizer(platInfo, config)
  , vecInfo(nullptr)
  , scaFunc(_scaFunc)
  , clonedFunc(nullptr)
  , vecFunc(nullptr)
  , argShapes(_argShapes)
  , resShape(VectorShape::undef())
  , vectorWidth(_vectorWidth)
  , vecFuncName(MangleFunction(sleefName, argShapes, vectorWidth))
  {
    IF_DEBUG_SLEEF { errs() << "VLA: " << vecFuncName << "\n"; }
  }

  // materialized the vectorized function in the module @insertInto and returns a reference to it
  llvm::Function& requestVectorized() {
    if (vecFunc) return *vecFunc;
    vecFunc = targetModule.getFunction(vecFuncName);
    if (vecFunc) return *vecFunc;

    // need a proper res shape
    requestResultShape();

    // prepare scalar copy for transforming
    const int maskPos = -1; // TODO add support for masking
    clonedFunc = &cloneFunctionIntoModule(scaFunc, targetModule, vecFuncName + ".tmp");
    assert(clonedFunc);

    // create SIMD declaration
    vecFunc = createVectorDeclaration(*clonedFunc, resShape, argShapes, vectorWidth);
    // TODO vecFunc->copyAttributesFrom(callerFunc);
    vecFunc->setName(vecFuncName);

    VectorMapping mapping(clonedFunc, vecFunc, vectorWidth, maskPos, resShape, argShapes);
    vectorizer.getPlatformInfo().addMapping(mapping); // prevent recursive vectorization

    // set-up vecInfo
    FunctionRegion funcWrapper(*clonedFunc);
    Region funcRegion(funcWrapper);
    VectorizationInfo vecInfo(funcRegion, mapping);

    // compute anlaysis results
    PassBuilder PB;
    FunctionAnalysisManager FAM;
    PB.registerFunctionAnalyses(FAM);

    // compute DT, PDT, LI
    auto & DT = FAM.getResult<DominatorTreeAnalysis>(*clonedFunc);
    auto & PDT = FAM.getResult<PostDominatorTreeAnalysis>(*clonedFunc);
    auto & LI = FAM.getResult<LoopAnalysis>(*clonedFunc);
    auto & SE = FAM.getResult<ScalarEvolutionAnalysis>(*clonedFunc);
    auto & MDR = FAM.getResult<MemoryDependenceAnalysis>(*clonedFunc);
    auto & BPI = FAM.getResult<BranchProbabilityAnalysis>(*clonedFunc);

    // run pipeline
    vectorizer.analyze(vecInfo, DT, PDT, LI);
    vectorizer.linearize(vecInfo, DT, PDT, LI, &BPI);
    vectorizer.vectorize(vecInfo, DT, LI, SE, MDR, nullptr);
    vectorizer.finalize();

    return *vecFunc;
  }

  // result shape of function @funcName in target module @module
  VectorShape requestResultShape() {
    if (resShape.isDefined()) return resShape;

    // TODO run VA
    for (const auto & argShape : argShapes) {
      if (!argShape.isUniform()) {
        resShape = VectorShape::varying();
        return resShape;
      }
    }
    resShape = VectorShape::uni();
    return resShape;
  }

  // whether this resolver can provide a vectorized version ofthis function
  bool isVectorizable(const VectorShapeVec & argShapes, int vectorWidth) {
    return true;
  }
};

// used for shape-based call mappings
using VecMappingShortVec = llvm::SmallVector<VectorMapping, 4>;
using VectorFuncMap = std::map<const llvm::Function *, VecMappingShortVec*>;

std::unique_ptr<FunctionResolver>
SleefResolverService::resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule) {
  IF_DEBUG_SLEEF { errs() << "SLEEFResolverService: " << funcName << " for width " << vectorWidth << "\n"; }

  // query custom mappings with precedence
  llvm::StringRef vecFuncName = "";
  std::string funcNameStr = funcName.str();
  for (const auto & vd : commonVectorMappings) {
     if (vd.scalarFnName == funcNameStr &&
        ((vd.vectorWidth <= 0) || (vd.vectorWidth == vectorWidth)))
     {
       vecFuncName = vd.vectorFnName;
       break;
     }
  };
  IF_DEBUG_SLEEF { errs() << "\t n/a\n"; }
  if (vecFuncName.empty()) return nullptr;

  // decode bitwidth (for module lookup)
  bool doublePrecision = false;
  for (const auto * argTy : scaFuncTy.params()) {
    doublePrecision |= argTy->isDoubleTy();
  }

  auto & context = destModule.getContext();

  // remove the trailing isa specifier (_avx2/_avx/_sse/..)
  std::string sleefName = vecFuncName.substr(0, vecFuncName.rfind('_'));

  SleefISA isa;
  if (vecFuncName.count("vla")) isa = SLEEF_VLA;
  else if (vecFuncName.count("avx512")) isa = SLEEF_AVX512;
  else if (vecFuncName.count("avx2")) isa = SLEEF_AVX2;
  else if (vecFuncName.count("avx")) isa = SLEEF_AVX;
  else if (vecFuncName.count("sse")) isa = SLEEF_SSE;
  else if (vecFuncName.count("advsimd")) isa = SLEEF_ADVSIMD;
  else abort(); // unrecognized module

  // TODO factor out
  bool isExtraFunc = vecFuncName.count("_extra");
  if (isExtraFunc) {
    int modIdx = (int) isa;
    auto *& mod = extraModules[modIdx];
    if (!mod) mod = createModuleFromBuffer(reinterpret_cast<const char*>(extraModuleBuffers[modIdx]), extraModuleBufferLens[modIdx], context);
    Function *vecFunc = mod->getFunction(sleefName);
    assert(vecFunc && "mapped extra function not found in module!");
    return std::make_unique<SleefLookupResolver>(destModule, *vecFunc, vecFuncName);
  }

  // Look in SLEEF module
  auto modIndex = sleefModuleIndex(isa, doublePrecision);
  const llvm::Module*& mod = sleefModules[modIndex];
  if (!mod) {
    mod = createModuleFromBuffer(reinterpret_cast<const char*>(sleefModuleBuffers[modIndex]), sleefModuleBufferLens[modIndex], context);

    IF_DEBUG {
      bool brokenMod = verifyModule(*mod, &errs());
      if (brokenMod) abort();
    }
  }

  if (isa == SLEEF_VLA) {
    // on-the-fly vectorization module
    Function *vlaFunc = mod->getFunction(sleefName);
    return std::make_unique<SleefVLAResolver>(platInfo, sleefName, config, *vlaFunc, argShapes, vectorWidth);

  } else {
    // we'll have to link in the function
    Function *vecFunc = mod->getFunction(sleefName);
    assert(vecFunc && "mapped SLEEF function not found in module!");
    return std::make_unique<SleefLookupResolver>(destModule, *vecFunc, vecFuncName);
  }
}




void
addSleefResolver(const Config & config, PlatformInfo & platInfo, bool allowImprecise) {
  auto sleefRes = std::make_unique<SleefResolverService>(platInfo, config, allowImprecise);
  platInfo.addResolverService(std::move(sleefRes), true);
}






// TODO move into separate file
Constant & cloneConstant(Constant& constVal, Module & cloneInto) {
  if (isa<GlobalValue>(constVal)) {
    return cloneGlobalIntoModule(cast<GlobalValue>(constVal), cloneInto);
  }
  auto * expr = dyn_cast<ConstantExpr>(&constVal);
  if (!expr) return constVal;

  // descend into operands and replicate
  const ConstantExpr & constExpr = *expr;

  SmallVector<Constant*, 4> clonedOps;
  for (size_t i = 0; i < constExpr.getNumOperands(); ++i) {
    const Constant & cloned = cloneConstant(*constExpr.getOperand(i), cloneInto);
    clonedOps.push_back(const_cast<Constant*>(&cloned));
  }

  return *constExpr.getWithOperands(clonedOps);
}

GlobalValue & cloneGlobalIntoModule(GlobalValue &gv, Module &cloneInto) {
  if (isa<Function>(gv)) {
    auto & func = cast<Function>(gv);
    return cloneFunctionIntoModule(func, cloneInto, func.getName());

  } else {
    assert(isa<GlobalVariable>(gv));
    auto & global = cast<GlobalVariable>(gv);
    auto * clonedGv = cloneInto.getGlobalVariable(global.getName());
    if (clonedGv) return *clonedGv;

    // clone initializer (could depend on other constants)
    auto * initConst = const_cast<Constant*>(global.getInitializer());
    Constant * clonedInitConst = initConst;
    if (initConst) {
      clonedInitConst = &cloneConstant(*initConst, cloneInto);
    }

    auto * clonedGlobal = cast<GlobalVariable>(cloneInto.getOrInsertGlobal(global.getName(), global.getValueType()));
    clonedGlobal->setInitializer(clonedInitConst);
    clonedGlobal->setThreadLocalMode(global.getThreadLocalMode());
    // TODO set alignment, attributes, ...
    return *clonedGlobal;
  }

  // unsupported global value
  abort();
}

Function &cloneFunctionIntoModule(Function &func, Module &cloneInto, StringRef name) {
  // eg already migrated
  auto * existingFn = cloneInto.getFunction(name);
  if (existingFn && (func.isDeclaration() == existingFn->isDeclaration())) {
    return *existingFn;
  }

  // create function in new module, create the argument mapping, clone function into new function body, return
  Function & clonedFn = *Function::Create(func.getFunctionType(), Function::LinkageTypes::ExternalLinkage,
                                        name, &cloneInto);
  clonedFn.copyAttributesFrom(&func);

  // external decl
  if (func.isDeclaration()) return clonedFn;

  ValueToValueMapTy VMap;
  auto CI = clonedFn.arg_begin();
  for (auto I = func.arg_begin(), E = func.arg_end(); I != E; ++I, ++CI) {
    Argument *arg = &*I, *carg = &*CI;
    carg->setName(arg->getName());
    VMap[arg] = carg;
  }
  // remap constants
  for (auto I = inst_begin(func), E = inst_end(func); I != E; ++I) {
    for (size_t i = 0; i < I->getNumOperands(); ++i) {
      auto * usedConstant = dyn_cast<Constant>(I->getOperand(i));
      if (!usedConstant) continue;

      auto & clonedConstant = cloneConstant(*usedConstant, cloneInto);
      VMap[usedConstant] = &clonedConstant;
    }
  }

  SmallVector<ReturnInst *, 1> Returns; // unused

  CloneFunctionInto(&clonedFn, &func, VMap, false, Returns);
  return clonedFn;
}


// compiler-rt early inlining
Function *
requestScalarImplementation(const StringRef & funcName, FunctionType & funcTy, Module &insertInto) {
#ifdef RV_ENABLE_CRT
  if (!scalarModule) {
    scalarModule = createModuleFromBuffer(reinterpret_cast<const char*>(&crt_Buffer), crt_BufferLen, insertInto.getContext());
  }

  if (!scalarModule) return nullptr; // could not load module

  auto * scalarFn = scalarModule->getFunction(funcName);
  if (!scalarFn) return nullptr;
  return &cloneFunctionIntoModule(*scalarFn, insertInto, funcName);
#else
  return nullptr; // compiler-rt not available as bc module
#endif
}



} // namespace rv
