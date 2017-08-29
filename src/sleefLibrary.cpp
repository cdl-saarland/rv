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
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>

#include "utils/rvTools.h"
#include "rvConfig.h"

#include <llvm/IR/Verifier.h>

using namespace llvm;

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
  enum SleefISA {
    SLEEF_SSE  = 0,
    SLEEF_AVX  = 1,
    SLEEF_AVX2 = 2,
    SLEEF_AVX512 = 3,
    SLEEF_ADVSIMD = 4
  };

  inline int sleefModuleIndex(SleefISA isa, bool doublePrecision) {
    return int(isa) + (doublePrecision ? 5 : 0);
  }


  static const size_t sleefModuleBufferLens[] = {
      sse_sp_BufferLen,
      avx_sp_BufferLen,
      avx2_sp_BufferLen,
      avx512_sp_BufferLen,
      advsimd_sp_BufferLen,

      sse_dp_BufferLen,
      avx_dp_BufferLen,
      avx2_dp_BufferLen,
      avx512_dp_BufferLen,
      advsimd_dp_BufferLen,
  };

  static const unsigned char** sleefModuleBuffers[] = {
      &sse_sp_Buffer,
      &avx_sp_Buffer,
      &avx2_sp_Buffer,
      &avx512_sp_Buffer,
      &advsimd_sp_Buffer,

      &sse_dp_Buffer,
      &avx_dp_Buffer,
      &avx2_dp_Buffer,
      &avx512_dp_Buffer,
      &advsimd_dp_Buffer,
  };

  static const size_t extraModuleBufferLens[] = {
      0, // SSE
      0, // AVX
      avx2_extras_BufferLen,
      avx512_extras_BufferLen,
      advsimd_extras_BufferLen,
  };

  static const unsigned char** extraModuleBuffers[] = {
      nullptr, // SSE
      nullptr, // AVX
      &avx2_extras_Buffer,
      &avx512_extras_Buffer,
      &advsimd_extras_Buffer,
  };
static
void
AddMappings_SSE(PlatformInfo & platInfo, bool allowImprecise) {
      const VecDesc VecFuncs[] = {
//        {"ldexpf", "xldexpf_sse2", 4},
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

//        {"ldexp", "xldexp_sse2", 2},
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
          {"llvm.copysign.f32", "xcopysignf_sse", 4},
          {"llvm.fmin.f32", "xfminf_sse", 4},
          {"llvm.fmax.f32", "xfmaxf_sse", 4},
          {"llvm.fabs.f64", "xfabs_sse", 2},
          {"llvm.copysign.f64", "xcopysign_sse", 2},
          {"llvm.fmin.f64", "xfmin_sse", 2},
          {"llvm.fmax.f64", "xfmax_sse", 2}
      };
      platInfo.addVectorizableFunctions(VecFuncs, true);

      if (allowImprecise) {
        const VecDesc ImprecVecFuncs[] = {
//          {"sinf", "xsinf_sse2", 4},
//          {"cosf", "xcosf_sse2", 4},
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
            {"sqrtf", "xsqrtf_u05_sse2", 4},
            {"hypotf", "xhypotf_u05_sse2", 4},
            {"lgammaf", "xlgammaf_u1_sse2", 4},
            {"tgammaf", "xtgammaf_u1_sse2", 4},
            {"erff", "xerff_u1_sse2", 4},
            {"erfcf", "xerfcf_u15_sse2", 4},

//          {"sin", "xsin_sse2", 2},
//          {"cos", "xcos_sse2", 2},
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
            {"sqrt", "xsqrt_u05_sse2", 2},
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
            {"llvm.sqrt.f32", "xsqrtf_u05_sse", 4},
            {"llvm.exp2.f32", "xexp2f_sse", 4},
            {"llvm.log10.f32", "xlog10f_sse", 4},
            {"llvm.sin.f64", "xsin_sse", 2},
            {"llvm.cos.f64", "xcos_sse", 2},
            {"llvm.log.f64", "xlog_sse", 2},
            {"llvm.exp.f64", "xexp_sse", 2},
            {"llvm.pow.f64", "xpow_sse", 2},
            {"llvm.sqrt.f64", "xsqrt_u05_sse", 2},
            {"llvm.exp2.f64", "xexp2_sse", 2},
            {"llvm.log10.f64", "xlog10_sse", 2}
        };
        platInfo.addVectorizableFunctions(ImprecVecFuncs, true);
      }
}

static
void
AddMappings_AVX(PlatformInfo & platInfo, bool allowImprecise) {
      const VecDesc VecFuncs[] = {
//        {"ldexpf", "xldexpf_avx", 8},
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

//        {"ldexp", "xldexp_avx", 4},
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
          {"llvm.copysign.f32", "xcopysignf_avx", 8},
          {"llvm.minnum.f32", "xfminf_avx", 8},
          {"llvm.maxnum.f32", "xfmaxf_avx", 8},
          {"llvm.fabs.f64", "xfabs_avx", 4},
          {"llvm.copysign.f64", "xcopysign_avx", 4},
          {"llvm.minnum.f64", "xfmin_avx", 4},
          {"llvm.maxnum.f64", "xfmax_avx", 4}
      };
      platInfo.addVectorizableFunctions(VecFuncs, true);

      if (allowImprecise) {
        const VecDesc ImprecVecFuncs[] = {
//          {"sinf", "xsinf_avx", 8},
//          {"cosf", "xcosf_avx", 8},
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
            {"sqrtf", "xsqrtf_u05_avx", 8},
            {"hypotf", "xhypotf_u05_avx", 8},
            {"lgammaf", "xlgammaf_u1_avx", 8},
            {"tgammaf", "xtgammaf_u1_avx", 8},
            {"erff", "xerff_u1_avx", 8},
            {"erfcf", "xerfcf_u15_avx", 8},

//          {"sin", "xsin_avx", 4},
//          {"cos", "xcos_avx", 4},
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
            {"sqrt", "xsqrt_u05_avx", 4},
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
            {"llvm.sqrt.f32", "xsqrtf_u05_avx", 8},
            {"llvm.exp2.f32", "xexp2f_avx", 8},
            {"llvm.log10.f32", "xlog10f_avx", 8},
            {"llvm.sin.f64", "xsin_avx", 4},
            {"llvm.cos.f64", "xcos_avx", 4},
            {"llvm.log.f64", "xlog_avx", 4},
            {"llvm.exp.f64", "xexp_avx", 4},
            {"llvm.pow.f64", "xpow_avx", 4},
            {"llvm.sqrt.f64", "xsqrt_u05_avx", 4},
            {"llvm.exp2.f64", "xexp2_avx", 4},
            {"llvm.log10.f64", "xlog10_avx", 4}
        };
        platInfo.addVectorizableFunctions(ImprecVecFuncs, true);
      }
}


static
void
AddMappings_AVX2(PlatformInfo & platInfo, bool allowImprecise) {
      const VecDesc VecFuncs[] = {
//        {"ldexpf", "xldexpf_avx2", 8},
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

//        {"ldexp", "xldexp_avx2", 4},
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
      platInfo.addVectorizableFunctions(VecFuncs, true);






      if (allowImprecise) {
        const VecDesc ImprecVecFuncs[] = {
//          {"sinf", "xsinf_avx2", 8},
//          {"cosf", "xcosf_avx2", 8},
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
            {"sqrtf", "xsqrtf_u05_avx2", 8},
            {"hypotf", "xhypotf_u05_avx2", 8},
            {"lgammaf", "xlgammaf_u1_avx2", 8},
            {"tgammaf", "xtgammaf_u1_avx2", 8},
            {"erff", "xerff_u1_avx2", 8},
            {"erfcf", "xerfcf_u15_avx2", 8},

//          {"sin", "xsin_avx2", 4},
//          {"cos", "xcos_avx2", 4},
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
            {"sqrt", "xsqrt_u05_avx2", 4},
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
            {"llvm.sqrt.f32", "xsqrtf_u05_avx2", 8},
            {"llvm.exp2.f32", "xexp2f_avx2", 8},
            {"llvm.log10.f32", "xlog10f_avx2", 8},
            {"llvm.sin.f64", "xsin_avx2", 4},
            {"llvm.cos.f64", "xcos_avx2", 4},
            {"llvm.log.f64", "xlog_avx2", 4},
            {"llvm.exp.f64", "xexp_avx2", 4},
            {"llvm.pow.f64", "xpow_avx2", 4},
            {"llvm.sqrt.f64", "xsqrt_u05_avx2", 4},
            {"llvm.exp2.f64", "xexp2_avx2", 4},
            {"llvm.log10.f64", "xlog10_avx2", 4}
        };
        platInfo.addVectorizableFunctions(ImprecVecFuncs, true);
      }
}

static
void
AddMappings_AVX512(PlatformInfo & platInfo, bool allowImprecise) {
      const VecDesc VecFuncs[] = {
//        {"ldexpf", "xldexpf_avx512", 16},
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

//        {"ldexp", "xldexp_avx512", 8},
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
      platInfo.addVectorizableFunctions(VecFuncs, true);






      if (allowImprecise) {
        const VecDesc ImprecVecFuncs[] = {
//          {"sinf", "xsinf_avx512", 16},
//          {"cosf", "xcosf_avx512", 16},
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
            {"sqrtf", "xsqrtf_u05_avx512", 16},
            {"hypotf", "xhypotf_u05_avx512", 16},
            {"lgammaf", "xlgammaf_u1_avx512", 16},
            {"tgammaf", "xtgammaf_u1_avx512", 16},
            {"erff", "xerff_u1_avx512", 16},
            {"erfcf", "xerfcf_u15_avx512", 16},

//          {"sin", "xsin_avx512", 8},
//          {"cos", "xcos_avx512", 8},
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
            {"sqrt", "xsqrt_u05_avx512", 8},
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
            {"llvm.sqrt.f32", "xsqrtf_u05_avx512", 16},
            {"llvm.exp2.f32", "xexp2f_avx512", 16},
            {"llvm.log10.f32", "xlog10f_avx512", 16},
            {"llvm.sin.f64", "xsin_avx512", 8},
            {"llvm.cos.f64", "xcos_avx512", 8},
            {"llvm.log.f64", "xlog_avx512", 8},
            {"llvm.exp.f64", "xexp_avx512", 8},
            {"llvm.pow.f64", "xpow_avx512", 8},
            {"llvm.sqrt.f64", "xsqrt_u05_avx512", 8},
            {"llvm.exp2.f64", "xexp2_avx512", 8},
            {"llvm.log10.f64", "xlog10_avx512", 8}
        };
        platInfo.addVectorizableFunctions(ImprecVecFuncs, true);
      }
}

static
void
AddMappings_ADVSIMD(PlatformInfo & platInfo, bool allowImprecise) {
      const VecDesc VecFuncs[] = {
//        {"ldexpf", "xldexpf_advsimd", 2},
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

//        {"ldexp", "xldexp_advsimd", 8},
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
      platInfo.addVectorizableFunctions(VecFuncs, true);






      if (allowImprecise) {
        const VecDesc ImprecVecFuncs[] = {
//          {"sinf", "xsinf_advsimd", 4},
//          {"cosf", "xcosf_advsimd", 4},
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
            {"sqrtf", "xsqrtf_u05_advsimd", 4},
            {"hypotf", "xhypotf_u05_advsimd", 4},
            {"lgammaf", "xlgammaf_u1_advsimd", 4},
            {"tgammaf", "xtgammaf_u1_advsimd", 4},
            {"erff", "xerff_u1_advsimd", 4},
            {"erfcf", "xerfcf_u15_advsimd", 4},

//          {"sin", "xsin_advsimd", 2},
//          {"cos", "xcos_advsimd", 2},
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
            {"sqrt", "xsqrt_u05_advsimd", 2},
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
            {"llvm.sqrt.f32", "xsqrtf_u05_advsimd", 4},
            {"llvm.exp2.f32", "xexp2f_advsimd", 4},
            {"llvm.log10.f32", "xlog10f_advsimd", 4},
            {"llvm.sin.f64", "xsin_advsimd", 2},
            {"llvm.cos.f64", "xcos_advsimd", 2},
            {"llvm.log.f64", "xlog_advsimd", 2},
            {"llvm.exp.f64", "xexp_advsimd", 2},
            {"llvm.pow.f64", "xpow_advsimd", 2},
            {"llvm.sqrt.f64", "xsqrt_u05_advsimd", 2},
            {"llvm.exp2.f64", "xexp2_advsimd", 2},
            {"llvm.log10.f64", "xlog10_advsimd", 2}
        };
        platInfo.addVectorizableFunctions(ImprecVecFuncs, true);
      }
}

  static Module const *sleefModules[5 * 2];
  static Module const *extraModules[5 * 2];
  static Module* scalarModule; // scalar implementations to be inlined

  bool addSleefMappings(const Config & config,
                        PlatformInfo &platInfo,
                        bool allowImprecise) {
    if (!config.useSLEEF)
      return false;

#ifdef RV_ENABLE_BUILTINS
    if (config.useADVSIMD) {
      AddMappings_ADVSIMD(platInfo, allowImprecise);
    } else {
      // TODO add neon32 mappings
    }

    if (config.useSSE || config.useAVX || config.useAVX2 || config.useAVX512) {
      AddMappings_SSE(platInfo, allowImprecise);
    }

    if (config.useAVX) {
      AddMappings_AVX(platInfo, allowImprecise);
    }

    if (config.useAVX2 || config.useAVX512) {
      AddMappings_AVX2(platInfo, allowImprecise);
    }
    if (config.useAVX512) {
      AddMappings_AVX512(platInfo, allowImprecise);
    }

    // TODO generic mappings

    return config.useAVX || config.useAVX2 || config.useSSE || config.useAVX512 || config.useADVSIMD;

#else
    return false;
#endif
  }

  // forward decls
  GlobalValue & cloneGlobalIntoModule(GlobalValue &gv, Module &cloneInto);
  Constant & cloneConstant(Constant& constVal, Module & cloneInto);
  Function &cloneFunctionIntoModule(Function &func, Module &cloneInto, StringRef name);


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
#if 0
    if (func.isIntrinsic()) {

      // decode ambiguous type arguments
      auto id = func.getIntrinsicID();
      auto * funcTy = func.getFunctionType();
      SmallVector<Intrinsic::IITDescriptor, 4> paramDescs;
      Intrinsic::getIntrinsicInfoTableEntries(id, paramDescs);
      SmallVector<Type*,4> overloadedTypes;

      // append ambiguous types
      SmallSet<uint, 4> mangledArgs;
      for (auto desc : paramDescs) {
        if (desc.Kind != Intrinsic::IITDescriptor::Argument) continue;
        switch (desc.getArgumentKind()) {
          case llvm::Intrinsic::IITDescriptor::AK_Any:
            break;
          default:
            uint argIdx = desc.getArgumentNumber();
            if (!mangledArgs.insert(argIdx).second) continue; // already mangled that arg
            overloadedTypes.push_back(funcTy->getParamType(argIdx));
            break;
        }
      }

      return *Intrinsic::getDeclaration(&cloneInto, func.getIntrinsicID(), overloadedTypes);
    }
#endif

    // eg already migrated
    auto * existingFn = cloneInto.getFunction(func.getName());
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

    // remove the trailing isa specifier (_avx2/_avx/_sse/..)
    std::string sleefName = vecFuncName.substr(0, vecFuncName.rfind('_'));

    SleefISA isa;
    if (vecFuncName.count("avx512")) isa = SLEEF_AVX512;
    else if (vecFuncName.count("avx2")) isa = SLEEF_AVX2;
    else if (vecFuncName.count("avx")) isa = SLEEF_AVX;
    else if (vecFuncName.count("sse")) isa = SLEEF_SSE;
    else if (vecFuncName.count("advsimd")) isa = SLEEF_ADVSIMD;
    else return nullptr;

    bool isExtraFunc = vecFuncName.count("_extra");
    if (isExtraFunc) {
      int modIdx = (int) isa;
      auto *& mod = extraModules[modIdx];
      if (!mod) mod = createModuleFromBuffer(reinterpret_cast<const char*>(extraModuleBuffers[modIdx]), extraModuleBufferLens[modIdx], context);
      Function *vecFunc = mod->getFunction(sleefName);
      assert(vecFunc && "mapped extra function not found in module!");
      return &cloneFunctionIntoModule(*vecFunc, *insertInto, vecFuncName);
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

    Function *vecFunc = mod->getFunction(sleefName);
    assert(vecFunc && "mapped SLEEF function not found in module!");
    return &cloneFunctionIntoModule(*vecFunc, *insertInto, vecFuncName);
  }
}
