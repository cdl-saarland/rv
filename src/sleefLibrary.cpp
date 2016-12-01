//===- sleefLibrary.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#include "rv/sleefLibrary.h"

using namespace llvm;

namespace rv {
  bool addSleefMappings(const bool useSSE, const bool useAVX, const bool useAVX2, PlatformInfo &platformInfo) {

    if (useAVX2) {
      const VecDesc VecFuncs[] = {
          {"ldexpf", "xldexpf_avx2", 8},
          {"sinf", "xsinf_avx2", 8},
          {"cosf", "xcosf_avx2", 8},
          {"sincosf", "xsincosf_avx2", 8},
          {"tanf", "xtanf_avx2", 8},
          {"atanf", "xatanf_avx2", 8},
          {"atan2f", "xatan2f_avx2", 8},
          {"asinf", "xasinf_avx2", 8},
          {"acosf", "xacosf_avx2", 8},
          {"logf", "xlogf_avx2", 8},
          {"expf", "xexpf_avx2", 8},
          {"sqrtf", "xsqrtf_avx2", 8},
          {"cbrtf", "xcbrtf_avx2", 8},
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
          {"log1pf", "xlog1pf_avx2", 8}
      };

      platformInfo.addVectorizableFunctions(VecFuncs);
    }

    if (useAVX) {
      const VecDesc VecFuncs[] = {
          {"ldexpf", "xldexpf_avx", 8},
          {"sinf", "xsinf_avx", 8},
          {"cosf", "xcosf_avx", 8},
          {"sincosf", "xsincosf_avx", 8},
          {"tanf", "xtanf_avx", 8},
          {"atanf", "xatanf_avx", 8},
          {"atan2f", "xatan2f_avx", 8},
          {"asinf", "xasinf_avx", 8},
          {"acosf", "xacosf_avx", 8},
          {"logf", "xlogf_avx", 8},
          {"expf", "xexpf_avx", 8},
          {"sqrtf", "xsqrtf_avx", 8},
          {"cbrtf", "xcbrtf_avx", 8},
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
      };

      platformInfo.addVectorizableFunctions(VecFuncs);
    }

    if (useSSE || useAVX || useAVX2) {
      const VecDesc VecFuncs[] = {
          {"ldexpf", "xldexpf_sse", 4},
          {"sinf", "xsinf_sse", 4},
          {"cosf", "xcosf_sse", 4},
          {"sincosf", "xsincosf_sse", 4},
          {"tanf", "xtanf_sse", 4},
          {"atanf", "xatanf_sse", 4},
          {"atan2f", "xatan2f_sse", 4},
          {"asinf", "xasinf_sse", 4},
          {"acosf", "xacosf_sse", 4},
          {"logf", "xlogf_sse", 4},
          {"expf", "xexpf_sse", 4},
          {"sqrtf", "xsqrtf_sse", 4},
          {"cbrtf", "xcbrtf_sse", 4},
          {"powf", "xpowf_sse", 4},
          {"sinhf", "xsinhf_sse", 4},
          {"coshf", "xcoshf_sse", 4},
          {"tanhf", "xtanhf_sse", 4},
          {"asinhf", "xasinhf_sse", 4},
          {"acoshf", "xacoshf_sse", 4},
          {"atanhf", "xatanhf_sse", 4},
          {"exp2f", "xexp2f_sse", 4},
          {"exp10f", "xexp10f_sse", 4},
          {"expm1f", "xexpm1f_sse", 4},
          {"log10f", "xlog10f_sse", 4},
          {"log1pf", "xlog1pf_sse", 4}
      };

      platformInfo.addVectorizableFunctions(VecFuncs);
    }
    return useAVX || useAVX2 || useSSE;
  }
}