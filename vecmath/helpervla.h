//          Copyright Naoki Shibata 2010 - 2017.
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#include <stdint.h>
#include <math.h>
#include "misc.h"
#include <assert.h>
#include <stdbool.h>
// for abort() - used for unimplemented features
#include <stdlib.h>


typedef int32_t lane_t;

// RV predicate instrinsics
int64_t rv_lane(); // current lane id
inline int64_t rv_strided(int64_t base, int64_t stride) {
  return base + rv_lane() * stride;
}

bool rv_all(bool value);
bool rv_any(bool value);
int rv_swizzle(int value, int blockSize);

#define ENABLE_DP
#define ENABLE_SP

#define LOG2VECTLENDP 1
#define VECTLENDP 1
#define LOG2VECTLENSP 1
#define VECTLENSP 1

#define ACCURATE_SQRT

#define DFTPRIORITY LOG2VECTLENDP
#define ISANAME "VLA"

typedef uint64_t vmask;
typedef bool vopmask;
typedef double vdouble;
typedef int32_t vint;
typedef float vfloat;
typedef int64_t vint2;


typedef long double longdoubleVector;

typedef longdoubleVector vmaskl;
typedef longdoubleVector vlongdouble;

#ifdef Sleef_quad2_DEFINED
typedef union {
  uint8_t u[sizeof(Sleef_quad)*VECTLENDP];
  Sleef_quad q[VECTLENDP];
} quadVector;

typedef quadVector vmaskq;
typedef quadVector vquad;
#endif

//

static INLINE int vavailability_i(int name) { return -1; }
static INLINE void vprefetch_v_p(const void *ptr) { }

static INLINE int vtestallones_i_vo64(vopmask g) { return rv_all(g); }

static INLINE int vtestallones_i_vo32(vopmask g) { return rv_all(g); }

//

static vint2 vloadu_vi2_p(int32_t *p) { return p[rv_lane()]; }

static void vstoreu_v_p_vi2(int32_t *p, vint2 v) {
  p[rv_lane()] = v;
}

static vint vloadu_vi_p(int32_t *p) {
  return p[rv_lane()];
}

static void vstoreu_v_p_vi(int32_t *p, vint v) {
  p[rv_lane()] = v;
}

//

static INLINE vopmask vcast_vo32_vo64(vopmask m) {
  return m;
}

static INLINE vopmask vcast_vo64_vo32(vopmask m) {
  return m;
}

extern vmask vcast_vm_i_i(int h, int l) {
  return (((uint64_t) h) << 32 | (uint32_t) l);
}

static INLINE vint2 vcastu_vi2_vi(vint vi) {
  return ((vmask) vi) << 32;
#if 0
  vint2 ret;
  for(int i=0;i<VECTLENDP;i++) {
    ret.i[i*2+0] = 0;
    ret.i[i*2+1] = vi.i[i];
  }
  return ret;
#endif
}

static INLINE vint vcastu_vi_vi2(vint2 vi2) {
  return vi2; // FIXME
#if 0
  vint ret;
  for(int i=0;i<VECTLENDP;i++) ret.i[i] = vi2.i[i*2+1];
  return ret;
#endif
}

static INLINE vint vreinterpretFirstHalf_vi_vi2(vint2 vi2) {
  return vi2; // FIXME
}

static INLINE vint2 vreinterpretFirstHalf_vi2_vi(vint vi) {
  return vi;
}

extern vdouble vrev21_vd_vd(vdouble d0);
// {
//   abort();
// #if 0
//   vdouble r;
//   for(int i=0;i<VECTLENDP/2;i++) {
//     r.d[i*2+0] = d0.d[i*2+1];
//     r.d[i*2+1] = d0.d[i*2+0];
//   }
//   return r;
// #endif
// }

extern vdouble vreva2_vd_vd(vdouble d0);
// {
//   abort();
// #if 0
//   vdouble r;
//   for(int i=0;i<VECTLENDP/2;i++) {
//     r.d[i*2+0] = d0.d[(VECTLENDP/2-1-i)*2+0];
//     r.d[i*2+1] = d0.d[(VECTLENDP/2-1-i)*2+1];
//   }
//   return r;
// #endif
// }

extern vfloat vrev21_vf_vf(vfloat d0);
// {
//   abort();
// #if 0
//   vfloat r;
//   for(int i=0;i<VECTLENSP/2;i++) {
//     r.f[i*2+0] = d0.f[i*2+1];
//     r.f[i*2+1] = d0.f[i*2+0];
//   }
//   return r;
// #endif
// }

extern vfloat vreva2_vf_vf(vfloat d0);
// {
//   abort();
// #if 0
//   vfloat r;
//   for(int i=0;i<VECTLENSP/2;i++) {
//     r.f[i*2+0] = d0.f[(VECTLENSP/2-1-i)*2+0];
//     r.f[i*2+1] = d0.f[(VECTLENSP/2-1-i)*2+1];
//   }
//   return r;
// #endif
// }

static INLINE vdouble vcast_vd_d(double d) { return d; }

//

static INLINE vopmask vand_vo_vo_vo   (vopmask x, vopmask y) { return x &  y; }
static INLINE vopmask vandnot_vo_vo_vo(vopmask x, vopmask y) { return y & ~x; }
static INLINE vopmask vor_vo_vo_vo    (vopmask x, vopmask y) { return x |  y; }
static INLINE vopmask vxor_vo_vo_vo   (vopmask x, vopmask y) { return x ^  y; }

static INLINE vmask vand_vm_vm_vm     (vmask x, vmask y)     { return x &  y; }
static INLINE vmask vandnot_vm_vm_vm  (vmask x, vmask y)     { return y & ~x; }
static INLINE vmask vor_vm_vm_vm      (vmask x, vmask y)     { return x |  y; }
static INLINE vmask vxor_vm_vm_vm     (vmask x, vmask y)     { return x ^  y; }

static INLINE vmask vand_vm_vo64_vm(vopmask x, vmask y)      { return x &  y; }
static INLINE vmask vandnot_vm_vo64_vm(vopmask x, vmask y)   { return y & ~x; }
static INLINE vmask vor_vm_vo64_vm(vopmask x, vmask y)       { return x |  y; }
static INLINE vmask vxor_vm_vo64_vm(vopmask x, vmask y)      { return x ^  y; }

static INLINE vmask vand_vm_vo32_vm(vopmask x, vmask y)      { return x &  y; }
static INLINE vmask vandnot_vm_vo32_vm(vopmask x, vmask y)   { return y & ~x; }
static INLINE vmask vor_vm_vo32_vm(vopmask x, vmask y)       { return x |  y; }
static INLINE vmask vxor_vm_vo32_vm(vopmask x, vmask y)      { return x ^  y; }

//

static INLINE vdouble vsel_vd_vo_vd_vd   (vopmask o, vdouble x, vdouble y) { return o ? x : y; }
static INLINE vint2   vsel_vi2_vo_vi2_vi2(vopmask o, vint2 x, vint2 y)     { return o ? x : y; }

static INLINE CONST vdouble vsel_vd_vo_d_d(vopmask o, double v1, double v0) {
  return vsel_vd_vo_vd_vd(o, vcast_vd_d(v1), vcast_vd_d(v0));
}

static INLINE vdouble vsel_vd_vo_vo_d_d_d(vopmask o0, vopmask o1, double d0, double d1, double d2) {
  return vsel_vd_vo_vd_vd(o0, vcast_vd_d(d0), vsel_vd_vo_d_d(o1, d1, d2));
}

static INLINE vdouble vsel_vd_vo_vo_vo_d_d_d_d(vopmask o0, vopmask o1, vopmask o2, double d0, double d1, double d2, double d3) {
  return vsel_vd_vo_vd_vd(o0, vcast_vd_d(d0), vsel_vd_vo_vd_vd(o1, vcast_vd_d(d1), vsel_vd_vo_d_d(o2, d2, d3)));
}

static INLINE vdouble vcast_vd_vi(vint vi) { return vi; }
static INLINE vint vtruncate_vi_vd(vdouble vd) { return (vint) vd; }
static INLINE vint vrint_vi_vd(vdouble vd) { vint ret; return vd > 0 ? (int)(vd + 0.5) : (int)(vd - 0.5); }
static INLINE vdouble vtruncate_vd_vd(vdouble vd) { return vcast_vd_vi(vtruncate_vi_vd(vd)); }
static INLINE vdouble vrint_vd_vd(vdouble vd) { return vcast_vd_vi(vrint_vi_vd(vd)); }
static INLINE vint vcast_vi_i(int j) { return j; }

static INLINE vopmask veq64_vo_vm_vm(vmask x, vmask y) { return x == y; }
static INLINE vmask vadd64_vm_vm_vm(vmask x, vmask y) { return x + y; }

//

static INLINE vmask vreinterpret_vm_vd(vdouble vd) { return *(vmask*) &vd; } //union { vdouble vd; vmask vm; } cnv; cnv.vd = vd; return cnv.vm; }
static INLINE vint2 vreinterpret_vi2_vd(vdouble vd) { return *(int64_t*) &vd; } //union { vdouble vd; vint2 vi2; } cnv; cnv.vd = vd; return cnv.vi2; }
static INLINE vdouble vreinterpret_vd_vi2(vint2 vi) { return *(double*) &vi; } //union { vint2 vi2; vdouble vd; } cnv; cnv.vi2 = vi; return cnv.vd; }
static INLINE vdouble vreinterpret_vd_vm(vmask vm)  { return *(vdouble*) &vm; } //union { vmask vm; vdouble vd; } cnv; cnv.vm = vm; return cnv.vd; }

static INLINE vdouble vadd_vd_vd_vd(vdouble x, vdouble y) { return x + y; }
static INLINE vdouble vsub_vd_vd_vd(vdouble x, vdouble y) { return x - y; }
static INLINE vdouble vmul_vd_vd_vd(vdouble x, vdouble y) { return x * y; }
static INLINE vdouble vdiv_vd_vd_vd(vdouble x, vdouble y) { return x / y; }
static INLINE vdouble vrec_vd_vd(vdouble x)               { return 1.0 / x; }

static INLINE vdouble vabs_vd_vd(vdouble d) { uint64_t tmp = (*((uint64_t*) &d) & 0x7fffffffffffffffULL); return *(double*) tmp; }
static INLINE vdouble vneg_vd_vd(vdouble d) { return -d; }
static INLINE vdouble vmla_vd_vd_vd_vd  (vdouble x, vdouble y, vdouble z) { return x * y + z; }
static INLINE vdouble vmlapn_vd_vd_vd_vd(vdouble x, vdouble y, vdouble z) { return x * y - z; }
static INLINE vdouble vmax_vd_vd_vd(vdouble x, vdouble y) { return x > y ? x : y; }
static INLINE vdouble vmin_vd_vd_vd(vdouble x, vdouble y) { return x < y ? x : y; }

static INLINE vdouble vposneg_vd_vd(vdouble d) { lane_t i = rv_lane(); return (i & 1) == 0 ?  d : -d; }
static INLINE vdouble vnegpos_vd_vd(vdouble d) { lane_t i = rv_lane(); return (i & 1) == 0 ? -d :  d; }
static INLINE vdouble vsubadd_vd_vd_vd(vdouble x, vdouble y) { lane_t i = rv_lane(); return (i & 1) == 0 ? x - y : x + y; }
static INLINE vdouble vmlsubadd_vd_vd_vd_vd(vdouble x, vdouble y, vdouble z) { return vsubadd_vd_vd_vd(vmul_vd_vd_vd(x, y), z); }

static INLINE vopmask veq_vo_vd_vd(vdouble x, vdouble y)  { return x == y ? -1 : 0; }
static INLINE vopmask vneq_vo_vd_vd(vdouble x, vdouble y) { return x != y ? -1 : 0; }
static INLINE vopmask vlt_vo_vd_vd(vdouble x, vdouble y)  { return x <  y ? -1 : 0; }
static INLINE vopmask vle_vo_vd_vd(vdouble x, vdouble y)  { return x <= y ? -1 : 0; }
static INLINE vopmask vgt_vo_vd_vd(vdouble x, vdouble y)  { return x >  y ? -1 : 0; }
static INLINE vopmask vge_vo_vd_vd(vdouble x, vdouble y)  { return x >= y ? -1 : 0; }

static INLINE vint vadd_vi_vi_vi(vint x, vint y) { return x + y; }
static INLINE vint vsub_vi_vi_vi(vint x, vint y) { return x - y; }
static INLINE vint vneg_vi_vi   (vint x)         { return -x; }

static INLINE vint vand_vi_vi_vi(vint x, vint y)    { return x &  y; }
static INLINE vint vandnot_vi_vi_vi(vint x, vint y) { return y & ~x; }
static INLINE vint vor_vi_vi_vi(vint x, vint y)     { return x |  y; }
static INLINE vint vxor_vi_vi_vi(vint x, vint y)    { return x ^  y; }

static INLINE vint vand_vi_vo_vi(vopmask x, vint y)    { return vand_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(x), y); }
static INLINE vint vandnot_vi_vo_vi(vopmask x, vint y) { return vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(x), y); }

static INLINE vint vsll_vi_vi_i(vint x, int c) { return x << c; }
static INLINE vint vsrl_vi_vi_i(vint x, int c) { return ((uint32_t)x) >> c; }
static INLINE vint vsra_vi_vi_i(vint x, int c) { return x >> c; }

static INLINE vopmask veq_vo_vi_vi(vint x, vint y) { return x == y ? -1 : 0; }
static INLINE vopmask vgt_vo_vi_vi(vint x, vint y) { return x >  y ? -1 : 0; }

static INLINE vint vsel_vi_vo_vi_vi(vopmask m, vint x, vint y) {
  return m > x ? x : y;
#if 0
  union { vopmask vo; vint2 vi2; } cnv;
  cnv.vo = m;
  return vor_vi_vi_vi(vand_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(cnv.vi2), x),
		      vandnot_vi_vi_vi(vreinterpretFirstHalf_vi_vi2(cnv.vi2), y));
#endif
}

static INLINE vopmask visinf_vo_vd(vdouble d)  { return (d == SLEEF_INFINITY || d == -SLEEF_INFINITY) ? -1 : 0; }
static INLINE vopmask vispinf_vo_vd(vdouble d) { return d == SLEEF_INFINITY ? -1 : 0; }
static INLINE vopmask visminf_vo_vd(vdouble d) { return d == -SLEEF_INFINITY ? -1 : 0; }
static INLINE vopmask visnan_vo_vd(vdouble d)  { return d != d ? -1 : 0; }

static INLINE vdouble vsqrt_vd_vd(vdouble d) { return sqrt(d); }

#if defined(_MSC_VER)
// This function is needed when debugging on MSVC.
static INLINE double vcast_d_vd(vdouble v) { return v; }
#endif

static INLINE vdouble vload_vd_p(const double *ptr) { return *(vdouble *)ptr; }
static INLINE vdouble vloadu_vd_p(const double *ptr) { return ptr[rv_lane()]; }

static INLINE void vstore_v_p_vd(double *ptr, vdouble v) { *(vdouble *)ptr = v; }
static INLINE void vstoreu_v_p_vd(double *ptr, vdouble v) { ptr[rv_lane()] = v; }
static INLINE void vstream_v_p_vd(double *ptr, vdouble v) { *(vdouble *)ptr = v; }

static INLINE void vscatter2_v_p_i_i_vd(double *ptr, int offset, int step, vdouble v) {
  ptr[rv_strided(offset, step)] = v;
}

static INLINE void vsscatter2_v_p_i_i_vd(double *ptr, int offset, int step, vdouble v) { vscatter2_v_p_i_i_vd(ptr, offset, step, v); }

//

static INLINE vint2 vcast_vi2_vm(vmask vm) { return *(vint2*) &vm; }
static INLINE vmask vcast_vm_vi2(vint2 vi) { return *(vmask*) &vi; }

static INLINE vfloat vcast_vf_vi2(vint2 vi) { return (float) vi; }
static INLINE vint2 vtruncate_vi2_vf(vfloat vf) { return (vint2) vf; }
static INLINE vint2 vrint_vi2_vf(vfloat vf) { return vf > 0 ? (int)(vf + 0.5) : (int)(vf - 0.5); }
static INLINE vint2 vcast_vi2_i(int j) { return (int64_t) j; }
static INLINE vfloat vtruncate_vf_vf(vfloat vd) { return vcast_vf_vi2(vtruncate_vi2_vf(vd)); }
static INLINE vfloat vrint_vf_vf(vfloat vd) { return vcast_vf_vi2(vrint_vi2_vf(vd)); }

static INLINE vfloat vcast_vf_f(float f) { return f; }
static INLINE vmask vreinterpret_vm_vf(vfloat vf) { return (uint64_t) *(int32_t*) &vf; } // zext
static INLINE vfloat vreinterpret_vf_vm(vmask vm) { return *(float*) &vm; } // truncate
static INLINE vfloat vreinterpret_vf_vi2(vint2 vi) { return *(vfloat*) &vi; }
static INLINE vint2 vreinterpret_vi2_vf(vfloat vf) { return (vint2) *(vint*) &vf; }

static INLINE vfloat vadd_vf_vf_vf(vfloat x, vfloat y) { return x + y; }
static INLINE vfloat vsub_vf_vf_vf(vfloat x, vfloat y) { return x - y; }
static INLINE vfloat vmul_vf_vf_vf(vfloat x, vfloat y) { return x * y; }
static INLINE vfloat vdiv_vf_vf_vf(vfloat x, vfloat y) { return x / y; }
static INLINE vfloat vrec_vf_vf   (vfloat x)           { return 1.0    / x; }

static INLINE vfloat vabs_vf_vf(vfloat x) { uint32_t tmp = *(uint32_t*) &x; uint32_t f = tmp & 0x7fffffff; return *(float*) &f; }
static INLINE vfloat vneg_vf_vf(vfloat x) { return -x; }
static INLINE vfloat vmla_vf_vf_vf_vf  (vfloat x, vfloat y, vfloat z) { return x * y + z; }
static INLINE vfloat vmlanp_vf_vf_vf_vf(vfloat x, vfloat y, vfloat z) { return x * y - z; }
static INLINE vfloat vmax_vf_vf_vf(vfloat x, vfloat y) { return x > y ? x : y; }
static INLINE vfloat vmin_vf_vf_vf(vfloat x, vfloat y) { return x < y ? x : y; }

static INLINE vfloat vposneg_vf_vf(vfloat x) { return (rv_lane() & 1) == 0 ?  x : -x; }
static INLINE vfloat vnegpos_vf_vf(vfloat x) { return (rv_lane() & 1) == 0 ? -x :  x; }
static INLINE vfloat vsubadd_vf_vf_vf(vfloat x, vfloat y) { return (rv_lane() & 1) == 0 ? x - y : x + y; }
static INLINE vfloat vmlsubadd_vf_vf_vf_vf(vfloat x, vfloat y, vfloat z) { return vsubadd_vf_vf_vf(vmul_vf_vf_vf(x, y), z); }

static INLINE vopmask veq_vo_vf_vf(vfloat x, vfloat y)  { return ((x == y) ? -1 : 0); }
static INLINE vopmask vneq_vo_vf_vf(vfloat x, vfloat y) { return ((x != y) ? -1 : 0); }
static INLINE vopmask vlt_vo_vf_vf(vfloat x, vfloat y)  { return ((x <  y) ? -1 : 0); }
static INLINE vopmask vle_vo_vf_vf(vfloat x, vfloat y)  { return ((x <= y) ? -1 : 0); }
static INLINE vopmask vgt_vo_vf_vf(vfloat x, vfloat y)  { return ((x >  y) ? -1 : 0); }
static INLINE vopmask vge_vo_vf_vf(vfloat x, vfloat y)  { return ((x >= y) ? -1 : 0); }

static INLINE vint vadd_vi2_vi2_vi2(vint x, vint y) { return x + y; }
static INLINE vint vsub_vi2_vi2_vi2(vint x, vint y) { return x - y; }
static INLINE vint vneg_vi2_vi2(vint x)             { return -x; }

static INLINE vint vand_vi2_vi2_vi2(vint x, vint y)    { return x &  y; }
static INLINE vint vandnot_vi2_vi2_vi2(vint x, vint y) { return y & ~x; }
static INLINE vint vor_vi2_vi2_vi2(vint x, vint y)     { return x |  y; }
static INLINE vint vxor_vi2_vi2_vi2(vint x, vint y)    { return x ^  y; }

static INLINE vfloat vsel_vf_vo_vf_vf(vopmask o, vfloat x, vfloat y) { return o ? x : y; }

static INLINE CONST vfloat vsel_vf_vo_f_f(vopmask o, float v1, float v0) {
  return vsel_vf_vo_vf_vf(o, vcast_vf_f(v1), vcast_vf_f(v0));
}

static INLINE vfloat vsel_vf_vo_vo_f_f_f(vopmask o0, vopmask o1, float d0, float d1, float d2) {
  return vsel_vf_vo_vf_vf(o0, vcast_vf_f(d0), vsel_vf_vo_f_f(o1, d1, d2));
}

static INLINE vfloat vsel_vf_vo_vo_vo_f_f_f_f(vopmask o0, vopmask o1, vopmask o2, float d0, float d1, float d2, float d3) {
  return vsel_vf_vo_vf_vf(o0, vcast_vf_f(d0), vsel_vf_vo_vf_vf(o1, vcast_vf_f(d1), vsel_vf_vo_f_f(o2, d2, d3)));
}

static INLINE vint2 vand_vi2_vo_vi2(vopmask x, vint2 y) { return x & y; }
static INLINE vint2 vandnot_vi2_vo_vi2(vopmask x, vint2 y) { return vandnot_vi2_vi2_vi2(x, y); }

// UNTIL HERE
static INLINE vint2 vsll_vi2_vi2_i(vint2 x, int c) { return x << c; }
static INLINE vint2 vsrl_vi2_vi2_i(vint2 x, int c) { return ((uint32_t) x) >> c; } // vint2 ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = ((uint32_t)x.i[i]) >> c; return ret; }
static INLINE vint2 vsra_vi2_vi2_i(vint2 x, int c) { return x >> c;; } // vint2 ret; for(int i=0;i<VECTLENSP;i++) ret.i[i] = x.i[i] >> c; return ret; }

static INLINE vopmask visinf_vo_vf (vfloat d) { return (d == SLEEF_INFINITYf || d == -SLEEF_INFINITYf) ? -1 : 0; }
static INLINE vopmask vispinf_vo_vf(vfloat d) { return d == SLEEF_INFINITYf ? -1 : 0; }
static INLINE vopmask visminf_vo_vf(vfloat d) { return d == -SLEEF_INFINITYf ? -1 : 0; }
static INLINE vopmask visnan_vo_vf (vfloat d) { return d != d ? -1 : 0; }

static INLINE vopmask veq_vo_vi2_vi2 (vint2 x, vint2 y) { return x == y ? -1 : 0; }
static INLINE vopmask vgt_vo_vi2_vi2 (vint2 x, vint2 y) { return x >  y ? -1 : 0; }
static INLINE vint2   veq_vi2_vi2_vi2(vint2 x, vint2 y) { return x == y ? -1 : 0; }
static INLINE vint2   vgt_vi2_vi2_vi2(vint2 x, vint2 y) { return x >  y ? -1 : 0; }

static INLINE vfloat vsqrt_vf_vf(vfloat x) { return sqrtf(x); }

#ifdef _MSC_VER
// This function is needed when debugging on MSVC.
static INLINE float vcast_f_vf(vfloat v) { return v; }
#endif

static INLINE vfloat vload_vf_p(const float *ptr) { return *(vfloat *)ptr; }
static INLINE vfloat vloadu_vf_p(const float *ptr) { return ptr[rv_lane()]; }

static INLINE void vstore_v_p_vf(float *ptr, vfloat v) { *(vfloat *)ptr = v; }
static INLINE void vstoreu_v_p_vf(float *ptr, vfloat v) { ptr[rv_lane()] = v; }

static INLINE void vstream_v_p_vf(float *ptr, vfloat v) { *(vfloat *)ptr = v; }

static INLINE void vscatter2_v_p_i_i_vf(float *ptr, int offset, int step, vfloat v) {
  ptr[rv_strided(offset, step)] = v;
}

static INLINE void vsscatter2_v_p_i_i_vf(float *ptr, int offset, int step, vfloat v) { vscatter2_v_p_i_i_vf(ptr, offset, step, v); }

//

static INLINE vlongdouble vcast_vl_l(long double d) { return d; }

extern vlongdouble vrev21_vl_vl(vlongdouble d0);
// {
//   abort();
// #if 0
//   vlongdouble r;
//   for(int i=0;i<VECTLENDP/2;i++) {
//     r.ld[i*2+0] = d0.ld[i*2+1];
//     r.ld[i*2+1] = d0.ld[i*2+0];
//   }
//   return r;
// #endif
// }

extern vlongdouble vreva2_vl_vl(vlongdouble d0);
// {
//   abort();
// #if 0
//   vlongdouble r;
//   for(int i=0;i<VECTLENDP/2;i++) {
//     r.ld[i*2+0] = d0.ld[(VECTLENDP/2-1-i)*2+0];
//     r.ld[i*2+1] = d0.ld[(VECTLENDP/2-1-i)*2+1];
//   }
//   return r;
// #endif
// }

static INLINE vlongdouble vadd_vl_vl_vl(vlongdouble x, vlongdouble y) { return x + y; }
static INLINE vlongdouble vsub_vl_vl_vl(vlongdouble x, vlongdouble y) { return x - y; }
static INLINE vlongdouble vmul_vl_vl_vl(vlongdouble x, vlongdouble y) { return x * y; }

static INLINE vlongdouble vneg_vl_vl(vlongdouble x) { return -x; }
static INLINE vlongdouble vsubadd_vl_vl_vl(vlongdouble x, vlongdouble y) { lane_t i = rv_lane(); return (i & 1) == 0 ? x - y : x + y; }
static INLINE vlongdouble vmlsubadd_vl_vl_vl_vl(vlongdouble x, vlongdouble y, vlongdouble z) { return vsubadd_vl_vl_vl(vmul_vl_vl_vl(x, y), z); }
static INLINE vlongdouble vposneg_vl_vl(vlongdouble x) { lane_t i = rv_lane(); return (i & 1) == 0 ?  x : -x; }
static INLINE vlongdouble vnegpos_vl_vl(vlongdouble x) { lane_t i = rv_lane(); return (i & 1) == 0 ? -x :  x; }

static INLINE vlongdouble vload_vl_p(const long double *ptr) { return *(vlongdouble *)ptr; }
static INLINE vlongdouble vloadu_vl_p(const long double *ptr) { return ptr[rv_lane()]; }

static INLINE void vstore_v_p_vl(long double *ptr, vlongdouble v) { *(vlongdouble *)ptr = v; }
static INLINE void vstoreu_v_p_vl(long double *ptr, vlongdouble v) {
  ptr[rv_lane()] = v;
}
static INLINE void vstream_v_p_vl(long double *ptr, vlongdouble v) { *(vlongdouble *)ptr = v; }

static INLINE void vscatter2_v_p_i_i_vl(long double *ptr, int offset, int step, vlongdouble v) {
  ptr[offset + rv_lane() * step] = v;
}

static INLINE void vsscatter2_v_p_i_i_vl(long double *ptr, int offset, int step, vlongdouble v) { vscatter2_v_p_i_i_vl(ptr, offset, step, v); }

#ifdef Sleef_quad2_DEFINED
static INLINE vquad vcast_vq_q(Sleef_quad d) { return d; }

extern vquad vrev21_vq_vq(vquad d0);
// {
//   abort();
// #if 0
//   vquad r;
//   for(int i=0;i<VECTLENDP/2;i++) {
//     r.q[i*2+0] = d0.q[i*2+1];
//     r.q[i*2+1] = d0.q[i*2+0];
//   }
//   return r;
// #endif
// }

extern vquad vreva2_vq_vq(vquad d0);
// {
//   abort();
// #if 0
//   vquad r;
//   for(int i=0;i<VECTLENDP/2;i++) {
//     r.q[i*2+0] = d0.q[(VECTLENDP/2-1-i)*2+0];
//     r.q[i*2+1] = d0.q[(VECTLENDP/2-1-i)*2+1];
//   }
//   return r;
// #endif
// }

static INLINE vquad vadd_vq_vq_vq(vquad x, vquad y) { return x + y; }
static INLINE vquad vsub_vq_vq_vq(vquad x, vquad y) { return x - y; }
static INLINE vquad vmul_vq_vq_vq(vquad x, vquad y) { return x * y; }

static INLINE vquad vneg_vq_vq(vquad x) { return -x; }
static INLINE vquad vsubadd_vq_vq_vq(vquad x, vquad y) { lane_t i = rv_lane(); return (i & 1) == 0 ? x - y : x + y; }
static INLINE vquad vmlsubadd_vq_vq_vq_vq(vquad x, vquad y, vquad z) { return vsubadd_vq_vq_vq(vmul_vq_vq_vq(x, y), z); }
static INLINE vquad vposneg_vq_vq(vquad x) { lane_t i = rv_lane(); return (i & 1) == 0 ?  x : -x; }
static INLINE vquad vnegpos_vq_vq(vquad x) { lane_t i = rv_lane(); return (i & 1) == 0 ? -x :  x; }

static INLINE vquad vload_vq_p(const Sleef_quad *ptr) {
  return ptr[rv_lane()];
}
static INLINE vquad vloadu_vq_p(const Sleef_quad *ptr) {
  return ptr[rv_lane()];
}

static INLINE void vstore_v_p_vq(Sleef_quad *ptr, vquad v) { *(vquad *)ptr = v; }
static INLINE void vstoreu_v_p_vq(Sleef_quad *ptr, vquad v) {
  ptr[rv_lane()] = v;
}
static INLINE void vstream_v_p_vq(Sleef_quad *ptr, vquad v) { *(vquad *)ptr = v; }

static INLINE void vscatter2_v_p_i_i_vq(Sleef_quad *ptr, int offset, int step, vquad v) {
  *ptr[offset + rv_lane() * step] = v;
#if 0
  for(int i=0;i<VECTLENDP/2;i++) {
    *(ptr+(offset + step * i)*2 + 0) = v.q[i*2+0];
    *(ptr+(offset + step * i)*2 + 1) = v.q[i*2+1];
  }
#endif
}

static INLINE void vsscatter2_v_p_i_i_vq(Sleef_quad *ptr, int offset, int step, vquad v) { vscatter2_v_p_i_i_vq(ptr, offset, step, v); }
#endif
