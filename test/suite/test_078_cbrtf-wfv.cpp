// Shapes: TrT, LaunchCode: foo2f8

#include <stdint.h>
#include <cmath>
#include <limits>

#define INLINE inline
#define CONST

#define INT_MAX std::numeric_limits<int>::max()

#define SLEEF_NAN __builtin_nan("")
#define SLEEF_NANf __builtin_nanf("")
#define SLEEF_NANl __builtin_nanl("")
#define SLEEF_INFINITY __builtin_inf()
#define SLEEF_INFINITYf __builtin_inff()
#define SLEEF_INFINITYl __builtin_infl()

#ifndef SLEEF_FP_ILOGB0
#define SLEEF_FP_ILOGB0 ((int)-2147483648)
#endif

#ifndef SLEEF_FP_ILOGBNAN
#define SLEEF_FP_ILOGBNAN ((int)2147483647)
#endif

#define SLEEF_SNAN (((union { long long int i; double d; }) { .i = 0x7ff0000000000001LL }).d)
#define SLEEF_SNANf (((union { long int i; float f; }) { .i = 0xff800001 }).f)

typedef struct {
  float x, y;
} Sleef_float2;

static INLINE CONST int32_t floatToRawIntBits(float d) {
  union {
    float f;
    int32_t i;
  } tmp;
  tmp.f = d;
  return tmp.i;
}

static INLINE CONST float intBitsToFloat(int32_t i) {
  union {
    float f;
    int32_t i;
  } tmp;
  tmp.i = i;
  return tmp.f;
}

static INLINE CONST float fabsfk(float x) {
  return intBitsToFloat(0x7fffffffL & floatToRawIntBits(x));
}

static INLINE CONST float mulsignf(float x, float y) {
  return intBitsToFloat(floatToRawIntBits(x) ^ (floatToRawIntBits(y) & (1 << 31)));
}

static INLINE CONST double copysignfk(double x, double y) {
  return intBitsToFloat((floatToRawIntBits(x) & ~(1 << 31)) ^ (floatToRawIntBits(y) & (1 << 31)));
}

static INLINE CONST float signf(float d) { return mulsignf(1, d); }
static INLINE CONST float mlaf(float x, float y, float z) { return x * y + z; }
static INLINE CONST float rintfk(float x) { return x < 0 ? (int)(x - 0.5f) : (int)(x + 0.5f); }
static INLINE CONST int ceilfk(float x) { return (int)x + (x < 0 ? 0 : 1); }
static INLINE CONST float fminfk(float x, float y) { return x < y ? x : y; }
static INLINE CONST float fmaxfk(float x, float y) { return x > y ? x : y; }
static INLINE CONST int xisintf(float x) { return (x == (int)x); }

static INLINE CONST int xisnanf(float x) { return x != x; }
static INLINE CONST int xisinff(float x) { return x == SLEEF_INFINITYf || x == -SLEEF_INFINITYf; }
static INLINE CONST int xisminff(float x) { return x == -SLEEF_INFINITYf; }
static INLINE CONST int xispinff(float x) { return x == SLEEF_INFINITYf; }
static INLINE CONST int xisnegzerof(float x) { return floatToRawIntBits(x) == floatToRawIntBits(-0.0); }
static INLINE CONST int xisnumberf(double x) { return !xisinff(x) && !xisnanf(x); }

static INLINE CONST int ilogbkf(float d) {
  int m = d < 5.421010862427522E-20f;
  d = m ? 1.8446744073709552E19f * d : d;
  int q = (floatToRawIntBits(d) >> 23) & 0xff;
  q = m ? q - (64 + 0x7f) : q - 0x7f;
  return q;
}

static INLINE CONST float upperf(float d) {
  return intBitsToFloat(floatToRawIntBits(d) & 0xfffff000);
}

static INLINE CONST Sleef_float2 df(float h, float l) {
  Sleef_float2 ret;
  ret.x = h; ret.y = l;
  return ret;
}

static INLINE CONST Sleef_float2 dfx(double d) {
  Sleef_float2 ret;
  ret.x = d; ret.y = d - ret.x;
  return ret;
}

// vilogb2kf is similar to ilogbkf, but the argument has to be a
// normalized FP value.
static INLINE CONST int ilogb2kf(float d) {
  return ((floatToRawIntBits(d) >> 23) & 0xff) - 0x7f;
}

static INLINE CONST Sleef_float2 dfmul_f2_f_f(float x, float y) {
  float xh = upperf(x), xl = x - xh;
  float yh = upperf(y), yl = y - yh;
  Sleef_float2 r;

  r.x = x * y;
  r.y = xh * yh - r.x + xl * yh + xh * yl + xl * yl;

  return r;
}

static INLINE CONST Sleef_float2 dfmul_f2_f2_f(Sleef_float2 x, float y) {
  float xh = upperf(x.x), xl = x.x - xh;
  float yh = upperf(y  ), yl = y   - yh;
  Sleef_float2 r;

  r.x = x.x * y;
  r.y = xh * yh - r.x + xl * yh + xh * yl + xl * yl + x.y * y;

  return r;
}

static INLINE CONST Sleef_float2 dfmul_f2_f2_f2(Sleef_float2 x, Sleef_float2 y) {
  float xh = upperf(x.x), xl = x.x - xh;
  float yh = upperf(y.x), yl = y.x - yh;
  Sleef_float2 r;

  r.x = x.x * y.x;
  r.y = xh * yh - r.x + xl * yh + xh * yl + xl * yl + x.x * y.y + x.y * y.x;

  return r;
}

CONST int xilogbf(float d) {
  int e = ilogbkf(fabsfk(d));
  e = d == 0.0f  ? SLEEF_FP_ILOGB0 : e;
  e = xisnanf(d) ? SLEEF_FP_ILOGBNAN : e;
  e = xisinff(d) ? INT_MAX : e;
  return e;
}

static INLINE CONST float pow2if(int q) {
  return intBitsToFloat(((int32_t)(q + 0x7f)) << 23);
}

static INLINE CONST float ldexpkf(float x, int q) {
  float u;
  int m;
  m = q >> 31;
  m = (((m + q) >> 6) - m) << 4;
  q = q - (m << 2);
  m += 127;
  m = m <   0 ?   0 : m;
  m = m > 255 ? 255 : m;
  u = intBitsToFloat(((int32_t)m) << 23);
  x = x * u * u * u * u;
  u = intBitsToFloat(((int32_t)(q + 0x7f)) << 23);
  return x * u;
}

static INLINE CONST float ldexp2kf(float d, int e) { // faster than ldexpkf, short reach
  return d * pow2if(e >> 1) * pow2if(e - (e >> 1));
}

static INLINE CONST Sleef_float2 dfadd2_f2_f2_f(Sleef_float2 x, float y) {
  // |x| >= |y|

  Sleef_float2 r;

  r.x  = x.x + y;
  float v = r.x - x.x;
  r.y = (x.x - (r.x - v)) + (y - v);
  r.y += x.y;

  return r;
}

static INLINE CONST Sleef_float2 dfadd2_f2_f2_f2(Sleef_float2 x, Sleef_float2 y) {
  Sleef_float2 r;

  r.x  = x.x + y.x;
  float v = r.x - x.x;
  r.y = (x.x - (r.x - v)) + (y.x - v);
  r.y += x.y + y.y;

  return r;
}




// SLEEF function: xcbrtf_u1
extern "C" float foo(float d, float DUMMY) {
  float x, y, z;
  Sleef_float2 q2 = df(1, 0), u, v;
  int e, r;

  e = ilogbkf(fabsfk(d))+1;
  d = ldexp2kf(d, -e);
  r = (e + 6144) % 3;
  q2 = (r == 1) ? df(1.2599210739135742188, -2.4018701694217270415e-08) : q2;
  q2 = (r == 2) ? df(1.5874010324478149414,  1.9520385308169352356e-08) : q2;

  q2.x = mulsignf(q2.x, d); q2.y = mulsignf(q2.y, d);
  d = fabsfk(d);

  x = -0.601564466953277587890625f;
  x = mlaf(x, d, 2.8208892345428466796875f);
  x = mlaf(x, d, -5.532182216644287109375f);
  x = mlaf(x, d, 5.898262500762939453125f);
  x = mlaf(x, d, -3.8095417022705078125f);
  x = mlaf(x, d, 2.2241256237030029296875f);

  y = x * x; y = y * y; x -= (d * y - x) * (1.0 / 3.0f);

  z = x;

  u = dfmul_f2_f_f(x, x);
  u = dfmul_f2_f2_f2(u, u);
  u = dfmul_f2_f2_f(u, d);
  u = dfadd2_f2_f2_f(u, -x);
  y = u.x + u.y;

  y = -2.0 / 3.0 * y * z;
  v = dfadd2_f2_f2_f(dfmul_f2_f_f(z, z), y);
  v = dfmul_f2_f2_f(v, d);
  v = dfmul_f2_f2_f2(v, q2);
  z = ldexp2kf(v.x + v.y, (e + 6144) / 3 - 2048);

  if (xisinff(d)) { z = mulsignf(SLEEF_INFINITYf, q2.x); }
  if (d == 0) { z = mulsignf(0, q2.x); }

  return z;
}
