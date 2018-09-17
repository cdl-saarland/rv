#ifndef LAUNCHERTOOLS_H_
#define LAUNCHERTOOLS_H_

#include <stdlib.h>
#include <cmath>
#include <limits>

template<typename D>
static D * allocateRandArray(int n) {
  auto * data = new D[n];
  for (uint i = 0; i < n; ++i) {
    data[i] = (D) rand();
  }
  return data;
}

template<typename D>
static size_t
hashArray(D * data, int n, size_t accu)  {
  for (uint i = 0; i < n; ++i) {
    accu = (101 * accu) ^ (size_t) data[i];
  }
  return accu;
}

typedef __attribute__((ext_vector_type(8))) float float8;
typedef __attribute__((ext_vector_type(8))) int int8;

static float
wfvRand() {
  #define CUSTOM_RAND_MAX 1000 //prevent too large inputs
  float r = (float)rand()/(float)RAND_MAX;
  float neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
  return (rand() % CUSTOM_RAND_MAX) * r * neg;
}

template<typename S, typename V>
static V toVec(S v[8]) {
  V r;
  r.s0 = v[0];
  r.s1 = v[1];
  r.s2 = v[2];
  r.s3 = v[3];
  r.s4 = v[4];
  r.s5 = v[5];
  r.s6 = v[6];
  r.s7 = v[7];
  return r;
}

template<typename S, typename V>
static V toVec4(S v[4]) {
  V r;
  r.s0 = v[0];
  r.s1 = v[1];
  r.s2 = v[2];
  r.s3 = v[3];
  return r;
}

template<typename S, typename V>
static void toArray(V v, S * r) {
  r[0] = v.s0;
  r[1] = v.s1;
  r[2] = v.s2;
  r[3] = v.s3;
  r[4] = v.s4;
  r[5] = v.s5;
  r[6] = v.s6;
  r[7] = v.s7;
}


template<typename S, typename V>
static void toArray4(V v, S * r) {
  r[0] = v.s0;
  r[1] = v.s1;
  r[2] = v.s2;
  r[3] = v.s3;
}
template<typename S>
static void dumpArray(S * A, uint n) {
  if (n == 0) return;
  std::cerr <<  "[" << A[0];
  for (uint i = 1; i < n; ++i) {
    std::cerr<< " " << A[i];
  }
  std::cerr << "]";
}

template<typename TReal>
static bool isWithinPrecisionInterval(TReal a, TReal b, unsigned int interval_size = 1)
{
  if (!std::isfinite(a) && !std::isfinite(b)) return true;

  if (a > b) return isWithinPrecisionInterval(b, a, interval_size);

  float h = a;
  for (int i = 0; i < (int) interval_size ; ++i) {
    h = std::nexttoward(h, b);
    if (b <= h) return true;
  }

  return b <= h;
}

static bool IsClose(float a, float b) {
  const unsigned interval_size = 3;
  return isWithinPrecisionInterval(a, b, interval_size);
}

#endif
