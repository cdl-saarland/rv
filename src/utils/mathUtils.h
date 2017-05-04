#ifndef RV_UTILS_MATHUTILS_H_
#define RV_UTILS_MATHUTILS_H_



// query compiler / platform
#ifdef _MSC_VER
#define COMPILER_VSTUDIO
#endif




#include <utility>
#include <strings.h>

#ifdef COMPILER_VSTUDIO
#include <intrin.h>
#endif





template<typename N>
static N gcd(N a, N b) {
#if 0
  return std::gcd(a, b);
#else
  if (a > b) std::swap(a, b);

  while (a) {
    N q = b % a;
    b = a;
    a = q;
  }
#endif

  return b;
}


template<typename N> inline int highest_bit(N n);
template<typename N> inline int lowest_bit(N n);

#ifdef COMPILER_VSTUDIO
// VSTUDIO implementation

#pragma intrinsic(_BitScanReverse)
#pragma intrinsic(_BitScanForward)

template<> inline
int highest_bit(unsigned int n) {
  unsigned long mask = n;
  unsigned long index = 0;
  unsigned char isNonZero = _BitScanReverse(&index, mask);
  if (!isNonZero) return -1;
  else return sizeof(mask) * 8 - index - 1;
}

template<> inline
int lowest_bit(unsigned int n) {
  unsigned long mask = n;
  unsigned long index = 0;
  unsigned char isNonZero = _BitScanForward(&index, mask);
  if (!isNonZero) return -1;
  else return index - 1;
}


#else
// GCC / POSIX implementation

template<> inline
int highest_bit(unsigned int n) {
  // default to GCC
  if (n == 0) return -1;
  return sizeof(n) * 8 - __builtin_clz(n) - 1;
}

template<> inline
int lowest_bit(unsigned int n) {
  // default to POSIX
  return ffs(n) - 1;
}
#endif


#endif // RV_UTILS_MATHUTILS_H_
