#ifndef RV_UTILS_MATHUTILS_H_
#define RV_UTILS_MATHUTILS_H_

#include <utility>
#include <strings.h>

template<typename N>
static N gcd(N a, N b) {
  if (a > b) std::swap(a, b);

  while (a) {
    N q = b % a;
    b = a;
    a = q;
  }

  return b;
}

template<typename N> inline
int highest_bit(N n);

template<> inline
int highest_bit(unsigned int n) {
  if (n == 0) return -1;
  return sizeof(n) * 8 - __builtin_clz(n) - 1;
}

template<typename N> inline
int lowest_bit(N n);

template<> inline
int lowest_bit(unsigned int n) {
  return ffs(n) - 1; // POSIX
}


#endif // RV_UTILS_MATHUTILS_H_
