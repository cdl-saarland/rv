#ifndef RV_UTILS_MATHUTILS_H_
#define RV_UTILS_MATHUTILS_H_

#include <utility>

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

#endif // RV_UTILS_MATHUTILS_H_
