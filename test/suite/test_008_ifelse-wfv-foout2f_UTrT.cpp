#include <cmath>

extern "C" float
foo(float u, float t) {
  float r;
  if (u > 0) {
    r = sqrtf(t);
  } else {
    r = fabs(t);
  }
  return log(r);
}