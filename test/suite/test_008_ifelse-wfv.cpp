// Shapes: U_TrT, LaunchCode: foout2f

#include <cmath>

extern "C" float
foo(float u, float t) {
  float r;
  if (u > 0) {
    r = sqrtf(t);
  } else {
    r = fabs(t);
  }
  return r*.5f;
}
