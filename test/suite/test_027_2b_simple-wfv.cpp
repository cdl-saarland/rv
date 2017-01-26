// Shapes: T_TrT, LaunchCode: foo2f8
typedef float float4 __attribute__((ext_vector_type(4)));

#include <cmath>

extern "C"
float
foo(float a, float b) {
  float x = fabs(a);
  float y = fabs(b);

  float z = a + b;

  while (x > 0) {
    x = x / 10.0f;
    z = z + 0.001;
    while (y > 0) {
      y = y / 42.0;
      // z = z - y;
      z = z - 1.0;
      if (z > 1000.0) {
        goto D; // breaks out of both loops
      }
    }
  }
D:
  return z;
}
