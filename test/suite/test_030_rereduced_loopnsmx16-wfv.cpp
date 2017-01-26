// Shapes: T_TrT, LaunchCode: foo2f8
// test_072_loopnsmx16(float a, float b)
// in search for a simplified version

#include <algorithm>
#include <cmath>

float bound(float v) {
  return std::min<float>(fabs(v), 400.0);
}

extern "C" float
foo(float a, float b)
{
    float x = a + b;
    float r= x * x - b;
    float y = fabs(10.f*r);
    float z= fabs(a);

    float i = y;
X:  for (int j=0; j<bound(b); j+=200) {
      z += y-i;
    }

    for (int k=0; k<bound(x); k += 50) {
      z = z + 2*(1.f + fabs(a)); // keep!
    }

    if (z < y) goto X;

    return z;
}

