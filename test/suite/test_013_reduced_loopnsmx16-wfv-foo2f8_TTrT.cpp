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
    float  r, x = a + b;
    float y =r= x * x - b;
    float z= fabs(a);

    for (float i=3.2f; i<bound(a) ; i=i+2000.0f) {

X:    for (int j=0; j<bound(b); j+=2000.0f) {
        z += y-i;
      }

      for (int k=0; k<bound(x); k += 5000) {
           z = z-3*a; // keep!
       }

       if (z < y) goto X;
    }

    return z;
}

