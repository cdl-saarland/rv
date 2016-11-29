// test_072_loopnsmx16(float a, float b)
// in search for a simplified version

extern "C" float
foo(float a, float b)
{
    float z, r, x = a + b;
    float y =z=r= x * x - b;

    for (float i=3.2f; i<a ; i=i+2) {

X:    for (int j=0; j<b; ++j) {
        z += y-i;
      }

      for (int k=0; k<x; ++k) {
           z = z-3*a; // keep!
       }

       if (z < y) goto X;
    }

    return z;
}

