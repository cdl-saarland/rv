// Shapes: T_TrT, LaunchCode: foo2f8
// test_072_loopnsmx16(float a, float b)

extern "C" float
foo(float a, float b)
{
    float z, r, x = a + b;
    float y =z=r= x * x - b;
    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
X:    for (int j=0; j<b; ++j) {
        if (z > 120.5123f) goto Y;
        z += y-i;
      }
      if (x>y) {
            z = a+x;
            r = x*x;
            continue;
      } else if (y>x) {
Y:         for (int k=0; k<x; ++k) {
                z = z-3*a;
                if (a > 120.5123f) goto Z;
                else if (z == b*b) continue;
                z -= b*3.32f;
            }
            if (z != y) r = x-a;
            else if (z < y) goto X;
            else return b;
        } else {
            z = y-a;
            r = y+a;
        }
    }
Z: z -= r;
    return z;
}

