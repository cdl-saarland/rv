// Shapes: T_TrT, LaunchCode: foo2f8
// test_071_loopnsmx15(float, float)

extern "C" float
foo(float a, float b)
{
    float z, r, x = a + b;
    float y=z=r= x * x - b;
    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
X:    for (int j=0; j<b; ++j) {
        if (z > 120.5123f) goto Z;
        z += y-i;
      }
      // if (x<=y) {
            // if (z != y) r = x-a;
            // if (z < y) goto X;
            // else return b;
        // } else {
            z = y-a;
            // r = y+a;
       }
Z: // z -= r;
    return z;
}

