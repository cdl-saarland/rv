// Shapes: T_TrT, LaunchCode: foo2f8
// test_032_loopmx04(float a, float b)

extern "C" float
foo(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    if (x<y) {
        for (int i=0; i<10; ++i) {
            z += a;
            if (z > 53.12f) return a;
        }
    } else {
        z += a*a;
    }

    return z-b;
}

