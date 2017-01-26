// Shapes: T_TrT, LaunchCode: foo2f8
// test_058_loopnsmx02(float a, float b)

extern "C" float
foo(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<100; ++i) {
        z++;
        for (int j=0; j<b; ++j) {
            z -= i;
            if (z < 0.f) return z;
        }
    }

    z = z-y;

    return z;
}

