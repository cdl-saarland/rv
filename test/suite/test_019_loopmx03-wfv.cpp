// Shapes: T_TrT, LaunchCode: foo2f8
// test_031_loopmx03(float a, float b)

extern "C" float
foo(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<100; ++i) {
        z += x;
        if (i > a) break;
    }

    z = z+x;
    z = y-z;

    return z;
}

