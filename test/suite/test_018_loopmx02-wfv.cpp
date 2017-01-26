// Shapes: T_TrT, LaunchCode: foo2f8
// test_030_loopmx02(float a, float b)

extern "C" float
foo(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        if (z > 104.0f) break; //needs icmp + masking
        z += x; // 98/96/94/92 += 10
    }

    z = z-y;

    return z;
}

