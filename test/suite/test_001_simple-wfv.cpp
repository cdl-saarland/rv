// Shapes: T_TrT, LaunchCode: foo2f8

extern "C" float
foo(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    return x / y;
}
