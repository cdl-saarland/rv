// Shapes: T_TrT, LaunchCode: foo2f8const
//

extern "C" float rv_extract(float v, int i);

extern "C" float
foo(float a, float b)
{
    float x = rv_extract(a, 3) + b;
    float y = x * x - b;
    return x / y;
}
