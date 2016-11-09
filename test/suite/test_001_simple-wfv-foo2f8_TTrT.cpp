extern "C" float
foo(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    return x / y;
}
