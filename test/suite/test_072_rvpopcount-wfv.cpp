// Shapes: T_TrU, LaunchCode: popcount
//

extern "C" int rv_popcount(bool i);

extern "C" int
foo(float a, float b)
{
    return rv_popcount(a < b);
}
