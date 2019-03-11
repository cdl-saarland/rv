// Shapes: T_TrU, LaunchCode: popcount
#include <cstdint>

extern "C" int64_t rv_popcount(bool i);

extern "C" int64_t
foo(float a, float b)
{
    return rv_popcount(a < b);
}
