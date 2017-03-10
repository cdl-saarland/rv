// Shapes: T_TrU, LaunchCode: ballot
//

extern "C" int rv_ballot(bool i);

extern "C" int
foo(float a, float b)
{
    return rv_ballot(a < b);
}
