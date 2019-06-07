// Shapes: T_TrT, LaunchCode: compact
//

extern "C" float rv_compact(float, bool);

extern "C" float
foo(float a, int b)
{
    return rv_compact(a, b != 0);
}
