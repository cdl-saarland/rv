// Shapes: U_T_UrT, LaunchCode: load
//

extern "C" float rv_load(float*, int);

extern "C" float
foo(float* a, int b, int c)
{
    return rv_load(&a[b], c);
}
