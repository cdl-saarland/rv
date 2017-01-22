// Shapes: UUrU, LaunchCode: unifoo2f

extern "C" bool rv_any(bool);

extern "C" float
foo(float a, float b)
{
  return rv_any(a > b) ? a : b;
}
