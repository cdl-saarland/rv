// Shapes: T_TrT, LaunchCode: foo2f8

struct Clunky {
  long long int large;
  float data;
};

Clunky f(float a, float b) __attribute__((noinline));
float g(Clunky c) __attribute__((noinline));

Clunky f(float a, float b)  {
  Clunky c;
  c.large = (long long int) a;
  c.data = b;
  return c;
}

float g(Clunky c)  {
  return c.data + (float) (c.large >> 1);
}

extern "C" float
foo(float a, float b)
{
  Clunky c = f(a, b);
  float v = g(c);
  return v;
}
