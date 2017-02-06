// Shapes: U_TrT, LaunchCode: foout2f

#include <cmath>

float Global;

float Inc(float v) __attribute__((noinline));

float Inc(float v) {
  Global += v;
  return v;
}

extern "C" float
foo(float u, float t) {
float a, b, c, d, e, f, g, v, R, z, p;

  float x = cosf(sqrtf(fabs(5000 * u - 4))); // scrambled u
  float y = sin(u*u); // scrambled u

  if (sinf(3*u) <= 0.75f) {
    goto A;
  } else {
    R = 3.0;
    goto end;
  }

A:
  Inc(1.0);
  if (sinf(t) > 0.0) goto B; else goto C;

B:
  Inc(2.0);
  t = t + 1.0;
  R = 1.0;
  goto end;

C:
  Inc(45436456.0f);
  R = 2.0;
  goto end;

end:
  Inc(8.0);
  return R;
}
