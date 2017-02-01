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
float a, b, c, d, e, f, g, v, R, z;
  if (sinf(u) == 0.4f) goto A; else goto O;
A:
  Inc(1.0);
  a = (-t);
  if (t > 1.0) goto B; else goto C; // div
B:
  Inc(2.0);
  t = t + 1.0;
  goto M;
C:
  Inc(45436456.0f);
  goto Q;
M:
  Inc(7.0);
  if (logf(u + 1.0f)) goto N; else goto O;
N:
  Inc(432543.0f);
  R = 42.0;
  if (logf(u + 3.0) > 1.0) goto P; else goto end;
O:
  Inc(435.0);
  goto P;
P:
  Inc(4546545.0);
  goto X;
Q:
  Inc(46545.0);
  goto Y;
X:
  Inc(42.0);
  z = 1.0;
  goto Z;
Y:
  Inc(42.0);
  z = 2.0;
  goto Z;
Z:
  R = z;
  Inc(7.0);
end:
  Inc(8.0);
  return R;
}
