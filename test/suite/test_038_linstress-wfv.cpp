// Shapes: U_TrT, LaunchCode: foout2f

#include <cmath>

float Inc(float v) __attribute__((noinline));

float Global;

float Inc(float v) {
  Global += v;
  return v;
}

extern "C" float
foo(float u, float t) {
float a, b, c, d, e, f, g, v;
  if (sinf(u) == 0.4f) goto A; else goto E;
A:
  Inc(1.0);
  a = (-t);
  if (t > 1.0) goto B; else goto C;
B:
  Inc(2.0);
  t = t + 1.0;
  if (u > 10.0) goto D; else goto E;
C:
  Inc(3.0);
  v = 2.0;
  if (logf(u) > 1.0) goto E; else goto X;
D:
  Inc(4.0);
  f = logf(fabs(t) + 1.0f);
  g = f;
  if (fabs(u) == 3.0) goto F; else goto G;
E:
  Inc(5.0);
  f = Inc(fabs(t));
  goto F;
F: // phi f
  Inc(6.0);
  v = 1.0;
  goto X;
X:
  Inc(7.0);
  g = fabs(v);
  goto G;
G: // phi g
  Inc(8.0);
  return g;
}
