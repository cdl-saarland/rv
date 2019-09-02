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

  if (sinf(3*u) <= 0.75f) goto A;
  else if (sinf(59*u) <= .5) goto E; else goto K;
// entry -> {A, E, K}

A:
  Inc(1.0);
  a = (-t);
  if (y > 0.0) goto B; else goto H;

B:
  Inc(2.0);
  t = t + 1.0;
  goto C;
C:
  Inc(45436456.0f);
  if (cosf(t) <= 0.0) goto D; else goto E; // div
D:
  Inc(454366.0f);
  if (cosf(u) <= .25) {
    p = 4.1;
    goto F; }
  else {
    R = 4.3;
    goto end;
  }
E:
  Inc(4366.0f);
  p = 4.2;
  goto F;
F: /// phi
  v = p;
  Inc(2342345.f);
  goto G;
G:
  Inc(23452345.f);
  goto Z;

H:
  Inc(98123.f);
  goto I;
I:
  Inc(23425.f);
  if (cosf(x) <= .75) goto L; else goto K;
K:
  Inc(134545.f);
  goto M;
L:
  Inc(23234.f);
  R = 3.0;
  if (cosf(x) <= .25 ) goto M; else goto end;
M:
  Inc(324234.f);
  goto N;
N:
  v = 2.0;
  Inc(4545.f);
  goto Z;

Z:
  R = v;  // phi
  Inc(7.f);

end:
  Inc(8.f);
  return R;
}
