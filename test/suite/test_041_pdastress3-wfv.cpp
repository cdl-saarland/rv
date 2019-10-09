// Shapes: U_TrT, LaunchCode: foout2f

#include <cmath>

int Global;

int Inc(int v) __attribute__((noinline));

int Inc(int v) {
  Global |= 1 << (v-1);
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
  Inc(1);
  a = (-t);
  if (y > 0.0) goto B; else goto H;

B:
  Inc(2);
  t = t + 1.0;
  goto C;
C:
  Inc(3);
  if (cosf(t) <= 0.0) goto D; else goto E; // div
D:
  Inc(4);
  if (cosf(u) <= .25) {
    p = 4.1;
    goto F;
  } else {
    R = 4.0;
    goto end;
  }
E:
  Inc(5);
  p = 4.2;
  goto F;
F: /// phi
  v = p;
  Inc(6);
  goto G;
G:
  Inc(7);
  goto Z;

H:
  Inc(8);
  goto I;
I:
  Inc(9);
  if (cosf(x) <= .75) goto L; else goto K;
K:
  Inc(10);
  goto M;
L:
  Inc(11);
  R = 3.0;
  if (cosf(x) <= .25 ) goto M; else goto end;
M:
  Inc(12);
  goto N;
N:
  v = 2.0;
  Inc(13);
  goto Z;

Z:
  R = v;  // phi
  Inc(14);

end:
  Inc(15);
  return R;
}
