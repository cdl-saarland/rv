// LoopHint: 0, LaunchCode: fooiA, Width: 4

#define ESCAPE 2
#define IMAGE_SIZE 8 * 800
#define START_X -2.0
#define START_Y START_X
#define MAX_ITER 10
#define step (-START_X - START_X)/IMAGE_SIZE

#define I 1.0iF

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__MINGW32__)
#include <complex.h>
#elif defined(__APPLE__)
#include <math.h>
#else
#include <tgmath.h>
#endif

#include <stdio.h>

static double __complex__
cmul(double __complex__ a, double __complex__ b) {
  double r = __real__ a * __real__ b - (__imag__ a * __imag__  b);
  double i = __real__ a * __imag__ b + __imag__ a * __real__ b;
  return r + r * I;
}

static int
colorMap(double __complex__ z) {
  return (int) ((__real__ z) + (__imag__ z)) % 256;
}

void foo(int * A) {
  int x, y, n;
  for (x = 0; x < IMAGE_SIZE; ++x) {
    for (y = 0; y < IMAGE_SIZE; ++y) {
      double __complex__ c = (START_X+x*step) + (START_Y-y*step) * I;
      double __complex__ z = 0.0;

      for (n = 0; n < MAX_ITER; ++n) {
        z = cmul(z, z) + c;
        if (hypot(__real__ z, __imag__ z) >= ESCAPE)
          break;
      }
      A[y * IMAGE_SIZE + x] = colorMap(z);
    }
  }
}
