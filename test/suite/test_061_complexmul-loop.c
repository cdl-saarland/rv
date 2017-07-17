// LoopHint: 0, LaunchCode: foodAB, Width: 4

#include <complex.h>

void
foo(double * A, double * B, int n)
{
  for (int i = 0; i < n; ++i) {
    double complex z1 = B[i] + B[i+1] * I;
    double complex z2 = (2.0f* B[i+1]) - B[i+1] * I;
    double complex R = z1 * z2;
    A[i] = creal(R) + cimag(R);
   }
}
