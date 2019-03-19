// Shapes: U_U_C, LaunchCode: dABi

#include <cmath>

extern "C" void
foo(double * A, long * B, int i)
{
   int n = 0;
   double a = A[i];
   while (a > 0.001) {
      a = log(a);
      ++n;
   }
   B[i] = n;
}
