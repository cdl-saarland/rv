// Shapes: T_TrT, LaunchCode: ifoo2f8

#include <cmath>

extern "C" int
foo(double a, double b)
{
   int n = 0;
   while (a > 0.001) {
      a = log(a);
      ++n;
   }
   return n;
}
