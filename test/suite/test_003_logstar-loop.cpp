// LoopHint: 0, LaunchCode: fooAiB

#include <cmath>

extern "C" void
foo(float * A, int * B, int n)
{
  for (int i = 0; i < n; ++i) {
    float a = A[i];
    int s = 0;
     while (a > 0.001) {
        a = log(a);
        ++s;
     }
     B[i] = s;
   }
}
