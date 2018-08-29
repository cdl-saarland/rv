// LaunchCode: memcpy, LoopHint: 0
#include <cstring>

extern "C"
void
foo(double * A, double * B, int k, int m, int n) {
  for (int i = 0; i < n; ++i) {
    memcpy(B + k*i, A + k*i, m);
  }
}
