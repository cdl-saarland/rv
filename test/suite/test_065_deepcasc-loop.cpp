// LoopHint: 0, LaunchCode: fooAn, Width: 4

#include <cmath>

extern "C" int
foo(int * A, int n) {
  for (int i = 0;  i < n; ++i) {
    double a = 0;
    if (i/6 % 5 != 0) {
      a -= sqrt(3*i*i*i);
    } else if (i/6 % 7 != 0) {
      a -= abs(i);
    } else if (i/6 % 9 != 0) {
      a += cos(i);
    } else if (i/6 % 13 != 0) {
      a -= sin(i);
    }
    A[i] = (int) a;
  }
  return 0;
}
