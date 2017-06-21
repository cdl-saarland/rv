// LoopHint: 0, LaunchCode: fooABCn

#include <cmath>

extern "C" void
foo(int *a, int *b, int *c, int n)
{
  for ( int i = 0; i < n; i++) {
    switch (n) {
      case 0:
        a[i] = abs(b[i]);
        break;
      case 50:
        b[i] = a[i] + abs(c[i]);
        break;
      default:
        c[i] = abs(a[i]);
    }
  }
}
