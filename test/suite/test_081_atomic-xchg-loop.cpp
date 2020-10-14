// LoopHint: 0, LaunchCode: fooABCn

#include<stdatomic.h>

extern "C" int
foo(int * A, int * B, int * C, int n) {
  atomic_int a = 0;
  atomic_int b = 0;
  atomic_int c = 0;
  for (int i = 0; i < n; ++i) {
    A[i] = atomic_exchange(&a, B[i]);
    if (i % 2 == 0) {
      B[i] = atomic_exchange(&b, i);
      C[i] = atomic_exchange(&c, 1);
    }
  }
  return a + b + c;
}
