// LoopHint: 0, LaunchCode: fooABCn

#include<stdatomic.h>

extern "C" int
foo(int * A, int * B, int * C, int n) {
  atomic_int a = 0;
  atomic_int b = 0;
  atomic_int c = 0;
  for (int i = 0; i < n; ++i) {
    A[i] = atomic_fetch_add(&a, B[i]);
    B[i] = atomic_fetch_add(&b, i);
    C[i] = atomic_fetch_add(&c, 1);
  }
  return a + b + c;
}
