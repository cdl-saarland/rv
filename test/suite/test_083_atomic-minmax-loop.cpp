// LoopHint: 0, LaunchCode: fooABCn

#include<stdatomic.h>

extern "C" int
foo(int * A, int * B, unsigned int * C, int n) {
  int a = 0;
  int b = 0;
  unsigned int c = 0;
  for (int i = 0; i < n; ++i) {
    A[i] = __atomic_fetch_min(&a, B[i], __ATOMIC_SEQ_CST);
    B[i] = __atomic_fetch_max(&b, 1, __ATOMIC_SEQ_CST);
    C[i] = __atomic_fetch_max(&c, C[i], __ATOMIC_SEQ_CST);
  }
  return a + b + c;
}
