// LoopHint: 0, LaunchCode: fooABCn

#include<stdatomic.h>

extern "C" int
foo(int * A, int * B, int * C, int n) {
  atomic_int a = A[0];
  atomic_int b = B[0];
  atomic_int c = C[0];
  for (int i = 0; i < n; ++i) {
    A[i] = atomic_fetch_and(&a, B[i]);
    B[i] = atomic_fetch_or(&b, C[i]);
    C[i] = atomic_fetch_xor(&c, 8117);
  }
  return a + b + c;
}
