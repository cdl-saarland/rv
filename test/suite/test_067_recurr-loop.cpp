// LoopHint: 0, LaunchCode: foopAn

extern "C" bool rv_any(bool) __attribute__((noinline));

extern "C" bool rv_any(bool m) {
  return m;
}

extern "C" int
foo(int * A, int n) {
  int r = n;
  for (int i = 0; i < n; ++i) {
    if (rv_any(A[i] >= 0)) {
      r = A[i*2 % n];
    }
  }
  return r;
}
