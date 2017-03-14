// LoopHint: 0, LaunchCode: fooAn

extern "C" int
foo(int * A, int n) {
  int a = 0;
  for (int i = 0;  i < n; ++i) {
    a += A[i];
  }
  return a;
}
