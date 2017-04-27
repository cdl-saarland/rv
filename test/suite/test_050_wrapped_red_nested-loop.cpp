// LoopHint: 0, LaunchCode: fooAn

extern "C" int
foo(int * A, int n) {
  int a = 0;
  for (int i = 0; i < n; ++i) {
    int t = 0;
    for (int j = 0; j < n; j += 2) {
      t -= A[(i + j)];

    }
    a += t;
  }
  return a;
}
