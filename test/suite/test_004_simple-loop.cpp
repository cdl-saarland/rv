// LoopHint: 0, LaunchCode: fooA

extern "C" void
foo(int n, float * A)
{
  for (int i = 0; i < n; ++i) {
    A[i] = 42.0;
  }
}
