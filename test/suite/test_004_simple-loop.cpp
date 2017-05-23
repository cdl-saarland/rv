// LoopHint: 0, LaunchCode: fooAn

extern "C" void
foo(float * A, int n)
{
  for (int i = 0; i < n; ++i) {
    A[i] = 42.0;
  }
}
