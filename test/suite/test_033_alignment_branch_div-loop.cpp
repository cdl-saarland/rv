// LoopHint: 0, LaunchCode: fooA
extern "C" void
foo(float* A) {
  for (unsigned i = 0; i < 2048; ++i) {
    if (i < 9) {
      A[i] = 42.0;
    }
  }
}
