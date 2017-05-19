// LoopHint: 0, LaunchCode: fooA
extern "C" void
foo(float* A) {
  for (unsigned i = 0; i < 2048; ++i) {
    if (3 * i < 24) {
      A[i] = 42.0;
    }
  }
}
