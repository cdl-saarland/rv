extern "C" void
foo(double * A, double * B, int n) {
  #pragma clang loop vectorize(enable) vectorize_width(8)
  for (int i = 0; i < 8*n; ++i) {
    A[i] = B[i] * 42;
  }
}
