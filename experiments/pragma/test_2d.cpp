extern "C" void
foo(double * A, double * B, int n) {
  #pragma clang loop vectorize(enable) vectorize_width(8)
  for (int i = 0; i < 8*n; ++i) {
    for (int j = 0; j < n; ++j) {
      A[i] += B[j] * 42;
    }
  }
}
