// LoopHint: 0, LaunchCode: fooABn

extern "C"
void
foo(float *A, float * B, int n) {
  for (int i = 0; i < n; ++i) {
    float x = B[i];
    if (x > 100.0f) {
      A[i] = x / n;
    }
  }
}
