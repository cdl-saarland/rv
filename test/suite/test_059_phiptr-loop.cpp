// LoopHint: 0, LaunchCode: fooABn

extern "C"
void
foo(float *A, float * B, int n) {
  for (int i = 0; i < n/2; ++i) {

    float * X = A;
    for (int j = 0; j < n; ++j) {
      if (B[j] < -2.0f) { X=B; }
      if (B[j] > 10.0f) break;
    }

    X[2 * i] = B[i];
    X[2 * i + 1] = B[i];
  }
}
