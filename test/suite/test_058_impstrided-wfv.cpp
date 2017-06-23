// Shapes: C_U, LaunchCode: ivfoo

extern "C"
void
foo(int i, float *A) {
  // float a = A[2*i];
  A[2*i + 1] = 42.0f * i;
  //A[2*i+1] = a-b;
}
