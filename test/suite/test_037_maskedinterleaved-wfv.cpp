// Shapes: C_U, LaunchCode: ivfoo

typedef float float4 __attribute__((ext_vector_type(4)));

extern "C"
void
foo(int i, float *A) {
  if (A[2*i] > A[2*i+1]) {
    float a = A[2*i];
    float b = A[2*i+1];
    A[2*i] = a+b;
    A[2*i+1] = a-b;
  }
}
