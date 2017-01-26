// Shapes: C_U, LaunchCode: ivfoo
typedef float float4 __attribute__((ext_vector_type(4)));

extern "C"
void
foo(int i, float A[4]) {
  A[i] = A[i] * 5 + 4;
}
