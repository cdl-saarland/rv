// Shapes: C_U, LaunchCode: ivfoo
typedef float float4 __attribute__((ext_vector_type(4)));

extern "C"
void
foo(int i, float A[4]) {
  if (i == 4000) {
    A[0] = 42.0;
  }
}
