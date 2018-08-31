// Shapes: U_C, LaunchCode: maskwfv, MaskPos: 1

// expected signature: foo_SIMD(float*A, <8 x i1> mask, int i)

extern "C" void
foo(float  * A, int i) {
  A[i] = 42.0f;
}

