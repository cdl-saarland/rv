// Shapes: U_CrT, LaunchCode: si

struct S {
  float x;
  float y;
  float z;
  float w;
};

extern "C"
void
foo(S * A, int i) {
  A[i] = S{1.0f, 2.0f, 3.0f, 4.0f};
}
