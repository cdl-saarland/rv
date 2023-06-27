// Shapes: C_U_UrT, LaunchCode: fancystructDual

struct T {
  float a [7];
  float b;
};

extern "C" float
foo(int i, T * A, T * B) {

  T a = A[i];
  B[i] = a;

  return B[i].b;
}
