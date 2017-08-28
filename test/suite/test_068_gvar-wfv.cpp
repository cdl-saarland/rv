// Shapes: T_T_UrT, LaunchCode: gvfoo2f8, VarShape[sharedGlobal]: S4

extern "C" {

extern float sharedGlobal[8];

float
foo(float a, float b, int i) {
  return sharedGlobal[i] + a + b;
}

}
