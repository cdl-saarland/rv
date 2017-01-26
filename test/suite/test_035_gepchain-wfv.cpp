// Shapes: C_UrT, LaunchCode: catfoo
extern "C"
float
foo(int i, float * data) {
  auto * p = data + i;
  float a = *p;
  float b = *((p + (i*i % 32)));
  return a+b;
}
