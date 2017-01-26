// Shapes: C_UrT, LaunchCode: catfoo
extern "C"
float
foo(int i, float * data) {
  auto * p = data + 48; // uni GEP
  float a = *p;
  float b = *((p + (i*i % 32))); // divergent GEP
  return a+b;
}
