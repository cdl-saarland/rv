// LoopHint: 0, LaunchCode: foo128n

struct Data {
  double v;
  long d;
};

extern "C"
void
foo(Data *A, int n) {
  for (int i = 0; i < n; ++i) {
    A[i].d = i;
  }
}
