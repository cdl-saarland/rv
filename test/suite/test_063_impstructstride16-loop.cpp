// LoopHint: 0, LaunchCode: foo128n

struct Data {
  float _Complex c; // 8 byte
  unsigned long long d; // 8 byte
}; //

extern "C"
void
foo(Data *A, int n) {
  for (int i = 0; i < n; ++i) {
    A[i].d = i;
  }
}
