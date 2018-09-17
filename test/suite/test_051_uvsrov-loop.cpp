// LoopHint: 0, LaunchCode: fooABCn
//

struct TexCoord { float u; float v; };

extern "C" void
foo(TexCoord * A, float * B, float * C, int n) {
  for (int i = 0; i < n; ++i) {
    TexCoord uv{B[0], 1.0f};
    for (int j = 0; j < n; ++j) {
      if (A[((j + 3*i) % n)].v > .5f) {
        uv = A[i];
      }
    }
    TexCoord o = (*B == B[42]) ? uv : TexCoord{400.0f, 200.0f};
    C[2*i] = o.u;
    C[2*i + 1] = o.v;
  }
}
