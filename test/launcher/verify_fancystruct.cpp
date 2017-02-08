#include "launcherTools.h"

struct Point { float x; float y; float z; }

struct T {
  Point a;
  Point b;
}

extern "C" float foo(int i, T * D);
extern "C" float foo_SIMD(int i, T * D);

int main(int argc, char ** argv) {
  const uint numInputs = 100;

  const int width = 8;

  for (unsigned i = 0; i < numInputs; ++i) {
    T d[width];

    for (j = 0; j < sizeof(d); j += 4) {
      *((float*) d) = rand();
    }

    float8 simdRes = foo_SIMD(0, d);

    float vecRes[8];
    toArray<float, float8>(simdRes, vecRes);

    for (int l = 0; l < width; ++l) {
      float expectedRes = foo(l, d);
      if (vecRes[l] != expectedRes) {
        std::cerr << "MISMATCH!\n";
        std::cerr << l << " : retScalar = " << expectedRes << " retVec = " << vecRes[l] << "\n";
        return -1;
      }
    }
  }

  return 0;
}
