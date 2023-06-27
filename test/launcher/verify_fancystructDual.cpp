#include <iostream>
#include "launcherTools.h"
#include <random>

struct T {
  float a [7];
  float b;
};

extern "C" float foo(int i, T * A, T * B);
extern "C" float8 foo_SIMD(int i, T * A, T * B);

int main(int argc, char ** argv) {
  const uint numInputs = 100;

  const int width = 8;

  std::mt19937 randSource(43);
  std::uniform_real_distribution<float> randGen;

  for (unsigned i = 0; i < numInputs; ++i) {
    T a[width];
    T b[width];

    for (unsigned j = 0; j < width; ++j) {
      a[j].a[0] = randGen(randSource);
      a[j].a[1] = randGen(randSource);
      a[j].a[2] = randGen(randSource);
      a[j].a[3] = randGen(randSource);
      a[j].a[4] = randGen(randSource);
      a[j].a[5] = randGen(randSource);
      a[j].a[6] = randGen(randSource);

      a[j].b = randGen(randSource);
    }

    float8 simdRes = foo_SIMD(0, a, b);

    float vecRes[8];
    toArray(simdRes, vecRes);

    bool error = false;
    for (int l = 0; l < width; ++l) {
      float expectedRes = foo(l, a, b);
      if (vecRes[l] != expectedRes) {
        std::cerr << "MISMATCH!\n";
        std::cerr << l << " : retScalar = " << expectedRes << " retVec = " << vecRes[l] << "\n";
        error = true;
      }
    }

    if (error) return -1;
  }

  return 0;
}
