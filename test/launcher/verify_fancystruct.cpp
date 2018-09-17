#include <iostream>
#include "launcherTools.h"
#include <random>

struct Point { float x; float y; float z; };

struct T {
  Point a;
  Point b;
};

extern "C" float foo(int i, T * D);
extern "C" float8 foo_SIMD(int i, T * D);

int main(int argc, char ** argv) {
  const uint numInputs = 100;

  const int width = 8;

  std::mt19937 randSource(43);
  std::uniform_real_distribution<float> randGen;

  for (unsigned i = 0; i < numInputs; ++i) {
    T d[width];

    for (unsigned j = 0; j < width; ++j) {
      d[j].a.x = randGen(randSource);
      d[j].a.y = randGen(randSource);
      d[j].a.z = randGen(randSource);
      d[j].b.x = randGen(randSource);
      d[j].b.y = randGen(randSource);
      d[j].b.z = randGen(randSource);
    }

    float8 simdRes = foo_SIMD(0, d);

    float vecRes[8];
    toArray(simdRes, vecRes);

    bool error = false;
    for (int l = 0; l < width; ++l) {
      float expectedRes = foo(l, d);
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
