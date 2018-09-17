#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>
#include <random>

#include "launcherTools.h"

// typedef float float4 __attribute__((ext_vector_type(4)));

extern "C" void foo(int u, float * A);
extern "C" void foo_SIMD(int u, float * A);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 100;

  std::mt19937 randSource(42);
  std::uniform_real_distribution<float> randGen;

  for (unsigned i = 0; i < numVectors; ++i) {

    const uint padded = 2 * vectorWidth;
    float aScalar[padded];
    float aVec[padded];
    for (uint i = 0; i < padded; ++i) {
      aScalar[i] = randGen(randSource);
      aVec[i] = aScalar[i];
    }

    // invoke SIMD
    foo_SIMD(0, aVec);

    // invoke scalar
    for (int j = 0; j < vectorWidth; ++j) {
      foo(j, aScalar);
    }

    for (uint i = 0; i < padded; ++i) {

      if (aScalar[i] != aVec[i]) {
        std::cerr << "MISMATCH!\n";
        std::cerr << i << " : s[i] = " << aScalar[i] << " v[i] = " << aVec[i] << "\n";
        dumpArray(aScalar, padded); std::cerr << "\n";
        dumpArray(aVec, padded); std::cerr << "\n";
        return -1;
      }
    }
  }

  return 0;
}
