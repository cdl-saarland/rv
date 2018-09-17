#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"
#include <random>

// typedef float float4 __attribute__((ext_vector_type(4)));

extern "C" float foo(int c, float * A);
extern "C" float8 foo_SIMD(int c, float * A);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numIters = 1;

  std::mt19937 randSource(42);
  std::uniform_real_distribution<float> randGen;

  float data[2048];

  for (unsigned b = 0; b < numIters; ++b) {

    for (int j = 0; j < 2048; ++j) {
      data[j] = randGen(randSource);
    }

    // invoke SIMD
    auto res = foo_SIMD(b * vectorWidth, data);

    float aVec[vectorWidth];
    toArray<float, float8>(res, aVec);

    // invoke scalar
    float aScalar[vectorWidth];
    for (int j = 0; j < vectorWidth; ++j) {
      aScalar[j] = foo(b*vectorWidth + j, data);
    }

    for (uint i = 0; i < vectorWidth; ++i) {
      if (aScalar[i] != aVec[i]) {
        std::cerr << "MISMATCH!\n";
        std::cerr << i << " : s[i] = " << aScalar[i] << " v[i] = " << aVec[i] << "\n";
        dumpArray(aScalar, vectorWidth); std::cerr << "\n";
        dumpArray(aVec, vectorWidth); std::cerr << "\n";
        return -1;
      }
    }
  }

  return 0;
}
