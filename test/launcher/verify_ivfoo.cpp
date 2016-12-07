#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

// typedef float float4 __attribute__((ext_vector_type(4)));

extern "C" void foo(float u, float A[4]);
extern "C" void foo_SIMD(float u, float A[4]);

int main(int argc, char ** argv) {
  const uint vectorWidth = 4;
  const uint numVectors = 100;

  for (unsigned i = 0; i < numVectors; ++i) {

    float aScalar[vectorWidth];
    float aVec[vectorWidth];
    for (uint i = 0; i < vectorWidth; ++i) {
      aScalar[i] = (float) rand();
      aVec[i] = aScalar[i];
    }

    // invoke SIMD
    foo_SIMD(0, aVec);

    // invoke scalar
    for (int j = 0; j < vectorWidth; ++j) {
      foo(j, aScalar);
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
