#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>
#include <random>

#include "launcherTools.h"

extern "C" int foo_SIMD(float8 a, float8 b);

int main(int argc, char ** argv) {
  const size_t vectorWidth = 8;
  const size_t numVectors = 200;

  std::mt19937 randGen(42);
  std::uniform_real_distribution<float> uniDist;

  for (unsigned i = 0; i < numVectors; ++i) {
    float a[8];
    float b[8];
    for (uint i = 0; i < vectorWidth; ++i) {
      a[i] = (float) 42.0f;
      b[i] = (float) uniDist(randGen);
    }

    uint numActive = foo_SIMD(*((float8*) &a), *((float8*) &b));

    bool broken = false;
    broken |= numActive > vectorWidth;
    uint scaCount = 0;
    for (size_t i = 0; i < vectorWidth; ++i) {
      scaCount += a[i] < b[i];
    }

    if (scaCount != numActive) {
      std::cerr << "Expected " << scaCount << ", was " << numActive << "\n";
      std::cerr << "MISMATCH!\n";
      broken = true;
    }

    if (broken) {
        std::cerr << "-- vectors --\n";
        dumpArray(a, vectorWidth); std::cerr << "\n";
        dumpArray(b, vectorWidth); std::cerr << "\n";
        std::cerr << "-- result --\n";
        std::cerr << numActive;
      return -1;
    }
  }

  return 0;
}
