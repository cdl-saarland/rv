#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

extern "C" int foo_SIMD(float8 a, float8 b);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 200;

  for (unsigned i = 0; i < numVectors; ++i) {
    float a[8];
    float b[8];
    for (uint i = 0; i < vectorWidth; ++i) {
      a[i] = (float) 42.0f;
      b[i] = (float) wfvRand();
    }

    int mask = foo_SIMD(*((float8*) &a), *((float8*) &b));

    bool broken = false;
    broken |= (mask & ~0xFF) != 0;
    for (uint i = 0; i < vectorWidth; ++i) {
      int bit = (mask >> i) & 1;
      int expectedRes = a[i] < b[i];
      if (expectedRes != bit) {
        std::cerr << "MISMATCH!\n";
        std::cerr << i << " : a = " << a[i] << " b = " << b[i] << " expected result " << expectedRes << " but was " << bit << "\n";
        broken = true;
      }
    }
    if (broken) {
        std::cerr << "-- vectors --\n";
        dumpArray(a, vectorWidth); std::cerr << "\n";
        dumpArray(b, vectorWidth); std::cerr << "\n";
        std::cerr << "-- result --\n";
        std::cerr << std::hex << mask;
      return -1;
    }
  }

  return 0;
}
