#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

extern "C" float8 foo_SIMD(float * a, int8 b, int c);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 200;

  for (unsigned i = 0; i < numVectors; ++i) {
    float a[8];
    int b[8];
    for (uint i = 0; i < vectorWidth; ++i) {
      a[i] = (float) i;
      b[i] = i;
    }

    int c = 3;

    float8 res = foo_SIMD(a, *((int8*) &b), c);

    float exp = a[c];
    bool broken = false;
    for (uint i = 0; i < vectorWidth; ++i) {
      if (exp != res[i]) {
        std::cerr << "MISMATCH!\n";
        broken = true;
        break;
      }
    }
    if (broken) {
      std::cerr << "-- vectors --\n";
      dumpArray(a, vectorWidth); std::cerr << "\n";
      dumpArray(b, vectorWidth); std::cerr << "\n";

      std::cerr << "-- result --\n";
      float resArray[8];
      toArray<float, float8>(res, resArray);
      dumpArray(resArray, vectorWidth);
      std::cerr << "\n";

      std::cerr << "-- expected --\n";
      std::cerr << exp << "\n";
      return -1;
    }
  }

  return 0;
}
