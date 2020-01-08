#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

extern "C" float8 foo_SIMD(float8 a, int8 b);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 200;

  for (unsigned i = 0; i < numVectors; ++i) {
    float a[8];
    int b[8];
    for (uint i = 0; i < vectorWidth; ++i) {
      a[i] = (float) i;
      b[i] = rand() % 5;
    }

    float8 res = foo_SIMD(*((float8*) &a), *((int8*) &b));

    float exp[8];
    for (uint i = 0, j = 0; i < vectorWidth; ++i) {
      exp[i] = a[i];
      if (b[i] != 0)
        exp[j++] = a[i];
    }
    bool broken = false;
    for (uint i = 0; i < vectorWidth; ++i) {
      if (exp[i] != res[i]) {
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
      dumpArray(exp, vectorWidth);
      return -1;
    }
  }

  return 0;
}
