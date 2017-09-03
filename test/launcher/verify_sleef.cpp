/*
 * foo_launcher.cpp
 *
 *  Created on: Jul 22, 2015
 *      Author: Simon Moll
 */

#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>
#include <cmath>
#include <limits>

#include "launcherTools.h"

extern "C" float foo(int u, float t);
extern "C" float8 foo_SIMD(int u, float8 t);

template<typename TReal>
static bool isWithinPrecisionInterval(TReal a, TReal b, unsigned int interval_size = 1)
{
  if (!std::isfinite(a) && !std::isfinite(b)) return true;

    TReal min_a = a - (a - std::nextafter(a, std::numeric_limits<TReal>::lowest())) * interval_size;
    TReal max_a = a + (std::nextafter(a, std::numeric_limits<TReal>::max()) - a) * interval_size;

    return min_a <= b && max_a >= b;
}

static inline bool IsClose(float a, float b) {
  const unsigned interval_size = 2;
  return isWithinPrecisionInterval(a, b, interval_size);
}

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 100;

  for (unsigned i = 0; i < numVectors; ++i) {
    int a = i;
    float b[8];

    for (uint i = 0; i < vectorWidth; ++i) {
      b[i] = (float) rand();
    }

    float8 rVec = foo_SIMD(a, *((float8*) &b));
    float r[8];
    toArray(rVec, r);


    for (uint i = 0; i < vectorWidth; ++i) {
      float expectedRes = foo(a, b[i]);

      if (!IsClose(r[i], expectedRes)) {
        std::cerr << "MISMATCH!\n";
        std::cerr << i << " : a = " << a << " b = " << b[i] << " expected result (close to) " << expectedRes << " but was " << r[i] << "\n";
        dumpArray(b, vectorWidth); std::cerr << "\n";
        dumpArray(r, vectorWidth); std::cerr << "\n";
        return -1;
      }
    }
  }

  return 0;
}
