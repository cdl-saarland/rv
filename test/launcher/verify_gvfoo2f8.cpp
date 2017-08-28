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

#include "launcherTools.h"

extern "C" float foo(float a, float b, int);
extern "C" float8 foo_SIMD(float8 a, float8 b, int);

extern "C" float shared_Global;

float sharedGlobal[8] = {1.0, -5.0, 6.0, -134.45, 443.4, 454.1, 99999.0, 2143.0};

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 200;

  for (unsigned i = 0; i < numVectors; ++i) {
    float a[8];
    float b[8];
    for (uint i = 0; i < vectorWidth; ++i) {
      a[i] = (float) wfvRand();
      b[i] = (float) wfvRand();
    }

    float8 rVec = foo_SIMD(*((float8*) &a), *((float8*) &b), 0);
    float r[8];
    toArray(rVec, r);

    bool broken = false;
    for (uint i = 0; i < vectorWidth; ++i) {
      float expectedRes = foo(a[i], b[i], i);
      if (r[i] != expectedRes) {
        std::cerr << "MISMATCH!\n";
        std::cerr << i << " : a = " << a[i] << " b = " << b[i] << " expected result " << expectedRes << " but was " << r[i] << "\n";
        broken = true;
      }
    }
    if (broken) {
        std::cerr << "-- vectors --\n";
        dumpArray(a, vectorWidth); std::cerr << "\n";
        dumpArray(b, vectorWidth); std::cerr << "\n";
        dumpArray(r, vectorWidth); std::cerr << "\n";
      return -1;
    }
  }

  return 0;
}
