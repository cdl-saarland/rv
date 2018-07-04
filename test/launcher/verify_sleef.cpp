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

#include <random>

#include "launcherTools.h"

extern "C" float foo(int u, float t);
extern "C" float8 foo_SIMD(int u, float8 t);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 100;

  std::mt19937 randSource(42);
  std::uniform_real_distribution<float> fRand;

  for (unsigned i = 0; i < numVectors; ++i) {
    int u = i;
    float b[8];

    for (uint i = 0; i < vectorWidth; ++i) {
      b[i] = fRand(randSource);
    }

    float8 rVec = foo_SIMD(u, *((float8*) &b));
    float r[8];
    toArray(rVec, r);


    for (uint i = 0; i < vectorWidth; ++i) {
      float expectedRes = foo(u, b[i]);

      if (!IsClose(r[i], expectedRes)) {
        std::cerr << "MISMATCH!\n";
        std::cerr << i << " : u = " << u << " b = " << b[i] << " expected result (close to) " << expectedRes << " but was " << r[i] << "\n";
        dumpArray(b, vectorWidth); std::cerr << "\n";
        dumpArray(r, vectorWidth); std::cerr << "\n";
        return -1;
      }
    }
  }

  return 0;
}
