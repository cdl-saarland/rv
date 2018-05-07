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
#include <random>

#include "launcherTools.h"

extern "C" int foo(float a, float b);
extern "C" int8 foo_SIMD(float8 a, float8 b);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 100;

  std::mt19937 randSource(42);
  std::uniform_real_distribution<float> randGen;

  for (unsigned i = 0; i < numVectors; ++i) {
    float a[8];
    float b[8];
    for (uint i = 0; i < vectorWidth; ++i) {
      a[i] = randGen(randSource);
      b[i] = randGen(randSource);
    }

    int8 rVec = foo_SIMD(*((float8*) &a), *((float8*) &b));
    int r[8];
    toArray<int, int8>(rVec, r);

    for (uint i = 0; i < vectorWidth; ++i) {
      float expectedRes = foo(a[i], b[i]);
      if (r[i] != expectedRes) {
        std::cerr << "MISMATCH!\n";
        std::cerr << i << " : a = " << a[i] << " b = " << b[i] << " expected result " << expectedRes << " but was " << r[i] << "\n";
        dumpArray<float>(a, vectorWidth); std::cerr << "\n";
        dumpArray<float>(b, vectorWidth); std::cerr << "\n";
        dumpArray<int>(r, vectorWidth); std::cerr << "\n";
        return -1;
      }
    }
  }

  return 0;
}
