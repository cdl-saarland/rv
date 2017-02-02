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

extern "C" float foo(float u, float t);
extern "C" float8 foo_SIMD(float u, float8 t);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 100;

  for (unsigned i = 0; i < numVectors; ++i) {
    float a = rand();
    float b[8];

    for (uint i = 0; i < vectorWidth; ++i) {
      b[i] = (float) rand();
      // std::cerr << b[i] << " ; ";
    }

    float8 rVec = foo_SIMD(a, *((float8*) &b));
    float r[8];
    toArray(rVec, r);

    for (uint i = 0; i < vectorWidth; ++i) {
      float expectedRes = foo(a, b[i]);

        if (getenv("DUMP_RESULT") ) { dumpArray(r, vectorWidth); std::cerr << "\n"; }

      if (r[i] != expectedRes) {
        std::cerr << "MISMATCH!\n";
        std::cerr << i << " : a = " << a << " b = " << b[i] << " expected result " << expectedRes << " but was " << r[i] << "\n";
        dumpArray(b, vectorWidth); std::cerr << "\n";
        dumpArray(r, vectorWidth); std::cerr << "\n";
        return -1;
      }
    }
  }

  return 0;
}
