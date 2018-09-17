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

extern "C" float foo(float a, float b);
extern "C" float foo_SIMD(float a, float b);

int main(int argc, char ** argv) {
  const uint numInputs = 100;

  std::mt19937 randSource(42);
  std::uniform_real_distribution<float> randGen;

  for (unsigned i = 0; i < numInputs; ++i) {
    float a = randGen(randSource);
    float b = randGen(randSource);
    float expectedRes = foo(a, b);
    float simdRes = foo_SIMD(a, b);

    if (expectedRes != simdRes) {
      std::cerr << "MISMATCH!\n";
      std::cerr << i << " : a = " << a << " b = " << b << " expected result " << expectedRes << " but was " << simdRes << "\n";
      return -1;
    }
  }

  return 0;
}
