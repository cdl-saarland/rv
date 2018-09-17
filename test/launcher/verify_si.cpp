/*
 * verify_cfloat.cpp
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

struct S {
  float x;
  float y;
  float z;
  float w;
};

extern "C" void foo(S * s, int i);
extern "C" void foo_SIMD(S * s, int i);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 100;

  std::mt19937 randSource(43);
  std::uniform_real_distribution<float> randGen;

  float data[1024];
  float data2[1024];
  for (int i = 0; i < 1024; ++i) {
    data[i] = randGen(randSource);
    data2[i] = randGen(randSource);
  }

  for (unsigned i = 0; i < numVectors; i+= 8) {
    foo_SIMD(reinterpret_cast<S*>(data), i);

    for (int l = 0; l < vectorWidth; ++l) {
      foo(reinterpret_cast<S*>(data2), i+l);
      if (data[i+l] != data2[i+l]) {
        std::cerr << "MISMATCH!\n";
        std::cerr << l << " : expected result " << data2[i+l] << " but was " << data[i+l] << "\n";
        //dumpArray<float>(r, vectorWidth); std::cerr << "\n";
        return -1;
      }
    }
  }

  return 0;
}
