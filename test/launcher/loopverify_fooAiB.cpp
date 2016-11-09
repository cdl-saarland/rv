/*
 * foo_launcher.cpp
 *
 *  Created on: Jul 22, 2015
 *      Author: Simon Moll
 */

#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

extern "C" void foo(float * A, int * B, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 8 * 100;

  float * A = allocateRandArray<float>(n);
  int * B = allocateRandArray<int>(n);

  foo(A, B, n);

  size_t aHash = hashArray(A, n, 0);
  size_t hash = hashArray(A, n, aHash);
  delete A;
  delete B;

  std::cerr << hash << "\n";

  return 0;
}
