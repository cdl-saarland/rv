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

extern "C" void foo(int *a, int *b, int *c, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = vectorWidth * 800;

  int * A = allocateRandArray<int>(n*2);
  int * B = allocateRandArray<int>(n*2);
  int * C = allocateRandArray<int>(n*2);

  foo(A, C, B, n);

  size_t hash = hashArray(A, n, 0);
  hash = hashArray(B, n, hash);
  hash = hashArray(C, n, hash);

  delete A;
  delete B;
  delete C;

  std::cerr << hash << "\n";

  return 0;
}
