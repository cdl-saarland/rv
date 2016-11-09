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

extern "C" void foo(int * A);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 8 * 800;

  int * A = allocateRandArray<int>(n*n);

  foo(A);

  size_t hash = hashArray(A, n*n, 0);
  delete A;

  std::cerr << hash << "\n";

  return 0;
}
