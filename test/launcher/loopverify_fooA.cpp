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

extern "C" float foo(float * A);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 2048 * vectorWidth;

  float * A = allocateRandArray<float>(n);
  foo(A);
  size_t hash = hashArray(A, n, 0);
  delete A;

  std::cerr << hash << "\n";

  return 0;
}
