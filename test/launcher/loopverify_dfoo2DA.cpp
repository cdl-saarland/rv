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

extern "C" double foo(int m, int n, double * A);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint m = 8 * 25;
  const uint n = 8 * 25;

  double * A = allocateRandArray<double>(m*n);
  foo(m, n, A);
  size_t hash = hashArray(A, m*n, 0);
  delete A;

  std::cerr << hash << "\n";

  return 0;
}
