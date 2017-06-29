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

extern "C" void foo(double * A, double * B, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint n = 8 * 800;
  double * A = allocateRandArray<double>(2*n);
  double * B = allocateRandArray<double>(2*n);

  foo(A, B, n);

  size_t hash = hashArray(A, 2*n, 0);
  hash = hashArray(B, 2*n, hash);
  delete A;
  delete B;

  std::cerr << hash << "\n";

  return 0;
}
