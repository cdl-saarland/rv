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

extern "C" void foo(double * A, double * B, int k, int m, int n);
//  for (int i = 0; i < n; ++i) {
//    memcpy(B + k*i, A + k*i, m);
//  }

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const int n = 128;
  const int m = 3;
  const int k = 7;
  const size_t arrSize = k*n+m;

  double * A = allocateRandArray<double>(arrSize);
  double * B = allocateRandArray<double>(arrSize);

  foo(A, B, k, m, n);

  size_t hash = hashArray(A, n, 0);
  hash = hashArray(B, n, hash);

  delete A;
  delete B;

  std::cerr << hash << "\n";

  return 0;
}
