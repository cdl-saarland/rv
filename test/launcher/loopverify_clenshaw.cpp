
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

extern "C" void foo(double *coeffs,int n,double *xs,double *ys,int m);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint m = 2048 * vectorWidth;
  const uint n = 64;

  double * coeffs = allocateRandArray<double>(n);
  double * xs = allocateRandArray<double>(m);
  double * ys = allocateRandArray<double>(m);

  foo(coeffs, n, xs, ys, m);

  size_t hash = hashArray(coeffs, n, 0);
  hash = hashArray(xs, m, hash);
  hash = hashArray(ys, m, hash);

  delete [] coeffs;
  delete [] xs;
  delete [] ys;

  std::cerr << hash << "\n";

  return 0;
}
