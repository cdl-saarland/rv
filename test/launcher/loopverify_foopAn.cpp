
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

extern "C" int foo(int * A, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint n = 8 * 800;

  int * A = allocateRandArray<int>(n);
  // foo *pA* -> only non-negative ints
  for (size_t i = 0; i < n; ++i) A[i] = abs(A[i]);

  foo(A, n);

  size_t hash = hashArray(A, n, 0);
  delete A;

  std::cerr << hash << "\n";

  return 0;
}
