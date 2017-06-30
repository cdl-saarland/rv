
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

extern "C" int foo(size_t * A, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint n = 8 * 800;

  auto * D = allocateRandArray<size_t>(n * 2);

  foo(D,n);

  size_t hash = hashArray(D, 2*n, 0);
  delete [] D;

  std::cerr << hash << "\n";

  return 0;
}
