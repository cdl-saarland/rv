
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

extern "C" int foo(int * A, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 8 * 800;

  int * A = allocateRandArray<int>(n);

  foo(A, n);

  size_t hash = hashArray(A, n, 0);
  delete A;

  std::cerr << hash << "\n";

  return 0;
}
