
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

extern "C" int foo(float * A, float * B, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 8 * 800;

  float * A = allocateRandArray<float>(n);
  float * B = allocateRandArray<float>(n);

  foo(A, B, n);

  size_t hash = hashArray(A, n, 0);
  hash = hashArray(B, n, 0);
  delete [] A;
  delete [] B;

  std::cerr << hash << "\n";

  return 0;
}
