
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"

extern "C" void foo(float * A, float * B, float * C, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 8 * 800;

  float * A = allocateRandArray<float>(n);
  float * B = allocateRandArray<float>(n);
  float * C = allocateRandArray<float>(n);

  foo(A, B, C, n);

  size_t hash = hashArray(A, n, 0);
  hash = hashArray(B, n, hash);
  hash = hashArray(C, n, hash);

  delete A;
  delete B;
  delete C;

  std::cerr << hash << "\n";

  return 0;
}
