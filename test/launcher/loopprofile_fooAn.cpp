
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"
#include "timing.h"

extern "C" int foo(int * A, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 8 * 800;

  int * A = allocateRandArray<int>(n);

  auto start = GetTime();
  foo(A, n);
  auto end = GetTime();

  delete A;

  printf("%lu\n", GetTimeDiff(start, end));

  return 0;
}
