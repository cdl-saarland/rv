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
#include "timing.h"

extern "C" void foo(int * A);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 8 * 800;

  int * A = allocateRandArray<int>(n*n);

  auto start = GetTime();
  foo(A);
  auto end = GetTime();

  delete A;

  printf("%lu\n", GetTimeDiff(start, end));
  return 0;
}
