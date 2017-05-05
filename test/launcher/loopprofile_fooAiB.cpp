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

extern "C" void foo(float * A, int * B, int n);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 8 * 100;

  float * A = allocateRandArray<float>(n);
  int * B = allocateRandArray<int>(n);

  auto start = GetTime();
  foo(A, B, n);
  auto end = GetTime();

  delete A;
  delete B;

  printf("%lu\n", GetTimeDiff(start, end));

  return 0;
}
