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

extern "C" float foo(int n, float * A);

int main(int argc, char ** argv) {
  srand(42);

  const uint vectorWidth = 8;

  const uint n = 8 * 100;

  float * A = allocateRandArray<float>(n);
  auto start = GetTime();
  foo(n, A);
  auto end = GetTime();
  delete A;

  printf("%lu\n", GetTimeDiff(start, end));

  return 0;
}
