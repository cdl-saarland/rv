/*
 * foo_launcher.cpp
 *
 *  Created on: Jul 22, 2015
 *      Author: Simon Moll
 */

#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"
#include "timing.h"

extern "C" float foo(float a, float b);
extern "C" float8 foo_SIMD(float8 a, float8 b);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;
  const uint numVectors = 200;

  float a[8];
  float b[8];
  for (uint i = 0; i < vectorWidth; ++i) {
    a[i] = (float) wfvRand();
    b[i] = (float) wfvRand();
  }

  auto vecStart = GetTime();
  float8 rVec = foo_SIMD(*((float8*) &a), *((float8*) &b));
  auto vecEnd = GetTime();
  float r[8];
  toArray(rVec, r);

  auto start = GetTime();
  for (uint i = 0; i < vectorWidth; ++i) {
    float expectedRes = foo(a[i], b[i]);
    if (r[i] != expectedRes) {
      return -1;
    }
  }
  auto end = GetTime();


  double vecTime = double(GetTimeDiff(vecStart, vecEnd));
  double scalarTime = double(GetTimeDiff(start, end));

  printf("%lf\n",(scalarTime / vecTime));

  return 0;
}
