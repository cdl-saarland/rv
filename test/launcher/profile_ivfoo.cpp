#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>

#include "launcherTools.h"
#include "timing.h"

// typedef float float4 __attribute__((ext_vector_type(4)));

extern "C" void foo(int u, float * A);
extern "C" void foo_SIMD(int u, float * A);

int main(int argc, char ** argv) {
  const uint vectorWidth = 8;

  const uint padded = 2 * vectorWidth;
  float aScalar[padded];
  float aVec[padded];
  for (uint i = 0; i < padded; ++i) {
    aScalar[i] = (float) rand();
    aVec[i] = aScalar[i];
  }

  // invoke SIMD
  auto vecStart = GetTime();
  foo_SIMD(0, aVec);
  auto vecEnd = GetTime();

  // invoke scalar
  auto start = GetTime();
  for (int j = 0; j < vectorWidth; ++j) {
    foo(j, aScalar);
  }
  auto end = GetTime();

  for (uint i = 0; i < padded; ++i) {

    if (aScalar[i] != aVec[i]) {
      return -1;
    }
  }

  double vecTime = double(GetTimeDiff(vecStart, vecEnd));
  double scalarTime = double(GetTimeDiff(start, end));

  printf("%lf\n",(scalarTime / vecTime));

  return 0;
}
