#include <iostream>
#include "launcherTools.h"
#include "timing.h"

struct Point { float x; float y; float z; };

struct T {
  Point a;
  Point b;
};

extern "C" float foo(int i, T * D);
extern "C" float8 foo_SIMD(int i, T * D);

int main(int argc, char ** argv) {
  const uint numInputs = 100;

  const int width = 8;

  T d[width];

  for (unsigned j = 0; j < sizeof(d); j += 4) {
    *(((float*) d) + j) = rand();
  }

  auto vecStart = GetTime();
  float8 simdRes = foo_SIMD(0, d);
  auto vecEnd = GetTime();

  float vecRes[8];
  toArray(simdRes, vecRes);

  float expectedRes[8];
  auto start = GetTime();
  for (int l = 0; l < width; ++l) {
    expectedRes[l] = foo(l, d);
  }
  auto end = GetTime();

  bool error = false;
  for (int l = 0; l < width; ++l) {
    if (vecRes[l] != expectedRes[l]) {
      error = true;
      break;
    }
  }

  if (error) return -1;

  double vecTime = double(GetTimeDiff(vecStart, vecEnd));
  double scalarTime = double(GetTimeDiff(start, end));

  printf("%lf\n",(scalarTime / vecTime));

  return 0;
}
