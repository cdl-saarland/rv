#include <stdlib.h>
#include <stdio.h>
#include <iostream>

#include <cassert>
#include <random>

#include "launcherTools.h"

// typedef float float4 __attribute__((ext_vector_type(4)));

extern "C" void foo(int *threadId, int *b, int n);
extern "C" void foo_SIMD(int *threadId, int *b, int8 n);

int main(int argc, char ** argv) {
  const int n = 8;
  const int vectorWidth = 8;

  const int numRounds = 100;

  std::mt19937 randSource(42);
  std::uniform_int_distribution<int32_t> intRandGen;

  const int arrSize = n * vectorWidth + vectorWidth;
  int threadId[arrSize], threadIdScalar[arrSize];
  int b[arrSize], bScalar[arrSize];
  int fooN[arrSize];

  for (unsigned j = 0; j < numRounds; ++j) {

    // generate random input
    for (int i = 0; i < arrSize; ++i) {
      threadIdScalar[i] = threadId[i] = std::max(0, intRandGen(randSource));
      bScalar[i] = b[i] = std::max(0, intRandGen(randSource));
    }

    // random truncation of n
    for (int l = 0; l < vectorWidth; ++l) {
      fooN[l] = intRandGen(randSource) % n;
    }

    // _before_ foo call (to avoid overwriting)
    int8 nVec = toVec<int, int8>(fooN);

    // invoke scalar
    for (int l = 0; l < vectorWidth; ++l) {
      foo(threadId + l, bScalar + l, fooN[l]);
    }

    // invoke SIMD (C_C_T)
    foo_SIMD(threadId, b, nVec);

    // check output
    for (int i = 0; i < arrSize; ++i) {
      if (b[i] != bScalar[i]) {
        std::cerr << "MISMATCH!\n";
        std::cerr << i << " : b[i] = " << b[i] << " bScalar[i] = " << bScalar[i] << "\n";
        return -1;
      }
    }
  }

  return 0;
}
