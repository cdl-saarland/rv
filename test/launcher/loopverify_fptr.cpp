#include <iostream>
#include <stdio.h>
#include <stdlib.h>

#include <cassert>
#include <random>

#include "launcherTools.h"

extern "C" void foo(int *threadId, int *b, int n);

int main(int argc, char **argv) {
  const int numInputs = 128;

  int threadId[numInputs], b[numInputs];

  for (int i = 0; i < numInputs; ++i) {
    threadId[i] = i;
    b[i] = 0;
  }

  foo(threadId, b, numInputs);

  size_t hash = hashArray(b, numInputs, 0);

  std::cerr << hash << "\n";
  return 0;
}
