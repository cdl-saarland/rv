#include<iostream>
#include <cassert>
#include "launcherTools.h"

struct Node {
  float data;
  int left;
  int right;

  Node()
  : left(-1)
  , right(-1)
  , data(0.0f)
  {}
};

extern "C" {
  int foo(Node * nodes, float elem);

  int8 foo_SIMD(Node * nodes, float8 elem);
}

void insert(Node * nodes, int i, float elem, int & top) {
  if (top == 0) {
    nodes[0].data = elem;
    ++top;
    return;
  }

  if (nodes[i].data > elem) {
    if (nodes[i].left > 0) insert(nodes, nodes[i].left, elem, top);
    else {
      int j = top++;
      nodes[i].left = j;
      nodes[j].data = elem;
    }

  } else if (elem > nodes[i].data) {
    if (nodes[i].right > 0) insert(nodes, nodes[i].right, elem, top);
    else {
      int j = top++;
      nodes[i].right = j;
      nodes[j].data = elem;
    }
  }
}

int main(int argc, char ** argv) {
  Node nodes[256];
  int top = 0;

  const uint numElems = 128;
  float known[numElems];
  for (int i = 0; i < numElems; ++i) {
    known[i] = wfvRand();
    insert(nodes, 0, known[i], top);
  }


  const uint rounds = 10;

  for (int i = 0; i < rounds; ++i) {
  // random lookup input
    // we pick some known elements to get enough hits
    float elems[8];
    for (int l = 0; l < 8 ; ++l) {
      elems[l] = rand() > 0.0f ? known[rand() % numElems] : wfvRand();
    }
    float8 elemVec = toVec<float, float8>(elems);
    int8 foundVec = foo_SIMD(nodes, elemVec);
    int found[8];
    toArray<int, int8>(foundVec, found);

  // compare result against scalar function
    bool broken = false;
    for (int l = 0; l < 8; ++l) {
      int expectedRes = foo(nodes, elems[l]);
      if (found[l] != expectedRes) {
        std::cerr << "MISMATCH!\n";
        std::cerr << l << " : elem = " << elems[l] << " expected result " << expectedRes << " but was " << found[l] << "\n";
        broken = true;
      }
    }

    if (broken) return -1;
  }

  return 0;
}
