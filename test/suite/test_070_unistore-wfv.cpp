// Shapes: U_TrT, LaunchCode: bintree
struct Node {
  float data;
  int left;
  int right;
};

extern "C" {

// find
// divergent stack implementation
int foo(Node * nodes, float elem) {
// stack
  int stack[64];
  int top = 0;

  // place a divergent element (uniform ptr, unifom value, varying predicate)
  stack[0] = 0;
  if (elem > 0.0) {
    stack[0] = nodes[0].left;
  }
  top = 1;

  // uniform stack usage
  int next = 0;
  while (nodes[next].left > 0) {
    stack[top++] = next = nodes[next].right;
  }

  // compute stack checksum
  int a = 0;
  for (int i = 0; i < top; ++i) {
    a += stack[i];
  }
  return a;
}

}
