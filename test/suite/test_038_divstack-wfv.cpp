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

  int next = 0;
  int found = 0;

  while (next >= 0) {
    float label = nodes[next].data;
    int left = nodes[next].left;
    int right = nodes[next].right;

    if (label == elem) {
      break;
    }

    if (left > 0 && elem < label) {
      stack[top++] = left;
      next = left;
    } else if (right > 0 && label < elem) {
      next = right;
    }
  }

  // traverse the stack
  int a = 0;
  for (int i = 0; i < top; ++i) {
    a += stack[i];
  }
  return a;
}

}
