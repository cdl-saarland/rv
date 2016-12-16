struct Node {
  float data;
  int left;
  int right;
};

extern "C" {

// find
// divergent stack implementation
int foo(Node * nodes, float elem) {
  int stack[64];
  int top = 1;

  stack[0] = 0;
  int found = 0;

  while (top > 0) {
    int next = stack[--top];
    float label = nodes[next].data;
    int left = nodes[next].left;
    int right = nodes[next].right;

    if (label == elem) {
      return 1;
    }

    if (left > 0 && elem < label) {
      stack[top++] = left;
    } else if (right > 0 && label < elem) {
      stack[top++] = right;
    }
  }

  return 0;
}

}
