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
  int next = 0;
  int found = 0;

  while (next >= 0) {
    float label = nodes[next].data;
    int left = nodes[next].left;
    int right = nodes[next].right;

    if (label == elem) {
      return 1;
    }

    if (left > 0 && elem < label) {
      next = left;
    } else if (right > 0 && label < elem) {
      next = right;
    }
  }

  return 0;
}

}
