// Shapes: C_UrT, LaunchCode: fancystruct

#include <stdio.h>
struct Point { float x; float y; float z; };

struct T {
  Point a;
  Point b;
};

extern "C" float
foo(int i, T * D) {
  float x = D[i].a.x + D[i].a.y + D[i].a.z;
  float y = D[i].b.x + D[i].b.y + D[i].b.z;

  return x*y;
}
