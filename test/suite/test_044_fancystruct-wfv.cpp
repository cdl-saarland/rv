// Shapes: C_UrT, LaunchCode: fancystruct

#include <stdio.h>
struct Point { float x; float y; float z; };

struct T {
  Point a;
  Point b;
};

extern "C" float
foo(int i, T * D) {
  float ax = D[i].a.x;
  float ay = D[i].a.y;
  float az = D[i].a.z;

  float bx = D[i].b.x;
  float by = D[i].b.y;
  float bz = D[i].b.z;

  float x = ax + ay + az;
  float y = bx + by + bz;

  return x*y;
}
