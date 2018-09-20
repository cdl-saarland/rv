// Shapes: T_TrT, LaunchCode: ifoo2f8

#include <cmath>

extern "C" {

bool rv_any(bool);

int
logstar_rec(float a, int n) {
  bool keepGoing = a > 0.0001;
  if (rv_any(keepGoing)) {
    if (a > 0.0001) {
      return 1 + logstar_rec(log(a), n + 1);
    } else {
      return n;
    }
  }
  return n;
}

int
foo(float a, float b)
{
  return logstar_rec(a, 0) / 2;
  // int n = 0;
  // while (a > 0.001) {
  //    a = log(a);
  //    ++n;
  // }
  // return n;
}

}
