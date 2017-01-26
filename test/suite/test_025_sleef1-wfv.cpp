// Shapes: U_TrT, LaunchCode: sleef
#include <cmath>

extern "C" float
foo(int u, float t) {
  float r,r1,r2;
  if (u < 4) {
    r = ldexpf(t, u);
  } else if (u < 8){
    r = sinf(t);
  } else if (u < 12){
    r = cosf(t);
//  } else if (u < 16){
//    sincosf(t, &r1, &r2);
//    r = r1 + r2;
  } else if (u < 20){
    r = fabs(t);
    r = tanf(fmod(r, M_PI) - M_PI / 2.0);
  } else if (u < 24){
    r = atanf(t);
  } else if (u < 28){
    r = atan2f(t, u);
  } else if (u < 32){
    r = fabs(t);
    r = asinf(fmod(r, 2.0) - 1);
  } else if (u < 36){
    r = fabs(t);
    r = acosf(fmod(r, 2.0) - 1);
  } else if (u < 40){
    r = fabs(t);
    r = logf(r);
  } else if (u < 44){
    r = expf(t);
  } else if (u < 48){
    r = fabs(t);
    r = sqrtf(r);
  } else if (u < 52){
    r = fabs(t);
    r = cbrtf(t);
  } else if (u < 56){
    r = powf(t, u % 10);
  } else if (u < 60){
    r = sinhf(fmod(t, 10.0));
  } else if (u < 64){
    r = coshf(fmod(t, 10.0));
  } else if (u < 68){
    r = tanhf(t);
  } else if (u < 72){
    r = asinhf(t);
  } else if (u < 76){
    r = acoshf(t);
  } else if (u < 80){
    r = fabs(t);
    r = atanhf(fmod(r, 2.0) - 1);
  } else if (u < 84){
    r = exp2f(t);
  } else if (u < 88){
    r = exp10f(fmod(t, 100.0));
  } else if (u < 92){
    r = expm1f(t);
  } else if (u < 96){
    r = fabs(t);
    r = log10f(t);
  } else if (u < 100){
    r = fabs(t);
    r = log1pf(t);
  }
  return r;
}
