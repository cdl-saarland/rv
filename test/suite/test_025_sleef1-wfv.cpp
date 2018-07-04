// Shapes: U_TrT, LaunchCode: sleef
#include <cmath>

extern "C" float
foo(int u, float t) {
  float r = 42.0;
  if (u < 4) {
    r = ldexpf(t, u);
  } else if (u < 8){
    r = sinf(t);
  } else if (u < 12){
    r = cosf(t);
//  } else if (u < 16){
//    sincosf(t, &r1, &r2);
//    r = r1 + r2;
  } else if (u < 15){
    r = fabs(t);
  } else if (u < 20){
    r = tanf(fmod(fabs(t), M_PI) - M_PI / 2.0);
  } else if (u < 24){
    r = atanf(t);
  } else if (u < 28){
    r = atan2f(t, u);
  } else if (u < 32){
    r = asinf(fmod(fabs(t), 2.0) - 1);
  } else if (u < 36){
    r = acosf(fmod(fabs(t), 2.0) - 1);
  } else if (u < 40){
    r = fabs(t);
    r = logf(r);
  } else if (u < 44){
    r = expf(t);
  } else if (u < 48){
    r = fabs(t);
  } else if (u < 50){
    r = sqrtf(t);
  } else if (u < 52){
    r = cbrtf(fabs(t));
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
    r = atanhf(fmod(fabs(t), 2.0) - 1);
  } else if (u < 84){
    r = exp2f(t);
  } else if (u < 88){
    r = exp10f(fmod(t, 100.0));
  } else if (u < 92){
    r = expm1f(t);
  } else if (u < 96){
    r = log10f(fabs(t));
  } else if (u < 100){
    r = log1pf(fabs(t));
  }
  return r;
}
