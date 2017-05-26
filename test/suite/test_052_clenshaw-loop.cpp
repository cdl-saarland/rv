// LoopHint: 0, LaunchCode: clenshaw
//

extern "C" void
foo(double *coeffs,int n,double *xs,double *ys,int m)
{
  for (int i=0;i<m;i++){
    double x = xs[i];
    double u0=0,u1=0,u2=0;
    for (int k=n;k>=0;k--){
      u2 = u1;
      u1 = u0;
      u0 = 2*x*u1-u2+coeffs[k];
    }
    ys[i] = 0.5*(coeffs[0]+u0-u2);
  }
}
