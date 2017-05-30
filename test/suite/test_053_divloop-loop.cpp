// LoopHint: 0, LaunchCode: fooABCn

extern "C" void
foo(int *a, int *b, int *c, int n)
{
  for ( int i = 0; i < n; i++) {
    c[i] = a[i] + b[i];
    if ( i/2 == 0 )
      c[i] = c[i] + 5;
  }
}
