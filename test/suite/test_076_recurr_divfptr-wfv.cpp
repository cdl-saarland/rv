// Shapes: S4_S4_T, LaunchCode: divfptr

int mult(int x) { return 7 * x + 1; }

int dual(int x) { return 2 * x + 1; }

int beep(int x) { return -x; }

int sum(int x) { return x + 5; }

extern "C" void foo(int *threadId, int *b, int n) {
  for (int i = 0; i < n; i++) {
    int (*funptr)(int);// = nullptr;
    switch (threadId[i] % 8) {
    case 0:
      funptr = &mult;
      break;
    case 1:
      funptr = &dual;
      break;
    case 2:
      funptr = &beep;
      break;
    case 3:
      funptr = &sum;
      break;
    case 4:
      funptr = &dual;
      break;
    case 5:
      funptr = &mult;
      break;
    case 6:
      funptr = &sum;
      break;
    case 7:
      funptr = &beep;
      break;
    }
    b[i * 8] = funptr(threadId[i]);
  }
}
