// Shapes: C_U, LaunchCode: ivfoo
typedef float float4 __attribute__((ext_vector_type(4)));

#include <cmath>

extern "C"
void
foo(int i, float * A) {
  float v = A[i];
A: // br :T B C
  if (A[i + 1] > 0.0) goto B; else goto C;
B: // br :U C D
  v = A[i + 2];
  if (A[i] > 0.0) goto C; else goto D;
C: // br D
  v = fabs(A[i + 3]);
D:
  A[i] = v * v;
}

//
// At
// |   \
// |     \
// Bu --> C
//  \    |
//   \   |
//     D
