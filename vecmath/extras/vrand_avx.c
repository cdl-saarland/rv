#include "simdxorshift128plus.c"
#include <stdlib.h>

typedef __attribute__((ext_vector_type(4))) double double4;
typedef __attribute__((ext_vector_type(2))) double double2;

typedef __attribute__((ext_vector_type(8))) float float8;
typedef __attribute__((ext_vector_type(4))) float float4;


static __thread avx_xorshift128plus_key_t staticKey;
static __thread int ready = 0;

__attribute__((cold,noinline))
static void init() {
  // FIXME use a proper source of random for initialization
  uint64_t key1 = 42;
  uint64_t key2 = 1;
  avx_xorshift128plus_init(key1, key2, &staticKey);
  ready = -1;
}


double4 avx_vrand_d4_extra() {
  if (!ready) init();
  return (double4) avx_xorshift128plus(&staticKey);
}

float8 avx_vrand_f8_extra() {
  if (!ready) init();
  return (float8) avx_xorshift128plus(&staticKey);
}

#if 0
double2 vrand_d2() {
  init();
}

float8 vrand_f8() {
  init();
}
#endif

