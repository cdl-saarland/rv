#include "simdxorshift128plus.c"
#include <stdlib.h>

typedef __attribute__((ext_vector_type(4))) double double4;
typedef __attribute__((ext_vector_type(4))) uint64_t ulong4;

typedef __attribute__((ext_vector_type(2))) double double2;

typedef __attribute__((ext_vector_type(8))) float float8;
typedef __attribute__((ext_vector_type(8))) uint32_t uint8;

typedef __attribute__((ext_vector_type(4))) float float4;


static __thread avx_xorshift128plus_key_t staticKey;
static __thread int ready = 0;

__attribute__((cold,noinline))
static void init() {
  uint64_t key1 = rand();
  while (key1 == 0) key1 = rand();
  uint64_t key2 = rand();
  while(key2 == 0) key2 = rand();

  avx_xorshift128plus_init(key1, key2, &staticKey);
  ready = -1;
}


double4 avx_vrand_d4_extra() {
#if 1
  if (!ready) init();
  ulong4 raw = (ulong4) avx_xorshift128plus(&staticKey); // TODO should we shift out the lowest bit (??)
  ulong4 refExp  = (ulong4) 0x3fe0000000000000; // reference exponent (all bits set but ms bit)
  // ulong4 filter  = (ulong4) 0x3fefffffffffffff; // filter the signbit, ms exponent bit
  return (double4) ((raw >> 12) | refExp);
#else
  return (double4){drand48(), drand48(), drand48(), drand48()};
#endif
}

float8 avx_vrand_f8_extra() {
  if (!ready) init();
  uint8 raw = (uint8) avx_xorshift128plus(&staticKey);
  uint8 refExp  = (uint8) 0x3f700000; // reference exponent (all bits set but ms bit)
  // uint8 filter  = (uint8) 0x3f7fffff; // filter the signbit, ms exponent bit
  return (float8) ((raw >> 9 | refExp));
}

#if 0
double2 vrand_d2() {
  init();
}

float8 vrand_f8() {
  init();
}
#endif

