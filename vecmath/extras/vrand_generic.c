#include <stdlib.h>
#include <stdint.h>

typedef __attribute__((ext_vector_type(VECTOR_64))) double vdouble;
typedef __attribute__((ext_vector_type(VECTOR_64))) uint64_t vlong;

typedef __attribute__((ext_vector_type(VECTOR_32))) float vfloat;
typedef __attribute__((ext_vector_type(VECTOR_32))) uint32_t vint;

/* Keys for scalar xorshift128. Must be non-zero
These are modified by xorshift128plus.
 */
struct avx_xorshift128plus_key_s {
    vlong part1;
    vlong part2;
};

typedef struct avx_xorshift128plus_key_s avx_xorshift128plus_key_t;



/**
* You can create a new key like so...
*  avx_xorshift128plus_key_t mykey;
*  avx_xorshift128plus_init(324,4444,&mykey);
*
* This feeds the two integers (324 and 4444) as seeds to the random
* number generator.
*
*  Then you can generate random numbers like so...
*      avx_xorshift128plus(&mykey);
* If your application is threaded, each thread should have its own
* key.
*
*
* The seeds (key1 and key2) should be non-zero. You are responsible for
* checking that they are non-zero.
*/
void avx_xorshift128plus_init(uint64_t key1, uint64_t key2, avx_xorshift128plus_key_t *key);

/*
Return a 256-bit random "number"
*/
vlong avx_xorshift128plus( avx_xorshift128plus_key_t *key);




/* used by xorshift128plus_jump_onkeys */
static void xorshift128plus_onkeys(uint64_t * ps0, uint64_t * ps1) {
	uint64_t s1 = *ps0;
	const uint64_t s0 = *ps1;
	*ps0 = s0;
	s1 ^= s1 << 23; // a
	*ps1 = s1 ^ s0 ^ (s1 >> 18) ^ (s0 >> 5); // b, c
}

/* used by avx_xorshift128plus_init */
static void xorshift128plus_jump_onkeys(uint64_t in1, uint64_t in2,
		uint64_t * output1, uint64_t * output2) {
	/* todo: streamline */
	static const uint64_t JUMP[] = { 0x8a5cd789635d2dff, 0x121fd2155c472f96 };
	uint64_t s0 = 0;
	uint64_t s1 = 0;
	for (unsigned int i = 0; i < sizeof(JUMP) / sizeof(*JUMP); i++)
		for (int b = 0; b < 64; b++) {
			if (JUMP[i] & 1ULL << b) {
				s0 ^= in1;
				s1 ^= in2;
			}
			xorshift128plus_onkeys(&in1, &in2);
		}
	output1[0] = s0;
	output2[0] = s1;
}

void avx_xorshift128plus_init(uint64_t key1, uint64_t key2,
		avx_xorshift128plus_key_t *key) {
	uint64_t S0[VECTOR_64];
	uint64_t S1[VECTOR_64];
	S0[0] = key1;
	S1[0] = key2;
        for (size_t i = 0; i + 1 < VECTOR_64; ++i) {
	  xorshift128plus_jump_onkeys(S0[i], S1[i], &S0[i + 1], &S1[i + 1]);
        }
	key->part1 = *((const vlong *) S0);
	key->part2 = *((const vlong *) S1);
}

/*
 Return a 256-bit random "number"
 */
vlong avx_xorshift128plus(avx_xorshift128plus_key_t *key) {
	vlong s1 = key->part1;
	const vlong s0 = key->part2;
	key->part1 = key->part2;
	s1 = (key->part2 ^  (key->part2 <<  23));
	key->part2 = (((s1 ^ s0) ^ (s1 >> 18)) ^ (s0 >> 5));
	return (key->part2 + s0);
}








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


vdouble
vrand_extra() {
#if 1
  if (!ready) init();
  vlong raw = (vlong) avx_xorshift128plus(&staticKey); // TODO should we shift out the lowest bit (??)
  vlong refExp  = (vlong) 0x3fe0000000000000; // reference exponent (all bits set but ms bit)
  // ulong4 filter  = (ulong4) 0x3fefffffffffffff; // filter the signbit, ms exponent bit
  return (vdouble) ((raw >> 12) | refExp);
#else
  return (double4){drand48(), drand48(), drand48(), drand48()};
#endif
}

vfloat
vrandf_extra() {
  if (!ready) init();
  vint raw = (vint) avx_xorshift128plus(&staticKey);
  vint refExp  = (vint) 0x3f700000; // reference exponent (all bits set but ms bit)
  // uint8 filter  = (uint8) 0x3f7fffff; // filter the signbit, ms exponent bit
  return (vfloat) ((raw >> 9 | refExp));
}

