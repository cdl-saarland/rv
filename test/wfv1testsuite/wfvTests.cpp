//===- wfvTests.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg
//

#include <stdio.h>
#include <math.h>
//#include <mmintrin.h> //MMX
#include <xmmintrin.h> //SSE
//#include <emmintrin.h> //SSE2
//#include <pmmintrin.h> //SSE3
//#include <tmmintrin.h> //SSSE3
//#include <ammintrin.h> //SSE4a
//#include <smmintrin.h> //SSE4.1
//#include <nmmintrin.h> //SSE4.2
//#include <bmmintrin.h> //SSE5

#include <vector>

//#define RUN_IRREDUCIBLE_TESTS
#define USE_RANDOM_TESTS
#define NUM_RANDOM_INPUT_VALUE_SETS 10 // 100 takes several minutes
#include <ctime>    // For time()
#include <cstdlib>  // For srand() and rand()
#include <string.h> // For strcmp()

#define SIMD_WIDTH 4

typedef float VEC  __attribute__((ext_vector_type(4)));
typedef int   VECI __attribute__((ext_vector_type(4)));

#define ALIGN __attribute__ ((aligned (16)))

// helper: SmallVector does not allow to use __m128 directly (no constructor?!)
struct V
{
    V() {}
    V(VEC a) { data = a; }
    VEC data;
} ALIGN;

void
*aligned_malloc(const size_t size, const size_t align_size)
{
  char *ptr,*ptr2,*aligned_ptr;
  const int align_mask = align_size - 1;

  ptr=(char *)malloc(size + align_size + sizeof(int));
  if(ptr==NULL) return(NULL);

  ptr2 = ptr + sizeof(int);
  aligned_ptr = ptr2 + (align_size - ((size_t)ptr2 & align_mask));

  ptr2 = aligned_ptr - sizeof(int);
  *((int *)ptr2)=(int)(aligned_ptr - ptr);

  return(aligned_ptr);
}

// helper: extract ith element of a __m128
inline float&
get(const VEC& v, const unsigned idx)
{
    return ((float*)&v)[idx];
}

inline float&
get(const V& v, const unsigned idx)
{
    return ((float*)&v)[idx];
}

inline float&
get(const V* v, const unsigned idx)
{
    return ((float*)&(v->data))[idx];
}

// due to imprecisions etc. we apply a few rules
// when to treat results as equal.
// - float == float -> equality of first 5 decimal places
// - NaN == NaN -> true ;)
bool
resultMatches(const float a, const float b)
{
	return a == b ||
		(isnan(a) && isnan(b)) ||
		(fabs(a-b) < 0.000001);
}

//----------------------------------------------------------------------------//
// declarations of empty prototypes of packetized functions
//----------------------------------------------------------------------------//

//arithmetic only
extern "C" VEC test_001_simple_SIMD(VEC a, VEC b) ALIGN;

//simple control flow
extern "C" VEC test_002_if01_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_003_if02_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_004_if03_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_005_if04_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_006_if05_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_007_if06_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_008_if07_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_009_if08_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_010_if09_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_011_if10_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_012_if11_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_013_if12_SIMD(VEC a, VEC b) ALIGN;

//simple loops
extern "C" VEC test_014_loop01_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_015_loop02_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_016_loop03_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_017_loop04_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_018_loop05_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_019_loop06_SIMD(VEC a, VEC b) ALIGN;

//more complex loops
extern "C" VEC test_020_loopc01_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_021_loopc02_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_022_loopc03_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_023_loopc04_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_024_loopc05_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_025_loopc06_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_026_loopc07_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_027_loopc08_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_028_loopc09_SIMD(VEC a, VEC b) ALIGN;

//loops with multiple exits
extern "C" VEC test_029_loopmx01_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_030_loopmx02_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_031_loopmx03_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_032_loopmx04_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_033_loopmx05_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_034_loopmx06_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_035_loopmx07_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_036_loopmx08_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_037_loopmx09_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_038_loopmx10_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_039_loopmx11_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_040_loopmx12_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_041_loopmx13_SIMD(VEC a, VEC b) ALIGN;

//nested loops
extern "C" VEC test_042_loopns01_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_043_loopns02_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_044_loopns03_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_045_loopns04_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_046_loopns05_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_047_loopns06_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_048_loopns07_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_049_loopns08_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_050_loopns09_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_051_loopns10_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_052_loopns11_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_053_loopns12_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_054_loopns13_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_055_loopns14_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_056_loopns15_SIMD(VEC a, VEC b) ALIGN;

//nested loops with multiple exits
extern "C" VEC test_057_loopnsmx01_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_058_loopnsmx02_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_059_loopnsmx03_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_060_loopnsmx04_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_061_loopnsmx05_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_062_loopnsmx06_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_063_loopnsmx07_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_064_loopnsmx08_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_065_loopnsmx09_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_066_loopnsmx10_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_067_loopnsmx11_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_068_loopnsmx12_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_069_loopnsmx13_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_070_loopnsmx14_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_071_loopnsmx15_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_072_loopnsmx16_SIMD(VEC a, VEC b) ALIGN;

//function calls
extern "C" VEC test_073_call01_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_074_call02_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_075_call03_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_076_call04_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_077_call05_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_078_call06_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_079_call07_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_080_call08_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_081_call09_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_082_call10_SIMD(VEC a, VEC b) ALIGN;

// misc
extern "C" VEC test_083_misc_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_084_ocl_mandelbrot_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_085_noise_SIMD(VEC a, VEC b) ALIGN;
#if 0
extern "C" VEC test_086_ocl_aobench_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_087_ocl_aobench_inlined_SIMD(VEC a, VEC b) ALIGN;
#endif

// irreducible control flow
extern "C" VEC test_088_irreducible1_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_089_irreducible2_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_090_irreducible3_SIMD(VEC a, VEC b) ALIGN;
extern "C" VEC test_091_irreducible4_SIMD(VEC a, VEC b) ALIGN;

//----------------------------------------------------------------------------//
// implementations of corresponding scalar source functions
//----------------------------------------------------------------------------//
//arithmetic only
extern "C" float
test_001_simple(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    return x / y;
}

//simple control flow
extern "C" float
test_002_if01(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z;

    if (x<y) z = a;
    else z = a*a;

    return z;
}

extern "C" float
test_003_if02(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = a*a;

    if (x<y) z = z + a;

    z = z+b;

    return z;
}

extern "C" float
test_004_if03(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z;

    if (x<y) {
        z = a+x;
    } else {
        z = a*a;
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_005_if04(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z;
    float r;

    if (x<y) {
        z = a+x;
        r = x*x;
    } else {
        z = a*a;
        r = x-a;
    }

    z = z+x;
    z = y-z;

    return z * r;
}

extern "C" float
test_006_if05(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z;
    float r;

    if (x<y) {
        z = a+x;
        r = x*x;
    } else if (x>y) {
        z = a*a;
        r = x-a;
    } else {
        z = y-a;
        r = y+a;
    }

    z = z+x;
    z = y-z;

    return z * r;
}

extern "C" float
test_007_if06(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z;
    float r;

    if (x>y) {
        z = a+x;
        r = x*x;
    } else if (y>x) {
        z = a*a;
        if (z != y) r = x-a;
        else r = x+a;
    } else {
        z = y-a;
        r = y+a;
    }

    z = z+x;
    z = y-z;

    return z * r;
}

extern "C" float
test_008_if07(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 3;

    if (x<y) {
        z = a+x;
        r += z*z;
        float f = z-r;
        z -= f;
    }

    z = z+x;

    return z * r;
}

extern "C" float
test_009_if08(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 3;

    if ((a <= z && b > 4) || z>y) {
        z = a+x;
        r += z*z;
        float f = z-r;
        z -= f;
    }

    z = z+x;

    return z * r;
}

extern "C" float
test_010_if09(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z;

    if (x<y) {
        z = a+x;
    } else {
        z = a*a;
    }

    if (z > a && a < b) {
        z++;
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_011_if10(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    if (y > b) {
        z *= z;

        if (x<y) {
            z = a+x;
        } else {
            z = a*a;
        }

        z -= a;

        if (z > a && a < b) {
            z++;
        }

        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_012_if11(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    if (a < b) {
        z += a;
    } else if (b < a) {
        z += a*a;
    }

    z = z+x;
    z = y-z;

    return z;
}

// Some non-standard control-flow where a VARYING block
// does not post-dominate the common dominator of all
// incoming paths (mask generation regression test).
extern "C" float
test_013_if12(float a, float b)
{
    float x = a + b;
    float y = x * x - b;

	if (a > b) {
		++x;
	} else {
		x *= 2.f;
		if (x > 4.f) goto X;
	}

	x *= a;

	if (a > 10.f) {
		x += 4.4f;
	} else {
X:		x /= 3.f;
	}

    return x / y;
}

//simple loops
extern "C" float
test_014_loop01(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += x;
    }

    return z;
}

extern "C" float
test_015_loop02(float a, float b)
{
    int i = 0;
    float sum = a*b;

    do {
        sum += i;
        i++;
    } while (i < 42);

    return sum;
//    float i = 0.4f;
//    float sum = a*b;
//
//    do {
//        sum += i;
//        i+=a;
//    } while (i < 42.3f);
//
//    return sum;
}

extern "C" float
test_016_loop03(float a, float b)
{
    int i = 0;
    float sum = a*b;

    do {
        sum += i;
        i++;
    } while (i < 42);

    return sum;
}

extern "C" float
test_017_loop04(float a, float b)
{
    int i = 0;
    float sum = a*b;

    do {
	i++;
    } while (sum += i,i < 42);
    return sum;
}

extern "C" float
test_018_loop05(float a, float b)
{
    int i = 0;
    float sum = a*b;

    do {

    } while (sum += i,i++,i < 42);

    return sum;
}

extern "C" float
test_019_loop06(float a, float b)
{
    int i = 0;
    float sum = a*b+42;

    do ; while (++i,i < 1000);

    return sum;
}

//more complex loops
extern "C" float
test_020_loopc01(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    int i = 0;
    int j = 100;
    while (i<1000 && j != 42) {
        z -= 3*y;
        --j;
        ++i;
    }

    return z;
}

extern "C" float
test_021_loopc02(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = a;

    for (int i=0; i<1000; ++i) {
        if (z<y) {
            z += a;
        } else {
            z += b;
        }
    }

    return z-b;
}

extern "C" float
test_022_loopc03(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        if (x<y) {
            z += a;
        } else {
            z += a*a;
        }
    }

    return z-b;
}

extern "C" float
test_023_loopc04(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    if (x<y) {
        for (int i=0; i<1000; ++i) {
            z += a;
        }
    } else {
        z += a*a;
    }

    return z-b;
}

extern "C" float
test_024_loopc05(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        if (z > 200) continue;
        z += x;
    }

    z = z-y;

    return z;
}

extern "C" float
test_025_loopc06(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a; ++i) {
        z += x;
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_026_loopc07(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 42.42f;

    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
        z += x*i;
    }

    return z*r;
}

extern "C" float
test_027_loopc08(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a && z >= x-b; ++i) {
        if (z-x == y) z += x;
        else {
            z = a;
            if (z / a < x) {
                z += a/i;
            }
        }
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_028_loopc09(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 42.42f;

    for (float i=3.2f; i<a; i=i+2) {
        if (x<y) {
            z = a+x;
            r = x*x;
            y++;
        } else if (x>y) {
            z = a*a;
            y--;
            if (z != y) r = x-a;
            else r = x+a;
        } else {
            z = y-a;
            r = y+a;
        }
        z += x;
    }

    return z*r;
}

//loops with multiple exits
extern "C" float
test_029_loopmx01(float a, float b)
{
    int i = 0;
    float sum = a*b;

    for (;;) {
	sum += i;
	i++;
	if (i >= 42) break;
    }

    return sum;
}

extern "C" float
test_030_loopmx02(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        if (z > 104.0f) break; //needs icmp + masking
        z += x; // 98/96/94/92 += 10
    }

    z = z-y;

    return z;
}

extern "C" float
test_031_loopmx03(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<100; ++i) {
        z += x;
        if (i > a) break;
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_032_loopmx04(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    if (x<y) {
        for (int i=0; i<10; ++i) {
            z += a;
            if (z > 53.12f) return a;
        }
    } else {
        z += a*a;
    }

    return z-b;
}

extern "C" float
test_033_loopmx05(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;
    for (int i=5; i<1000; ++i) {
        if (z-x == y) continue;
        z += a;
        if (z / a < x) break;
    }

    z = z-y;

    z+=a;

    return z;
}

extern "C" float
test_034_loopmx06(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;
    for (int i=5; i<1000; ++i) {
        if (z-x == y) continue;
        z += a;
        if (z / a < x) break;
    }

    z = z-y;

    if (a > b) {
        z = z+y;
    } else {
        z--;
    }

    z+=a;

    return z;
}

extern "C" float
test_035_loopmx07(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;
    for (int i=0; i<1000; ++i) {
        z += a;
        if (z-x == y) {
            z *= 133.0f;
            continue;
        }
        z += y;
        if (z / a > x) {
            z /= -2.11f;
            break;
        }
        z -= b;
    }

    z = z-y;

    return z;
}

// Has a "FULLY_UNIFORM" block between loop and if.
extern "C" float
test_036_loopmx08(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r;

    for (int i=0; i<100; ++i) {
        if (z-x == y) continue;
        z += x;
        if (z / a < x) break;
    }

    if (x<y) {
        z = a+x;
        r = x*x;
    } else if (x>y) {
        z -= a*a;
        if (z != y) r = x-a;
        else r = x+a;
    } else {
        z = y-a;
        r = y+a;
    }

    z = z+x;
    z = y-z;

    return z * r;
}

extern "C" float
test_037_loopmx09(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += x;
        if (z > 600) continue;
        if (z-x == y) break;
        z += a;
        if (z > 200) continue;
        if (z / a < x) break;
        z -= b;
    }

    z = z-y;

    return z;
}

extern "C" float
test_038_loopmx10(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    //the 'butterfly' / 'lung'
    for (int i=0; i<100; ++i) {
        z += a;
        if (z > a) {
            a *= a;
            if (a > b) break;
        } else {
            a += a;
            if (a < b) break;
        }
        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_039_loopmx11(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    //the 'butterfly' / 'lung'
    for (int i=0; i<100; ++i) {
        z += a;
        if (a > 4) {
            if (a > b) break;
        } else {
            if (a < b) break;
        }
        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_040_loopmx12(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    //the 'butterfly' / 'lung' with vectorized loop and stuff :P
    for (int i=0; i<a && z != y*y; ++i) {
        z += a;
        if (z > a) {
            z += 7.33f;
            a *= a;
            if (a > b) break;
            z -= 7.13f;
        } else {
            z -= 7.33f;
            a += a;
            if (a < b) break;
            z += 7.13f;
        }
        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_041_loopmx13(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    //the 'butterfly' / 'lung' with vectorized loop and stuff :P
    for (int i=0; i<a && z != y*y; ++i) {
        z += a;
        if (a > 4) {
            z += 7.33f;
            if (a > b) break;
            z -= 7.13f;
        } else {
            z -= 7.33f;
            if (a < b) break;
            z += 7.13f;
        }
        z += b;
    }

    z = z+x;
    z = y-z;

    return z;
}

//nested loops
extern "C" float
test_042_loopns01(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<100; ++i) {
        for (int j=0; j<100; ++j) {
            z += x;
        }
        z -= 3*y;
    }

    return z;
}

// FULLY_UNIFORM outer loop, UNIFORM inner loops,
// VARYING if-statements, UNIFORM if inside innermost loop (i == j)
extern "C" float
test_043_loopns02(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += a;
        if (z / a < x) z += a;
        else {
            z -= b;
            if (a > b) z -= b;
            else {
                z *= z-y;
                if (b == a) {
                    for (int j=0; j<200; ++j) {
                        if (i == j) z *= z;
                        z += 13.2f;
                    }
                    z = a+3;
                } else {
                    ++z;
                }
            }
            for (int j=0; j<100; ++j) {
                if (i < j) z += a;
                else z -= 13.2f;
            }
        }
    }

    z = z-y;

    return z;
}

// VARYING inner loop, FULLY_UNIFORM outer loop
extern "C" float
test_044_loopns03(float a, float b)
{
    float x = a + b;
    float y = x;
    float z = y;

	for (int i=0; i<100; ++i) {
		for (int j=0; j<a; ++j) {
            z += x;
        }
    }

    z = z-y;

    return z;
}

// UNIFORM inner loop, VARYING outer loop
extern "C" float
test_045_loopns04(float a, float b)
{
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<100; ++j) {
            z += x;
        }
    }

    z = z-y;

    return z;
}

// VARYING innermost loop, UNIFORM inner loop, VARYING outer loop
extern "C" float
test_046_loopns05(float a, float b)
{
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<10; ++j) {
			for (int k=0; k<b; ++k) {
            	z += x;
			}
        }
    }

    z = z-y;

    return z;
}

// UNIFORM innermost loop, VARYING outer loops
extern "C" float
test_047_loopns06(float a, float b)
{
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<b; ++j) {
			for (int k=0; k<10; ++k) {
				z -= a;
			}
        }
    }

    z = z-y;

    return z;
}

// UNIFORM and VARYING inner loops, VARYING outer loop, several nesting levels
// The input values are capped to prevent loops calculating too long
extern "C" float
test_048_loopns07(float a, float b)
{
	a = a > 20.f ? 20.f : a;
	b = b > 20.f ? 20.f : b;
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<10; ++j) {
			for (int k=0; k<x; ++k) {
				for (int l=0; l<a-b; ++l) {
					z += x;
				}
			}
        }
        for (int j=0; j<b; ++j) {
			for (int k=0; k<10; ++k) {
				z -= a;
			}
        }
    }

    z = z-y;

    return z;
}

// like ns7, but with weird innermost loop induction variable that can diverge
// for different instances and thus cannot be kept UNIFORM.
// somehow, the weird innermost loop prevents it from calculating forever for
// large input values.
extern "C" float
test_049_loopns08(float a, float b)
{
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<10; ++j) {
			for (int k=0; k<x; ++k) {
				for (; k<a-b; ++k) {
					z += x;
				}
			}
        }
        for (int j=0; j<b; ++j) {
			for (int k=0; k<10; ++k) {
				z -= a;
			}
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_050_loopns09(float a, float b)
{
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<b; ++j) {
            z += x;
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_051_loopns10(float a, float b)
{
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<b; ++j) {
            z += a-i;
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_052_loopns11(float a, float b)
{
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<b; ++j) {
            a -= i;
            z += a;
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_053_loopns12(float a, float b)
{
    float x = a + b;
    float y = x;
    float z = y;

    for (int i=0; i<a; ++i) {
        for (int j=0; j<b; ++j) {
            z += y-i*j;
        }
        z -= a/i;
    }

    z = z-y;

    return z;
}

extern "C" float
test_054_loopns13(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<b; ++i) {
        z *= a;
        for (int j=0; j<100; ++j) {
            for (int k=0; k<a; ++k) {
                a -= i;
                z += x+k*j-a;
            }
            z -= i*b - j*x;
        }
        a *= i;
    }

    z = z-y;

    return z;
}

extern "C" float
test_055_loopns14(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a && z >= x-b; ++i) {
        if (z-x == y) z += x;
        else {
            z = a;
            for (int j=0; j<a; ++j) {
                z -= 12.33f;
                if (z / a < x) {
                    z += a;
                } else {
                    ++z;
                }
            }
        }
    }

    z = z+x;
    z = y-z;

    return z;
}

extern "C" float
test_056_loopns15(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a && z >= x-b; ++i) {
        if (z-x == y) z += x;
        else {
            z = a;
            for (int j=0; j<a; ++j) {
                z -= 12.33f;
                if (z / a < x) {
                    z += a*i;
                }
                z *= a-b;
                if (i == j) {
                    for (int k=0; k<a; k+=2) {
                        z += 5.33f;
                    }
                } else {
                    ++z;
                }
            }
        }
    }

    z = z+x;
    z = y-z;

    return z;
}

//nested loops with multiple exits

// FULLY_UNIFORM loops
extern "C" float
test_057_loopnsmx01(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += a;
        for (int j=3; j<200; ++j) {
            z /= -0.12f;
            if (i > 500) break;
            if (i*j > 2000) return z;
        }
    }

    z = z-y;

    return z;
}

// UNIFORM outer loop becomes VARYING due to return
extern "C" float
test_058_loopnsmx02(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<100; ++i) {
        z++;
        for (int j=0; j<b; ++j) {
            z -= i;
            if (z < 0.f) return z;
        }
    }

    z = z-y;

    return z;
}

// VARYING outer loop, UNIFORM inner loop becomes VARYING due to return
extern "C" float
test_059_loopnsmx03(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<b; ++i) {
        z++;
        for (int j=0; j<100; ++j) {
            z -= i;
            if (z < 0.f) return z;
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_060_loopnsmx04(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<2; ++i) {
        z += 1;
        for (int j=0; j<3; ++j) {
            if (z < 0.f) return z;
        }
    }

    return -9999;
}

extern "C" float
test_061_loopnsmx05(float a, float b)
{
    float z = a;
    float r = 1;
    for (int i=0; i<10; ++i) {
        for (int j=0; j<10; ++j) {
            if (z > 120.5123f) return r;
            z += 2;
        }
        r = 5;
    }
    return 7;
}

extern "C" float
test_062_loopnsmx06(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a; ++i) {
        z++;
        for (int j=0; j<b; ++j) {
            z -= i;
            if (z < 0.f) return z;
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_063_loopnsmx07(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<2; ++i) {
        z += 1;
        for (int j=0; j<3; ++j) {
            z /= -1.f;
            if (z < 0.f) return z;
        }
    }

    for (int i=0; i<a; ++i) {
        z++;
        for (int j=0; j<b; ++j) {
            z -= i;
            if (z < 0.f) return z;
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_064_loopnsmx08(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<2; ++i) {
        z += 1;
        for (int j=0; j<3; ++j) {
            z /= -1.f;
            if (z < -100.f) break;
            if (z < 0.f) return z;
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_065_loopnsmx09(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += a;
        for (int j=0; j<200; ++j) {
            z /= -0.12f;
            if (z < -100.f) break;
            if (z < 0.f) return z;
        }
    }

    for (int i=0; i<2; ++i) {
        z += 1;
        for (int j=0; j<3; ++j) {
            z /= -1.f;
            if (z < -100.f) break;
            if (z < 0.f) return z;
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_066_loopnsmx10(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<1000; ++i) {
        z += a;
        if (z / a < x) break;
        else {
            z -= b;
            if (a > b) {
                for (int j=3; j<4500; ++j) {
                    if (i == j) z /= -0.12f;
                    if (z < -100.f) break;
                    if (z < 0.f) return z;
                }
                continue;
            }
            else {
                z *= z-y;
                if (b == a) {
                    return z;
                } else {
                    ++z;
                    break;
                }
            }
        }
    }

    z = z-y;

    return z;
}

extern "C" float
test_067_loopnsmx11(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;
    float r = 42.42f;

    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
        for (int j=0; j<b; ++j) {
            if (z > 120.5123f) break;
            z += y-i;
        }
        if (x>y) {
            z = a+x;
            r = x*x;
            continue;
        } else if (y>x) {
            for (int k=0; k<x; ++k) {
                z = z-3*a;
                //if (k*z > 120.5123f) break;
                if (a > 120.5123f) return z;
                else if (z == b*b) continue;
                z -= b*3.32f;
            }
            if (z != y) r = x-a;
            else if (z < y) r = x+a;
            else return b;
        } else {
            z = y-a;
            r = y+a;
        }
    }

    z = z-y;

    return z*r;
}

extern "C" float
test_068_loopnsmx12(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a*b; ++i) {
        z += 1;
        for (int j=0; j<b; ++j) {
            z /= -1.32f;
            if (a < b) break;
            z += a;
            for (int k=0; k<a; ++k) {
                z += a/b;
                if (b < a) goto X;
                --z;
            }
            if (a == b) return z;
        }
    }

X:  z = z-y;

    return z;
}

extern "C" float
test_069_loopnsmx13small(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<2; ++i) {
        for (int j=0; j<2; ++j) {
            for (int k=0; k<2; ++k) {
                if (z < 0.f) return z;
                --z;
            }
        }
    }

    return -999.f;
}

extern "C" float
test_069_loopnsmx13(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (int i=0; i<a*b; ++i) {
        z += 1;
        for (int j=0; j<b; ++j) {
            z /= -1.32f;
            if (z < -100.f) break;
            z += a;
            for (int k=0; k<a; ++k) {
                z += a/b;
                if (z > 100.f) goto X;
                --z;
            }
            if (z < 0.f) return z;
        }
    }

X:  z = z-y;

    return z;
}

// no idea if this is guaranteed to always terminate, but it seems so ^^
// also no idea if all paths actually can be executed ^^
extern "C" float
test_070_loopnsmx14(float a, float b)
{
	float z, r, x = a + b;
    float y=z=r= x * x - b;
    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
X:    for (int j=0; j<b; ++j) {
        if (z > 120.5123f) goto Y;
        z += y-i;
      }
      if (x>y) {
Y:          z = a+x;
            r = x*x;
            continue;
      } else if (y>x) {
            for (int k=0; k<x; ++k) {
				z = z-3*a;
                if (a > 120.5123f) goto Z;
                else if (z == b*b) continue;
                z -= b*3.32f;
            }
            if (z != y) r = x-a;
            else if (z < y) goto X;
            else return b;
        } else {
            z = y-a;
            r = y+a;
        }
    }
Z: z -= r;
    return z;
}

extern "C" float
test_071_loopnsmx15(float a, float b)
{
	float z, r, x = a + b;
    float y=z=r= x * x - b;
    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
X:    for (int j=0; j<b; ++j) {
        if (z > 120.5123f) goto Z;
        z += y-i;
      }
      if (x>y) {
            if (z != y) r = x-a;
            else if (z < y) goto X;
            else return b;
        } else {
            z = y-a;
            r = y+a;
        }
    }
Z: z -= r;
    return z;
}

extern "C" float
test_072_loopnsmx16(float a, float b)
{
	float z, r, x = a + b;
    float y =z=r= x * x - b;
    for (float i=3.2f; i<a && z <= b*y; i=i+2) {
X:    for (int j=0; j<b; ++j) {
        if (z > 120.5123f) goto Y;
        z += y-i;
      }
      if (x>y) {
            z = a+x;
            r = x*x;
            continue;
      } else if (y>x) {
Y:         for (int k=0; k<x; ++k) {
                z = z-3*a;
                if (a > 120.5123f) goto Z;
                else if (z == b*b) continue;
                z -= b*3.32f;
            }
            if (z != y) r = x-a;
            else if (z < y) goto X;
            else return b;
        } else {
            z = y-a;
            r = y+a;
        }
    }
Z: z -= r;
    return z;
}

//function calls
extern "C" float
test_073_call01(float a, float b)
{
    return cosf(a*b);
}

extern "C" float
test_074_call02(float a, float b)
{
    return floorf(a/b);
}

extern "C" float
test_075_call03(float a, float b)
{
    return a > b ? logf(a-b) : 1.f;
}

extern "C" float
test_076_call04(float a, float b)
{
    return a > b ? logf(a-b) : sinf(b*a);
}

extern "C" float
test_077_call05(float a, float b)
{
    float z;

	if (a > b) {
        z = floorf(a);
        z += cosf(b);
    }
    else {
        z = sqrtf(b);
        z += logf(b);
    }

    return z;
}

extern "C" float
test_078_call06(float a, float b)
{
	// call to function without packetized native equivalent
	// (requires splitting)
    return powf(a, b);
}

extern "C" float
test_079_call07(float a, float b)
{
	// call to function without packetized native equivalent
	// (requires splitting with if-cascade (non-true-mask)
    return a < b ? a : powf(a, b);
}

extern "C" __attribute__((noinline)) float
noinlinecall(float x)
{
	return x*x;
}

extern "C" float
test_080_call08(float a, float b)
{
	// call to function without packetized native equivalent
	// (requires splitting with if-cascade (non-true-mask)
    return a < b ? a : noinlinecall(a+b);
}

extern "C" __attribute__((noinline)) float
noinlinecall2(float x)
{
	return x*x;
}

extern "C" __attribute__((noinline)) VEC
noinlinecall2_SIMD(VEC x)
{
	return _mm_mul_ps(x,x);
}

extern "C" float
test_081_call09(float a, float b)
{
    return a < b ? a : noinlinecall2(a+b);
}

extern "C" __attribute__((noinline)) float
noinlinecall3(float x)
{
	return x*x;
}

extern "C" __attribute__((noinline)) VEC
noinlinecall3_SIMD(VEC x, VEC mask)
{
	if (_mm_movemask_ps(mask)) return _mm_mul_ps(x,x);
	else return _mm_sub_ps(x, _mm_set_ps1(1.f));
}

extern "C" float
test_082_call10(float a, float b)
{
    return a < b ? a : noinlinecall3(a+b);
}

// uniformly calculated induction variable is used after
// varying loop -> has to be varying as well, broadcasting
// behind loop will not yield correct results!
extern "C" float
test_083_misc(float a, float b)
{
    unsigned iter=0;
    for(iter=0; (iter < a); ++iter)
    {
    }
	return a - iter;
}

extern "C" float
test_084_ocl_mandelbrot(float a, float b)
{
	const float tid = (float)a;
	const float width = (float)b;
	const float scale = 1.f;
	const float maxIterations = 20;

    const float i = tid-width; // should be %, not possible with float
    const float j = tid/width;

    const float x0 = ((i*scale) - ((scale/2)*width))/width;
    const float y0 = ((j*scale) - ((scale/2)*width))/width;

    float x = x0;
    float y = y0;

    float x2 = x*x;
    float y2 = y*y;

    const float scaleSquare = scale * scale;

    unsigned iter=0;
    for(iter=0; (x2+y2 <= scaleSquare) && (iter < maxIterations); ++iter)
    {
        y = 2 * x * y + y0;
        x = x2 - y2   + x0;

        x2 = x*x;
        y2 = y*y;
    }
    return (float)(255*iter/maxIterations);
}

//array access
extern "C" float
test_085_noise(float x, float y)
{
    float z = x*y;
    static int p[] = { 151,160,137,91,90,15,
        131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
        190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
        88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
        77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
        102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
        135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
        5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
        223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
        129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
        251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
        49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
        138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180,
        151,160,137,91,90,15,
        131,13,201,95,96,53,194,233,7,225,140,36,103,30,69,142,8,99,37,240,21,10,23,
        190, 6,148,247,120,234,75,0,26,197,62,94,252,219,203,117,35,11,32,57,177,33,
        88,237,149,56,87,174,20,125,136,171,168, 68,175,74,165,71,134,139,48,27,166,
        77,146,158,231,83,111,229,122,60,211,133,230,220,105,92,41,55,46,245,40,244,
        102,143,54, 65,25,63,161, 1,216,80,73,209,76,132,187,208, 89,18,169,200,196,
        135,130,116,188,159,86,164,100,109,198,173,186, 3,64,52,217,226,250,124,123,
        5,202,38,147,118,126,255,82,85,212,207,206,59,227,47,16,58,17,182,189,28,42,
        223,183,170,213,119,248,152, 2,44,154,163, 70,221,153,101,155,167, 43,172,9,
        129,22,39,253, 19,98,108,110,79,113,224,232,178,185, 112,104,218,246,97,228,
        251,34,242,193,238,210,144,12,191,179,162,241, 81,51,145,235,249,14,239,107,
        49,192,214, 31,181,199,106,157,184, 84,204,176,115,121,50,45,127, 4,150,254,
        138,236,205,93,222,114,67,29,24,72,243,141,128,195,78,66,215,61,156,180
    };

    //here begins 'real' noise-function
    const int X = (int)floorf(x) & 255;                  // FIND UNIT CUBE THAT
    const int Y = (int)floorf(y) & 255;                  // CONTAINS POINT.
    const int Z = (int)floorf(z) & 255;
    x -= floorf(x);                                      // FIND RELATIVE X,Y,Z
    y -= floorf(y);                                      // OF POINT IN CUBE.
    z -= floorf(z);
    const float U = x * x * x * (x * (x * 6 - 15) + 10);  // COMPUTE FADE CURVES
    const float V = y * y * y * (y * (y * 6 - 15) + 10);  // FOR EACH OF X,Y,Z.
    const float W = z * z * z * (z * (z * 6 - 15) + 10);
    const int A = p[X  ]+Y, AA = p[A]+Z, AB = p[A+1]+Z;   // HASH COORDINATES OF
    const int B = p[X+1]+Y, BA = p[B]+Z, BB = p[B+1]+Z;   // THE 8 CUBE CORNERS,

//      return lerp(w, lerp(v, lerp(u, grad(p[AA  ], x  , y  , z   ),  // AND ADD
//                                     grad(p[BA  ], x-1, y  , z   )), // BLENDED
//                             lerp(u, grad(p[AB  ], x  , y-1, z   ),  // RESULTS
//                                     grad(p[BB  ], x-1, y-1, z   ))),// FROM  8
//                     lerp(v, lerp(u, grad(p[AA+1], x  , y  , z-1 ),  // CORNERS
//                                     grad(p[BA+1], x-1, y  , z-1 )), // OF CUBE
//                             lerp(u, grad(p[AB+1], x  , y-1, z-1 ),
//                                     grad(p[BB+1], x-1, y-1, z-1 ))));

    int h = p[AA  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    float u = h<8 ? x : y;                 // INTO 12 GRADIENT DIRECTIONS.
    float v = h<4 ? y : h==12||h==14 ? x : z;
    const float grad1 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad1 = grad(p[AA  ], x  , y  , z   );

    h = p[BA  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y : h==12||h==14 ? x-1 : z;
    const float grad2 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad2 = grad(p[BA  ], x-1, y  , z   );

    h = p[AB  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x : z;
    const float grad3 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad3 = grad(p[AB  ], x  , y-1, z   );

    h = p[BB  ] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x-1 : z;
    const float grad4 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad4 = grad(p[BB  ], x-1, y-1, z   );

    h = p[AA+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x : y;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y : h==12||h==14 ? x : z-1;
    const float grad5 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad5 = grad(p[AA+1], x  , y  , z-1 );

    h = p[BA+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y : h==12||h==14 ? x-1 : z-1;
    const float grad6 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad6 = grad(p[BA+1], x-1, y  , z-1 );

    h = p[AB+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x : z-1;
    const float grad7 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad7 = grad(p[AB+1], x  , y-1, z-1 );

    h = p[BB+1] & 15;                      // CONVERT LO 4 BITS OF HASH CODE
    u = h<8 ? x-1 : y-1;                 // INTO 12 GRADIENT DIRECTIONS.
    v = h<4 ? y-1 : h==12||h==14 ? x-1 : z-1;
    const float grad8 = ((h&1) == 0 ? u : -u) + ((h&2) == 0 ? v : -v);
    //const float grad8 = grad(p[BB+1], x-1, y-1, z-1 );

    const float lerp1 = grad1 + U * (grad2 - grad1); //lerp(u, grad1, grad2);
    const float lerp2 = grad3 + U * (grad4 - grad3); //lerp(u, grad3, grad4);
    const float lerp3 = grad5 + U * (grad6 - grad5); //lerp(u, grad5, grad6);
    const float lerp4 = grad7 + U * (grad8 - grad7); //lerp(u, grad7, grad8);
    const float lerp12 = lerp1 + V * (lerp2 - lerp1); //lerp(v, lerp1, lerp2);
    const float lerp34 = lerp3 + V * (lerp4 - lerp3); //lerp(v, lerp3, lerp4);
    return lerp12 + W * (lerp34 - lerp12); //lerp(w, lerp12, lerp34);
}

//
// AOBench
// Adopted from AO Bench [http://lucille.atso-net.jp/aobench/]
//
struct Ray
{
	float orgX, orgY, orgZ;
	float dirX, dirY, dirZ;
};
struct Sphere
{
	float centerX, centerY, centerZ;
	float radius;
};
struct Plane
{
	float pX, pY, pZ;
	float nX, nY, nZ;
};

struct Intersection
{
    float t;
    float pX, pY, pZ;     // hit point
    float nX, nY, nZ;     // normal
    int hit;
};

extern "C" void
sphere_intersect(const struct Sphere* s, const struct Ray* ray, struct Intersection* isect)
{
    const float rsX = ray->orgX - s->centerX;
    const float rsY = ray->orgY - s->centerY;
    const float rsZ = ray->orgZ - s->centerZ;
    const float B = rsX*ray->dirX + rsY*ray->dirY + rsZ*ray->dirZ; //dot(rs, ray->dir);
    const float C = (rsX*rsX + rsY*rsY + rsZ*rsZ) - (s->radius * s->radius); //dot(rs, rs) - (s->radius * s->radius);
    const float D = B * B - C;

    if (D > 0.0f)
    {
		const float t = -B - sqrtf(D);
		if ( (t > 0.0f) && (t < isect->t) )
		{
			isect->t = t;
			isect->hit = 1;

			// calculate normal.
			const float pX = ray->orgX + ray->dirX * t;
			const float pY = ray->orgY + ray->dirY * t;
			const float pZ = ray->orgZ + ray->dirZ * t;
			float nX = pX - s->centerX;
			float nY = pY - s->centerY;
			float nZ = pZ - s->centerZ;
			//n = normalize(n);
			const float ls = nX*nX + nY*nY + nZ*nZ;
			const float l = 1.0f/sqrtf(ls);
			nX = nX * l;
			nY = nY * l;
			nZ = nZ * l;
			isect->nX = nX;
			isect->nY = nY;
			isect->nZ = nZ;
			isect->pX = pX;
			isect->pY = pY;
			isect->pZ = pZ;
		}
	}
}

extern "C" void
plane_intersect(const struct Plane* pl, const struct Ray* ray, struct Intersection* isect)
{
  	const float d = 1.0f - (pl->pX*pl->nX + pl->pY*pl->nY + pl->pZ*pl->nZ); //-dot(pl->p, pl->n);
	const float v = ray->dirX*pl->nX + ray->dirY*pl->nY + ray->dirZ*pl->nZ; //dot(ray->dir, pl->n);

	if (fabsf(v) < 1.0e-6f)
		return; // the plane is parallel to the ray.

    const float t = (1.0f - (ray->orgX*pl->nX + ray->orgY*pl->nY + ray->orgZ*pl->nZ + d)) / v; //-(dot(ray->org, pl->n) + d) / v;

    if ( (t > 0.0f) && (t < isect->t) )
    {
		isect->hit = 1;
		isect->t   = t;
		isect->nX   = pl->nX;
		isect->nY   = pl->nY;
		isect->nZ   = pl->nZ;

		const float pX = ray->orgX + t * ray->dirX;
		const float pY = ray->orgY + t * ray->dirY;
		const float pZ = ray->orgZ + t * ray->dirZ;
		isect->pX = pX;
		isect->pY = pY;
		isect->pZ = pZ;
	}
}

extern "C" void
Intersect(const struct Ray* r, struct Intersection* i)
{
	struct Sphere sphere[3];
	sphere[0].centerX = -2.0f;
	sphere[0].centerY = 0.0f;
	sphere[0].centerZ = -3.5f;
	sphere[0].radius = 0.5f;
	sphere[1].centerX = -0.5f;
	sphere[1].centerY = 0.0f;
	sphere[1].centerZ = -3.0f;
	sphere[1].radius = 0.5f;
	sphere[2].centerX = 1.0f;
	sphere[2].centerY = 0.0f;
	sphere[2].centerZ = -2.2f;
	sphere[2].radius = 0.5f;
	struct Plane pl;
    pl.pX = 0.0f;
	pl.pY = -0.5f;
	pl.pZ = 0.0f;
    pl.nX = 0.0f;
	pl.nY = 1.0f;
	pl.nZ = 0.0f;

	//sphere_intersect(&sphere[0], r, i);
	//sphere_intersect(&sphere[1], r, i);
	sphere_intersect(&sphere[2], r, i);
	plane_intersect(&pl, r, i);
}

extern "C" void
orthoBasis(float basisX[3], float basisY[3], float basisZ[3], const float nX, const float nY, const float nZ)
{
	basisX[2] = nX;
	basisY[2] = nY;
	basisZ[2] = nZ;
	basisX[1] = 0.0f;
	basisY[1] = 0.0f;
	basisZ[1] = 0.0f;

	if ((nX < 0.6f) && (nX > -0.6f))
		basisX[1] = 1.0f;
	else if ((nY < 0.6f) && (nY > -0.6f))
		basisY[1] = 1.0f;
	else if ((nZ < 0.6f) && (nZ > -0.6f))
		basisZ[1] = 1.0f;
	else
		basisX[1] = 1.0f;


	//basis[0] = cross(basis[1], basis[2]);
	basisX[0] = basisY[1] * basisZ[2] - basisZ[1] * basisY[2];
	basisY[0] = basisZ[1] * basisX[2] - basisX[1] * basisZ[2];
	basisZ[0] = basisX[1] * basisY[2] - basisY[1] * basisX[2];
	//basis[0] = normalize(basis[0]);
	const float ls0 = basisX[0]*basisX[0] + basisY[0]*basisY[0] + basisZ[0]*basisZ[0];
	const float l0 = 1.0f/sqrtf(ls0);
	basisX[0] = basisX[0] * l0;
	basisY[0] = basisY[0] * l0;
	basisZ[0] = basisZ[0] * l0;

	//basis[1] = cross(basis[2], basis[0]);
	basisX[1] = basisY[2] * basisZ[0] - basisZ[2] * basisY[0];
	basisY[1] = basisZ[2] * basisX[0] - basisX[2] * basisZ[0];
	basisZ[1] = basisX[2] * basisY[0] - basisY[2] * basisX[0];
	//basis[1] = normalize(basis[1]);
	const float ls1 = basisX[1]*basisX[1] + basisY[1]*basisY[1] + basisZ[1]*basisZ[1];
	const float l1 = 1.0f/sqrtf(ls1);
	basisX[1] = basisX[1] * l1;
	basisY[1] = basisY[1] * l1;
	basisZ[1] = basisZ[1] * l1;

}

extern "C" float
computeAO(struct Intersection* isect, float* sd)
{
	int i, j;
	const int ntheta = 16;
	const int nphi   = 16;
	float eps  = 0.0001f;

	// Slightly move ray org towards ray dir to avoid numerical probrem.
	float pX = isect->pX + eps * isect->nX;
	float pY = isect->pY + eps * isect->nY;
	float pZ = isect->pZ + eps * isect->nZ;

	// Calculate orthogonal basis.
	float basisX[3], basisY[3], basisZ[3];
	orthoBasis(basisX, basisY, basisZ, isect->nX, isect->nY, isect->nZ);

	float occlusion = 0.0f;

	for (j = 0; j < ntheta; j++)
	{
		for (i = 0; i < nphi; i++)
		{
			// Pick a random ray direction with importance sampling.
			// p = cos(theta) / 3.141592f
			*sd = (int)(fmodf((float)(*sd)*1364.0f+626.0f, 509.0f));
			const float r = *sd/509.0f;    //(float)(seed)/509.0f;//random(seed);
			*sd = (int)(fmodf((float)(*sd)*1364.0f+626.0f, 509.0f));
			const float phi = *sd/509.0f * 2.0f * 3.141592f;  //2.0f * 3.141592f * random();

			const float refX = cosf(phi) * sqrtf(1.0f - r);
			const float refY = sinf(phi) * sqrtf(1.0f - r);
			const float refZ = sqrtf(r);

			// local -> global
			const float rrayX = refX * basisX[0] + refY * basisX[1] + refZ * basisX[2];
			const float rrayY = refX * basisY[0] + refY * basisY[1] + refZ * basisY[2];
			const float rrayZ = refX * basisZ[0] + refY * basisZ[1] + refZ * basisZ[2];

			struct Ray ray;
			ray.orgX = pX;
			ray.orgY = pY;
			ray.orgZ = pZ;
			ray.dirX = rrayX;
			ray.dirY = rrayY;
			ray.dirZ = rrayZ;

			struct Intersection occIsect;
			occIsect.hit = 0;
			occIsect.t = 1.0e+30f;
			occIsect.nX = occIsect.pX = 0.0f;
			occIsect.nY = occIsect.pY = 0.0f;
			occIsect.nZ = occIsect.pZ = 0.0f;
			Intersect(&ray, &occIsect);
			if (occIsect.hit != 0)
				occlusion += 1.0f;
		}
	}

	// [0.0, 1.0]
	occlusion = ((float)(ntheta * nphi) - occlusion) / (float)(ntheta * nphi);
	return occlusion;
}

#if 0
extern "C" float
test_086_ocl_aobench(float x, float y)
{

	unsigned nIndex = (int)x;

	struct Intersection i;
	i.hit = 0;
	i.t = 1.0e+30f;
	i.nX = i.pX = 0;
	i.nY = i.pY = 0;
	i.nZ = i.pZ = 0;

	const float px = ((float)(int)(nIndex) - 512) / 512.0f;
	const float py = ((float)(int)(nIndex) - 512) / 512.0f;
	const float ls = px*px + py*py + 1.0f;
	const float l = 1.0f/sqrtf(ls);
	const float dirX = px * l;
	const float dirY = py * l;
	const float dirZ = -1.0f * l;
	struct Ray r;
	r.orgX = 0;
	r.orgY = 0;
	r.orgZ = 0;
	r.dirX = dirX;
	r.dirY = dirY;
	r.dirZ = dirZ;
	int seed = (int)(fmodf((dirX+512.0f) * (dirY+512.0f) * 4525434.0f, 65536.0f));

	int rcol = 0;
	Intersect(&r, &i);
	if (i.hit != 0)
	{
		float s = seed;
		rcol = (int)(computeAO(&i, &s) * 255);
		seed = s;
	}

	return (float)(rcol | (rcol<<8) | (rcol<<16) | (255<<24));
}

#if 0
extern "C" float
test_087_ocl_aobench_inlined(float x, float y)
{
	unsigned nIndex = (int)x;

	struct Intersection isect;
	isect.hit = 0;
	isect.t = 1.0e+30f;
	isect.nX = isect.pX = 0;
	isect.nY = isect.pY = 0;
	isect.nZ = isect.pZ = 0;

	const float px = ((float)(int)(nIndex) - 512) / 512.0f;
	const float py = ((float)(int)(nIndex) - 512) / 512.0f;
	const float ls = px*px + py*py + 1.0f;
	const float l = 1.0f/sqrtf(ls);
	const float dirX = px * l;
	const float dirY = py * l;
	const float dirZ = -1.0f * l;
	struct Ray ray;
	ray.orgX = 0;
	ray.orgY = 0;
	ray.orgZ = 0;
	ray.dirX = dirX;
	ray.dirY = dirY;
	ray.dirZ = dirZ;
	int seed = (int)(fmodf((dirX+512.0f) * (dirY+512.0f) * 4525434.0f, 65536.0f));

	int rcol = 0;

    //////////////////// Intersect start

    struct Sphere sphere[3];
	sphere[0].centerX = -2.0f;
	sphere[0].centerY = 0.0f;
	sphere[0].centerZ = -3.5f;
	sphere[0].radius = 0.5f;
	sphere[1].centerX = -0.5f;
	sphere[1].centerY = 0.0f;
	sphere[1].centerZ = -3.0f;
	sphere[1].radius = 0.5f;
	sphere[2].centerX = 1.0f;
	sphere[2].centerY = 0.0f;
	sphere[2].centerZ = -2.2f;
	sphere[2].radius = 0.5f;
	struct Plane pl;
    pl.pX = 0.0f;
	pl.pY = -0.5f;
	pl.pZ = 0.0f;
    pl.nX = 0.0f;
	pl.nY = 1.0f;
	pl.nZ = 0.0f;

    //////////////////// sphere_intersect start

    const float rsX = ray.orgX - sphere[2].centerX;
    const float rsY = ray.orgY - sphere[2].centerY;
    const float rsZ = ray.orgZ - sphere[2].centerZ;
    const float B = rsX*ray.dirX + rsY*ray.dirY + rsZ*ray.dirZ;
    const float C = (rsX*rsX + rsY*rsY + rsZ*rsZ) - (sphere[2].radius * sphere[2].radius);
    const float D = B * B - C;

    if (D > 0.0f)
    {
		const float t = -B - sqrtf(D);
		if ( (t > 0.0f) && (t < isect.t) )
		{
			isect.t = t;
			isect.hit = 1;

			// calculate normal.
			const float pX = ray.orgX + ray.dirX * t;
			const float pY = ray.orgY + ray.dirY * t;
			const float pZ = ray.orgZ + ray.dirZ * t;
			float nX = pX - sphere[2].centerX;
			float nY = pY - sphere[2].centerY;
			float nZ = pZ - sphere[2].centerZ;
			//n = normalize(n);
			const float ls = nX*nX + nY*nY + nZ*nZ;
			const float l = 1.0f/sqrtf(ls);
			nX = nX * l;
			nY = nY * l;
			nZ = nZ * l;
			isect.nX = nX;
			isect.nY = nY;
			isect.nZ = nZ;
			isect.pX = pX;
			isect.pY = pY;
			isect.pZ = pZ;
		}
	}

    //////////////////// sphere_intersect end

    //////////////////// plane_intersect start

  	const float d = 1.0f - (pl.pX*pl.nX + pl.pY*pl.nY + pl.pZ*pl.nZ);
	const float v = ray.dirX*pl.nX + ray.dirY*pl.nY + ray.dirZ*pl.nZ;

	if (fabsf(v) >= 1.0e-6f)
    {
        const float t = (1.0f - (ray.orgX*pl.nX + ray.orgY*pl.nY + ray.orgZ*pl.nZ + d)) / v;

        if ( (t > 0.0f) && (t < isect.t) )
        {
            isect.hit = 1;
            isect.t   = t;
            isect.nX   = pl.nX;
            isect.nY   = pl.nY;
            isect.nZ   = pl.nZ;

            const float pX = ray.orgX + t * ray.dirX;
            const float pY = ray.orgY + t * ray.dirY;
            const float pZ = ray.orgZ + t * ray.dirZ;
            isect.pX = pX;
            isect.pY = pY;
            isect.pZ = pZ;
        }
    }

    //////////////////// plane_intersect end

    //////////////////// Intersect end

	if (isect.hit != 0)
	{
		float sd = seed;

        //////////////////// computeAO start

        int i, j;
        const int ntheta = 16;
        const int nphi   = 16;
        float eps  = 0.0001f;

        // Slightly move ray org towards ray dir to avoid numerical probrem.
        float pX = isect.pX + eps * isect.nX;
        float pY = isect.pY + eps * isect.nY;
        float pZ = isect.pZ + eps * isect.nZ;

        // Calculate orthogonal basis.
        float basisX[3], basisY[3], basisZ[3];

        //////////////////// orthoBasis start

        basisX[2] = isect.nX;
        basisY[2] = isect.nY;
        basisZ[2] = isect.nZ;
        basisX[1] = 0.0f;
        basisY[1] = 0.0f;
        basisZ[1] = 0.0f;

        if ((isect.nX < 0.6f) && (isect.nX > -0.6f))
            basisX[1] = 1.0f;
        else if ((isect.nY < 0.6f) && (isect.nY > -0.6f))
            basisY[1] = 1.0f;
        else if ((isect.nZ < 0.6f) && (isect.nZ > -0.6f))
            basisZ[1] = 1.0f;
        else
            basisX[1] = 1.0f;


        //basis[0] = cross(basis[1], basis[2]);
        basisX[0] = basisY[1] * basisZ[2] - basisZ[1] * basisY[2];
        basisY[0] = basisZ[1] * basisX[2] - basisX[1] * basisZ[2];
        basisZ[0] = basisX[1] * basisY[2] - basisY[1] * basisX[2];
        //basis[0] = normalize(basis[0]);
        const float ls0 = basisX[0]*basisX[0] + basisY[0]*basisY[0] + basisZ[0]*basisZ[0];
        const float l0 = 1.0f/sqrtf(ls0);
        basisX[0] = basisX[0] * l0;
        basisY[0] = basisY[0] * l0;
        basisZ[0] = basisZ[0] * l0;

        //basis[1] = cross(basis[2], basis[0]);
        basisX[1] = basisY[2] * basisZ[0] - basisZ[2] * basisY[0];
        basisY[1] = basisZ[2] * basisX[0] - basisX[2] * basisZ[0];
        basisZ[1] = basisX[2] * basisY[0] - basisY[2] * basisX[0];
        //basis[1] = normalize(basis[1]);
        const float ls1 = basisX[1]*basisX[1] + basisY[1]*basisY[1] + basisZ[1]*basisZ[1];
        const float l1 = 1.0f/sqrtf(ls1);
        basisX[1] = basisX[1] * l1;
        basisY[1] = basisY[1] * l1;
        basisZ[1] = basisZ[1] * l1;

        //////////////////// orthoBasis end

        float occlusion = 0.0f;

        for (j = 0; j < ntheta; j++)
        {
            for (i = 0; i < nphi; i++)
            {
                // Pick a random ray direction with importance sampling.
                // p = cos(theta) / 3.141592f
                sd = (int)(fmodf((float)sd*1364.0f+626.0f, 509.0f));
                const float r = sd/509.0f;    //(float)(seed)/509.0f;//random(seed);
                sd = (int)(fmodf((float)sd*1364.0f+626.0f, 509.0f));
                const float phi = sd/509.0f * 2.0f * 3.141592f;  //2.0f * 3.141592f * random();

                const float refX = cosf(phi) * sqrtf(1.0f - r);
                const float refY = sinf(phi) * sqrtf(1.0f - r);
                const float refZ = sqrtf(r);

                // local -> global
                const float rrayX = refX * basisX[0] + refY * basisX[1] + refZ * basisX[2];
                const float rrayY = refX * basisY[0] + refY * basisY[1] + refZ * basisY[2];
                const float rrayZ = refX * basisZ[0] + refY * basisZ[1] + refZ * basisZ[2];

                struct Ray occRay;
                occRay.orgX = pX;
                occRay.orgY = pY;
                occRay.orgZ = pZ;
                occRay.dirX = rrayX;
                occRay.dirY = rrayY;
                occRay.dirZ = rrayZ;

                struct Intersection occIsect;
                occIsect.hit = 0;
                occIsect.t = 1.0e+30f;
                occIsect.nX = occIsect.pX = 0.0f;
                occIsect.nY = occIsect.pY = 0.0f;
                occIsect.nZ = occIsect.pZ = 0.0f;

                //////////////////// Intersect start

                struct Sphere sphereOcc[3];
                sphereOcc[0].centerX = -2.0f;
                sphereOcc[0].centerY = 0.0f;
                sphereOcc[0].centerZ = -3.5f;
                sphereOcc[0].radius = 0.5f;
                sphereOcc[1].centerX = -0.5f;
                sphereOcc[1].centerY = 0.0f;
                sphereOcc[1].centerZ = -3.0f;
                sphereOcc[1].radius = 0.5f;
                sphereOcc[2].centerX = 1.0f;
                sphereOcc[2].centerY = 0.0f;
                sphereOcc[2].centerZ = -2.2f;
                sphereOcc[2].radius = 0.5f;
                struct Plane plOcc;
                plOcc.pX = 0.0f;
                plOcc.pY = -0.5f;
                plOcc.pZ = 0.0f;
                plOcc.nX = 0.0f;
                plOcc.nY = 1.0f;
                plOcc.nZ = 0.0f;

                //////////////////// sphere_intersect start

                const float rsX = occRay.orgX - sphereOcc[2].centerX;
                const float rsY = occRay.orgY - sphereOcc[2].centerY;
                const float rsZ = occRay.orgZ - sphereOcc[2].centerZ;
                const float B = rsX*occRay.dirX + rsY*occRay.dirY + rsZ*occRay.dirZ;
                const float C = (rsX*rsX + rsY*rsY + rsZ*rsZ) - (sphereOcc[2].radius * sphereOcc[2].radius);
                const float D = B * B - C;

                if (D > 0.0f)
                {
                    const float t = -B - sqrtf(D);
                    if ( (t > 0.0f) && (t < occIsect.t) )
                    {
                        occIsect.t = t;
                        occIsect.hit = 1;

                        // calculate normal.
                        const float pX = occRay.orgX + occRay.dirX * t;
                        const float pY = occRay.orgY + occRay.dirY * t;
                        const float pZ = occRay.orgZ + occRay.dirZ * t;
                        float nX = pX - sphereOcc[2].centerX;
                        float nY = pY - sphereOcc[2].centerY;
                        float nZ = pZ - sphereOcc[2].centerZ;
                        //n = normalize(n);
                        const float ls = nX*nX + nY*nY + nZ*nZ;
                        const float l = 1.0f/sqrtf(ls);
                        nX = nX * l;
                        nY = nY * l;
                        nZ = nZ * l;
                        occIsect.nX = nX;
                        occIsect.nY = nY;
                        occIsect.nZ = nZ;
                        occIsect.pX = pX;
                        occIsect.pY = pY;
                        occIsect.pZ = pZ;
                    }
                }

                //////////////////// sphere_intersect end

                //////////////////// plane_intersect start

                const float d = 1.0f - (plOcc.pX*plOcc.nX + plOcc.pY*plOcc.nY + plOcc.pZ*plOcc.nZ);
                const float v = occRay.dirX*plOcc.nX + occRay.dirY*plOcc.nY + occRay.dirZ*plOcc.nZ;

                if (fabsf(v) >= 1.0e-6f)
                {
                    const float t = (1.0f - (occRay.orgX*plOcc.nX + occRay.orgY*plOcc.nY + occRay.orgZ*plOcc.nZ + d)) / v;

                    if ( (t > 0.0f) && (t < occIsect.t) )
                    {
                        occIsect.hit = 1;
                        occIsect.t   = t;
                        occIsect.nX   = plOcc.nX;
                        occIsect.nY   = plOcc.nY;
                        occIsect.nZ   = plOcc.nZ;

                        const float pX = occRay.orgX + t * occRay.dirX;
                        const float pY = occRay.orgY + t * occRay.dirY;
                        const float pZ = occRay.orgZ + t * occRay.dirZ;
                        occIsect.pX = pX;
                        occIsect.pY = pY;
                        occIsect.pZ = pZ;
                    }
                }

                //////////////////// plane_intersect end

                //////////////////// Intersect end

                if (occIsect.hit != 0)
                    occlusion += 1.0f;
            }
        }

        // [0.0, 1.0]
        occlusion = ((float)(ntheta * nphi) - occlusion) / (float)(ntheta * nphi);

        //////////////////// computeAO end

        rcol = (int)occlusion * 255;
		seed = sd;
	}

	return (float)(rcol | (rcol<<8) | (rcol<<16) | (255<<24));
}
#endif
#endif

#ifdef RUN_IRREDUCIBLE_TESTS
//irreducible control-flow tests
extern "C" float
test_088_irreducible1(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    int i=0;
    if (a > b) goto G;

    for(; i<100; ++i) {
        z += a;
G:      ++z;
    }

    return z-b;
}

extern "C" float
test_089_irreducible2(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    int i=0, j=0;
    if (a > b) goto G;

    for(; i<a; ++i) {
        z += a;
	for (; j<b; ++j) {
	    ++z;
G:	    z -= 2.f;
	}
    }

    return z-b;
}

extern "C" float
test_090_irreducible3(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    int i=0, j=0;
    if (a > b) goto G;
    if (b > a) goto G2;

    for(; i<a; ++i) {
        z += a;
	for (; j<b; ++j) {
	    ++z;
	    if (z < 0.f) goto X;
G:	    z -= 2.f;
	}
	++z;
	if (z < -100.f) return a;
G2:	++z;
    }

X:  z -= b;

    return z;
}

extern "C" float
test_091_irreducible4(float a, float b)
{
    float x = a + b;
    float y = x * x - b;
    float z = y;

    for (unsigned k=0; k<a; ++k) {
        int i=0, j=0;
        if (a > b) goto G;
        if (b > a) goto G2;

        for(; i<a; ++i) {
            z += a;
	    for (; j<b; ++j) {
	        ++z;
	        if (z < 0.f) goto X;
G:	        z -= 2.f;
	    }
	    ++z;
	    if (z < -100.f) return a;
G2:	    ++z;
        }

X:      z -= b;
    }

    return z;
}
#endif

//----------------------------------------------------------------------------//
// add test cases to test suite (uncomment / comment blocks to enable/disable)
//----------------------------------------------------------------------------//
typedef float (*scalarFnType)(float, float);
typedef VEC (*vecFnType)(VEC, VEC) ALIGN;

#define ADD_TEST(name) \
	testCases.push_back(std::make_pair("test_" #name "", std::make_pair(test_##name, test_##name##_SIMD)))

// If any of these lines in added without the appropriate function being generated,
// the test suite will fail with a segfault due to a bad function pointer.
// TODO: Uncommenting lines in this file on the other hand does not make any difference...
typedef std::pair<const char*, std::pair<scalarFnType, vecFnType> > TestCaseType;
typedef std::vector<TestCaseType> TestCaseVecType;
void
addTestCases(TestCaseVecType& testCases)
{
#if 0
#endif
	ADD_TEST(001_simple);

	ADD_TEST(002_if01);
	ADD_TEST(003_if02);
	ADD_TEST(004_if03);
	ADD_TEST(005_if04);
	ADD_TEST(006_if05);
	ADD_TEST(007_if06);
	ADD_TEST(008_if07);
	ADD_TEST(009_if08);
	ADD_TEST(010_if09);
	ADD_TEST(011_if10);
	ADD_TEST(012_if11);
	ADD_TEST(013_if12);

	ADD_TEST(014_loop01);
	ADD_TEST(015_loop02);
	ADD_TEST(016_loop03);
	ADD_TEST(017_loop04);
	ADD_TEST(018_loop05);
	ADD_TEST(019_loop06);

	ADD_TEST(020_loopc01);
	ADD_TEST(021_loopc02);
	ADD_TEST(022_loopc03);
	ADD_TEST(023_loopc04);
	ADD_TEST(024_loopc05);
	ADD_TEST(025_loopc06);
	ADD_TEST(026_loopc07);
	ADD_TEST(027_loopc08);
	ADD_TEST(028_loopc09);

	ADD_TEST(029_loopmx01);
	ADD_TEST(030_loopmx02);
	ADD_TEST(031_loopmx03);
	ADD_TEST(032_loopmx04);
	ADD_TEST(033_loopmx05);
	ADD_TEST(034_loopmx06);
	ADD_TEST(035_loopmx07);
	ADD_TEST(036_loopmx08);
	ADD_TEST(037_loopmx09);
	ADD_TEST(038_loopmx10);
	ADD_TEST(039_loopmx11);
	ADD_TEST(040_loopmx12);
	ADD_TEST(041_loopmx13);

	ADD_TEST(042_loopns01);
	ADD_TEST(043_loopns02);
	ADD_TEST(044_loopns03);
	ADD_TEST(045_loopns04);
	ADD_TEST(046_loopns05);
	ADD_TEST(047_loopns06);
	ADD_TEST(048_loopns07);
	ADD_TEST(049_loopns08);
	ADD_TEST(050_loopns09);
	ADD_TEST(051_loopns10);
	ADD_TEST(052_loopns11);
	ADD_TEST(053_loopns12);
	ADD_TEST(054_loopns13);
	ADD_TEST(055_loopns14);
	ADD_TEST(056_loopns15);

	ADD_TEST(057_loopnsmx01);
	ADD_TEST(058_loopnsmx02);
	ADD_TEST(059_loopnsmx03);
	ADD_TEST(060_loopnsmx04);
	ADD_TEST(061_loopnsmx05);
	ADD_TEST(062_loopnsmx06);
	ADD_TEST(063_loopnsmx07);
	ADD_TEST(064_loopnsmx08);
	ADD_TEST(065_loopnsmx09);
	ADD_TEST(066_loopnsmx10);
	ADD_TEST(067_loopnsmx11);
	ADD_TEST(068_loopnsmx12);
	ADD_TEST(069_loopnsmx13);
	ADD_TEST(070_loopnsmx14);
	ADD_TEST(071_loopnsmx15);
	ADD_TEST(072_loopnsmx16);

	ADD_TEST(073_call01);
	ADD_TEST(074_call02);
	ADD_TEST(075_call03);
	ADD_TEST(076_call04);
	ADD_TEST(077_call05);
	ADD_TEST(078_call06);
	ADD_TEST(079_call07);
	ADD_TEST(080_call08);
	ADD_TEST(081_call09);
	ADD_TEST(082_call10);

	ADD_TEST(083_misc);
	ADD_TEST(084_ocl_mandelbrot);
	ADD_TEST(085_noise);
#if 0
	ADD_TEST(086_ocl_aobench);
	ADD_TEST(087_ocl_aobench_inlined); // Sometimes fails due to struct/writeback issue.
#endif

#ifdef RUN_IRREDUCIBLE_TESTS
	ADD_TEST(088_irreducible1);
	ADD_TEST(089_irreducible2);
	ADD_TEST(090_irreducible3);
	ADD_TEST(091_irreducible4);
#endif
}

int
main(int argc, char** argv)
{
    bool useAVX                        = false;
    bool disableMemAccessAnalysis      = false; // Only used for the message below.
    bool disableControlFlowDivAnalysis = false; // Only used for the message below.
    bool disableAllAnalyses            = false; // Only used for the message below.
    bool verbose                       = false;
    bool error                         = false;
    bool displayUsage                  = false;

    for (int i=1; i<argc; ++i)
    {
        if (strcmp(argv[i], "-mavx") == 0)
        {
            useAVX = true;
            continue;
        }
        if (strcmp(argv[i], "-no-mem-analysis") == 0)
        {
            disableMemAccessAnalysis = true;
            continue;
        }
        if (strcmp(argv[i], "-no-cf-analysis") == 0)
        {
            disableControlFlowDivAnalysis = true;
            continue;
        }
        if (strcmp(argv[i], "-no-analyses") == 0)
        {
            disableAllAnalyses = true;
            continue;
        }
        if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "-verbose") == 0)
        {
            verbose = true;
            continue;
        }
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "-help") == 0)
        {
            displayUsage = true;
            continue;
        }
        // Ignore other flags to wfvTestSuite
        if (strcmp(argv[i], "-no-avx") == 0 ||
            strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "-dump") == 0 ||
            strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "-help") == 0 ||
            strcmp(argv[i], "-O0") == 0 || strcmp(argv[i], "-no-opt") == 0 ||
            strcmp(argv[i], "-no-verify") == 0 ||
            strcmp(argv[i], "-test1") == 0 ||
            strcmp(argv[i], "-test2") == 0 ||
            strcmp(argv[i], "-test3") == 0)
        {
            continue;
        }

        printf("ERROR: Unknown argument found: %s\n\n", argv[i]);
        displayUsage = true;
        error = true;
    }

    if (displayUsage)
    {
        printf("Usage: This test suite should only be executed through wfvTestSuite.\n");
        printf("Options:\n");
        printf("  -d          : vectorizer has divergence analysis enabled\n");
        printf("  -mavx       : vectorizer generates AVX code\n");
        printf("  -v -verbose : enable verbose output of results\n");
        printf("  -h -help    : display this help message\n\n");
        return error ? -1 : 0;
    }

    printf("\n\n\n--------------------------------------------------------------------------------\n");
    printf("Running Whole-Function Vectorization Test Suite 1 ");
    if (disableAllAnalyses)
    {
        printf("(ALL analyses DISABLED)");
    }
    else
    {
        if (disableMemAccessAnalysis && disableControlFlowDivAnalysis)
        {
            printf("(memory access analysis DISABLED, control-flow divergence analysis DISABLED)");
        }
        else if (disableMemAccessAnalysis)
        {
            printf("(memory access analysis DISABLED)");
        }
        else if (disableControlFlowDivAnalysis)
        {
            printf("(control-flow divergence analysis DISABLED)");
        }
        else
        {
            printf("(all analyses ENABLED)");
        }
    }
    printf("...\n\n");


    //------------------------------------------------------------------------//
    // create function pointers for test cases and save test case names
    //------------------------------------------------------------------------//
    TestCaseVecType testCases;

    //add scalar and generated functions
    addTestCases(testCases);

    const unsigned testCaseNr = testCases.size();

    //------------------------------------------------------------------------//
    // create input values
    //------------------------------------------------------------------------//
#ifdef USE_RANDOM_TESTS
    const unsigned inputNr           = 14 + NUM_RANDOM_INPUT_VALUE_SETS;
#else
    const unsigned inputNr           = 14;
#endif
    const unsigned inputParamNr      = 2;
    const unsigned inputPermutations = pow(inputNr, inputParamNr);

	float scalarInputs0[inputNr];
	float scalarInputs1[inputNr];
	float scalarInputs2[inputNr];
	float scalarInputs3[inputNr];

	// 14 hardcoded input value sets
	scalarInputs0[0] = 0.f;
	scalarInputs0[1] = 3.f;
	scalarInputs0[2] = 2.f;
	scalarInputs0[3] = 8.f;
	scalarInputs0[4] = 10.2f;
	scalarInputs0[5] = -1.f;
	scalarInputs0[6] = 0.f;
	scalarInputs0[7] = 1000.23f;
	scalarInputs0[8] = 0.0002f;
	scalarInputs0[9] = -0.0002f;
	scalarInputs0[10] = -3.f;
	scalarInputs0[11] = -1.f;
	scalarInputs0[12] = 0.f;
	scalarInputs0[13] = 12.f;

    scalarInputs1[0] = 1.f;
    scalarInputs1[1] = 2.f;
    scalarInputs1[2] = 4.f;
    scalarInputs1[3] = 6.f;
    scalarInputs1[4] = -14.13f;
    scalarInputs1[5] = -13.f;
    scalarInputs1[6] = 0.f;
    scalarInputs1[7] = 0.0002f;
    scalarInputs1[8] = 420.001f;
    scalarInputs1[9] = -420.001f;
    scalarInputs1[10] = 3.f;
    scalarInputs1[11] = -1.f;
    scalarInputs1[12] = 0.f;
    scalarInputs1[13] = 12.f;

    scalarInputs2[0] = 2.f;
    scalarInputs2[1] = 1.f;
    scalarInputs2[2] = 6.f;
    scalarInputs2[3] = 4.f;
    scalarInputs2[4] = 999.f;
    scalarInputs2[5] = -5.f;
    scalarInputs2[6] = 0.f;
    scalarInputs2[7] = 420.001f;
    scalarInputs2[8] = 0.01f;
    scalarInputs2[9] = 0.01f;
    scalarInputs2[10] = 3.f;
    scalarInputs2[11] = 1.f;
    scalarInputs2[12] = 333.333f;
    scalarInputs2[13] = 4.f;

    scalarInputs3[0] = 3.f;
    scalarInputs3[1] = 0.f;
    scalarInputs3[2] = 8.f;
    scalarInputs3[3] = 2.f;
    scalarInputs3[4] = 0.f;
    scalarInputs3[5] = -420.001f;
    scalarInputs3[6] = 0.f;
    scalarInputs3[7] = 0.01f;
    scalarInputs3[8] = 1000.23f;
    scalarInputs3[9] = 0.01f;
    scalarInputs3[10] = -3.f;
    scalarInputs3[11] = 1.f;
    scalarInputs3[12] = -333.333f;
    scalarInputs3[13] = -4.f;

#ifdef USE_RANDOM_TESTS
	// now add random inputs
	#define CUSTOM_RAND_MAX 1000 //prevent too large inputs
	srand((unsigned)time(0));
	for (unsigned i=0; i<inputNr-14; ++i)
	{
		float r = (float)rand()/(float)RAND_MAX;
		float neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs0[i+14] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		r = (float)rand()/(float)RAND_MAX;
		neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs1[i+14] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		r = (float)rand()/(float)RAND_MAX;
		neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs2[i+14] = (rand() % CUSTOM_RAND_MAX) * r * neg;

		r = (float)rand()/(float)RAND_MAX;
		neg = rand() > (RAND_MAX/2) ? 1.f : -1.f;
		scalarInputs3[i+14] = (rand() % CUSTOM_RAND_MAX) * r * neg;
	}
#endif

    //------------------------------------------------------------------------//
    // create result arrays for generated functions
    //------------------------------------------------------------------------//
    const unsigned maxResultNr = inputPermutations * testCaseNr;
    std::vector<V*>* results = new std::vector<V*>(maxResultNr); //only aligned if on heap

    const bool printAllResults = false;

    //------------------------------------------------------------------------//
    // create result arrays for scalar functions
    //------------------------------------------------------------------------//
	typedef std::pair<const char*, std::pair<const char*, std::vector<float>* > > ScalarResult;
    ScalarResult* scalarResults = new ScalarResult[maxResultNr]();

    //------------------------------------------------------------------------//
    // create timer and data structures that hold execution times
    //------------------------------------------------------------------------//
    //Packetizer::Timer timer;
    std::vector<double> executionTimesScalar;
    std::vector<double> executionTimesPacketized;

    //------------------------------------------------------------------------//
    // compute results of scalar and generated functions
    //------------------------------------------------------------------------//
    unsigned testsRun = 0;

    for (unsigned TC=0; TC<testCaseNr; ++TC)
	{
        unsigned inputPermsRun = 0;
        for (unsigned i=0; i<inputNr; ++i)
		{
            for (unsigned j=0; j<inputNr; ++j)
			{
                // abort if we have already run too many test cases
                if (testsRun >= maxResultNr)
				{
                    printf("\nERROR: not enough space allocated for results!\n");
                    return -1;
                }

                // get function pointers of current test case
                TestCaseType testCase = testCases[TC];

                // get input values
                const float input0i = scalarInputs0[i];
                const float input1i = scalarInputs1[i];
                const float input2i = scalarInputs2[i];
                const float input3i = scalarInputs3[i];
                const float input0j = scalarInputs0[j];
                const float input1j = scalarInputs1[j];
                const float input2j = scalarInputs2[j];
                const float input3j = scalarInputs3[j];

                // generate info-string for this execution
				char* inputString = new char[256]();
				sprintf(inputString, "[ [ %f %f %f %f ] | [ %f %f %f %f ] ]",
					input0i, input1i, input2i, input3i, input0j, input1j, input2j, input3j);

				//printf("input: %s\n", inputString);

                // execute scalar function
                std::vector<float>* resS = new std::vector<float>();
                //timer.startTimer();
                const float resS0 = testCase.second.first(input0i, input0j);
                const float resS1 = testCase.second.first(input1i, input1j);
                const float resS2 = testCase.second.first(input2i, input2j);
                const float resS3 = testCase.second.first(input3i, input3j);
                //timer.stopTimer();
                //executionTimesScalar.push_back(timer.getTime());

                // store result of scalar function
                resS->push_back(resS0);
                resS->push_back(resS1);
                resS->push_back(resS2);
                resS->push_back(resS3);
                scalarResults[testsRun] = std::make_pair(inputString, std::make_pair(testCase.first, resS));


                // generate inputs for packetized function
                V* ax = (V*)aligned_malloc(sizeof(V), 16);
                V* bx = (V*)aligned_malloc(sizeof(V), 16);
                ax->data = _mm_set_ps(scalarInputs3[i], scalarInputs2[i], scalarInputs1[i], scalarInputs0[i]);
                bx->data = _mm_set_ps(scalarInputs3[j], scalarInputs2[j], scalarInputs1[j], scalarInputs0[j]);

                // execute generated function
                V* r = (V*)aligned_malloc(sizeof(V), 16);
                //timer.startTimer();
                r->data = testCase.second.second(ax->data, bx->data);
                //timer.stopTimer();
                //executionTimesPacketized.push_back(timer.getTime());

                // store result of generated function
                (*results)[testsRun] = r;

                ++testsRun;
                ++inputPermsRun;
#if 0
#define CLEAR "\033[K"
                printf("\rProgress: %6.2f%% | test %2d/%2d (inputs %3d/%3d) : %s" CLEAR,
					   ((float)testsRun * 100.f) / (float)maxResultNr,
					   TC+1,
					   testCaseNr,
					   inputPermsRun,
					   inputPermutations,
					   testCase.first);
				fflush(stdout);
#endif
            }
        }
    }

    if (testsRun <= 0)
	{
        printf("\nERROR: need to compute at least one result! (forgot to activate test cases?)\n");
        return -1;
    }
    if (testsRun != maxResultNr)
	{
        printf("\nERROR: unexpected number of results  (testsRun != maxResultNr)!\n");
        return -1;
    }


    if (printAllResults)
	{
        for (unsigned i=0; i<testsRun; ++i)
		{
            const char* testCaseName = scalarResults[i].second.first;
			const char* inputValues  = scalarResults[i].first;
            printf("%s %s (scalar)     = [ ", testCaseName, inputValues);
            printf("%f %f %f %f ",
				   (*scalarResults[i].second.second)[0],
				   (*scalarResults[i].second.second)[1],
				   (*scalarResults[i].second.second)[2],
				   (*scalarResults[i].second.second)[3]);
            printf("]\n");

            printf("%s %s (packetized) = [ ", testCaseName, inputValues);
            printf("%f %f %f %f ",
				   get((*results)[i], 0),
				   get((*results)[i], 1),
				   get((*results)[i], 2),
				   get((*results)[i], 3));
            printf("]\n");
        }
    }

    printf("\nVerifying results...\n\n");

    bool allSuccessful = true;
    unsigned failedTestsNr = 0;
    for (unsigned i=0; i<testsRun; ++i)
	{
		const float scalarRes0 = (*scalarResults[i].second.second)[0];
		const float scalarRes1 = (*scalarResults[i].second.second)[1];
		const float scalarRes2 = (*scalarResults[i].second.second)[2];
		const float scalarRes3 = (*scalarResults[i].second.second)[3];
		const float pktRes0 = get((*results)[i], 0);
		const float pktRes1 = get((*results)[i], 1);
		const float pktRes2 = get((*results)[i], 2);
		const float pktRes3 = get((*results)[i], 3);

		const bool success =
			resultMatches(scalarRes0, pktRes0) &&
			resultMatches(scalarRes1, pktRes1) &&
			resultMatches(scalarRes2, pktRes2) &&
			resultMatches(scalarRes3, pktRes3);

        if (printAllResults)
		{
            printf("%s ", scalarResults[i].second.first);
            success ? printf(" SUCCESSFUL!") : printf(" FAILED!     ");
			printf(" %s\n", scalarResults[i].first);
        }
		else if (!success)
		{
            printf("%s FAILED! ", scalarResults[i].second.first);
			printf(" %s\n", scalarResults[i].first); //input-values
            printf("  expected result: [ %f %f %f %f ]\n",
				   scalarRes0,
				   scalarRes1,
				   scalarRes2,
				   scalarRes3);
            printf("  computed result: [ %f %f %f %f ]\n",
				   pktRes0,
				   pktRes1,
				   pktRes2,
				   pktRes3);
        }

        allSuccessful &= success;
        if (!success) ++failedTestsNr;
    }

    if (allSuccessful) printf("ALL TESTS SUCCESSFUL! (%d)", testsRun);
	else printf("\n%d / %d TESTS FAILED!", failedTestsNr, testsRun);

	//printf("\ninput values:\n");
	//for (unsigned i=0; i<inputNr; ++i) {
		//printf("%f %f %f %f\n", scalarInputs0[i], scalarInputs1[i], scalarInputs2[i], scalarInputs3[i]);
	//}

    printf("\n\ntest-suite run complete!\n");
    printf("--------------------------------------------------------------------------------\n\n");

    return !allSuccessful;
}
