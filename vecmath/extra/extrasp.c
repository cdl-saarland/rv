#include <immintrin.h>
#include <stdint.h>

inline float as_float(int x) {
    union { int i; float f; } u = { .i = x };
    return u.f;
}

#ifdef ENABLE_AVX
__m256 xfabsf(__m256 x) {
    return _mm256_and_ps(x, _mm256_set1_ps(as_float(0x7FFFFFFF)));
}
__m256 xcopysignf(__m256 x, __m256 y) {
    __m256 s = _mm256_and_ps(y, _mm256_set1_ps(as_float(0x80000000)));
    return _mm256_or_ps(_mm256_and_ps(x, _mm256_set1_ps(as_float(0x7FFFFFFF))), s);
}
__m256 xfminf(__m256 x, __m256 y) {
    return _mm256_min_ps(x, y);
}
__m256 xfmaxf(__m256 x, __m256 y) {
    return _mm256_max_ps(x, y);
}
#endif

#ifdef ENABLE_SSE41
__m128 xfabsf(__m128 x) {
    return _mm_and_ps(x, _mm_set1_ps(as_float(0x7FFFFFFF)));
}
__m128 xcopysignf(__m128 x, __m128 y) {
    __m128 s = _mm_and_ps(y, _mm_set1_ps(as_float(0x80000000)));
    return _mm_or_ps(_mm_and_ps(x, _mm_set1_ps(as_float(0x7FFFFFFF))), s);
}
__m128 xfminf(__m128 x, __m128 y) {
    return _mm_min_ps(x, y);
}
__m128 xfmaxf(__m128 x, __m128 y) {
    return _mm_max_ps(x, y);
}
#endif
