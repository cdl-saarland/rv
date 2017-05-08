#include <immintrin.h>
#include <stdint.h>

inline double as_double(int64_t x) {
    union { int64_t i; double d; } u = { .i = x };
    return u.d;
}

#ifdef ENABLE_AVX
__m256d xfabs(__m256d x) {
    return _mm256_and_pd(x, _mm256_set1_pd(as_double(0x7FFFFFFFFFFFFFFFll)));
}
__m256d xcopysign(__m256d x, __m256d y) {
    __m256d s = _mm256_and_pd(y, _mm256_set1_pd(as_double(0x8000000000000000)));
    return _mm256_or_pd(_mm256_and_pd(x, _mm256_set1_pd(as_double(0x7FFFFFFFFFFFFFFF))), s);
}
__m256d xfmin(__m256d x, __m256d y) {
    return _mm256_min_pd(x, y);
}
__m256d xfmax(__m256d x, __m256d y) {
    return _mm256_max_pd(x, y);
}
#endif

#ifdef ENABLE_SSE41
__m128d xfabs(__m128d x) {
    return _mm_and_pd(x, _mm_set1_pd(as_double(0x7FFFFFFFFFFFFFFFll)));
}
__m128d xcopysign(__m128d x, __m128d y) {
    __m128d s = _mm_and_pd(y, _mm_set1_pd(as_double(0x8000000000000000)));
    return _mm_or_pd(_mm_and_pd(x, _mm_set1_pd(as_double(0x7FFFFFFFFFFFFFFF))), s);
}
__m128d xfmin(__m128d x, __m128d y) {
    return _mm_min_pd(x, y);
}
__m128d xfmax(__m128d x, __m128d y) {
    return _mm_max_pd(x, y);
}
#endif
