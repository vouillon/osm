#include <stdint.h>

static uint64_t dilate (uint32_t x0) {
  uint64_t x = x0;
  x = (x | (x << 16)) & 0x0000FFFF0000FFFF;
  x = (x | (x << 8)) &  0x00FF00FF00FF00FF;
  x = (x | (x << 4)) &  0x0F0F0F0F0F0F0F0F;
  x = (x | (x << 2)) &  0x3333333333333333;
  x = (x | (x << 1)) &  0x5555555555555555;
  return x;
}

/* Encoding and decoding the Hilbert order, Xian Lu and Gunther Schrack */

uint64_t hilbert_coordinate_1 (uint32_t x, uint32_t y) {
  uint32_t heven = x ^ y;
  uint32_t notx = ~ x;
  uint32_t noty = ~ y;
  uint32_t temp = notx ^ y;
  uint32_t v1 = 0;
  uint32_t v0 = 0;
  int k;
  for (k = 1; k < 32; k++) {
    v1 = ((v1 & heven) | ((v0 ^ noty) & temp)) >> 1;
    v0 = ((v0 & (v1 ^ notx)) | ((~v0) & (v1 ^ noty))) >> 1;
  }
  uint32_t hodd = ((~ v0) & (v1 ^ x)) | (v0 & (v1 ^ noty));
  return (dilate(hodd) << 1) | dilate(heven);
}

static inline void swap (uint32_t * x, uint32_t * y) {
  uint32_t temp;
  temp = *x;
  *x = *y;
  *y = temp;
}

/* This one is slower, due to conditionals... */
/* A new algorithm for encoding and decoding the Hilbert order,
   Ningtao Chen, Nengchao Wang and Baochang Shi */

uint64_t hilbert_coordinate_2 (uint32_t x, uint32_t y) {
  uint32_t w = 1 << 31;
  uint64_t z = 0;

  do {
    z = z << 2;
    if (y & w) {
      if (x & w) {
        /* Quadrant 2 */
        z |= 2;
      } else {
        /* Quadrant 1 */
        z |= 1;
      }
    } else {
      if (x & w) {
        /* Quadrant 3 */
        x = ~x;
        y = ~y;
        z |= 3;
      } else {
        /* Quadrant 0 */
      }
      swap (&x, &y);
    }
    w = w >> 1;
  } while (w > 0);

  return z;
}

#ifdef __SSE2__

#include <emmintrin.h>

__m128i static dilate_sse (__m128i x) {
  x = _mm_and_si128(_mm_or_si128(x, _mm_slli_epi64 (x, 16)),
                    _mm_set1_epi32 (0x0000FFFF));
  x = _mm_and_si128(_mm_or_si128(x, _mm_slli_epi32 (x, 8)),
                    _mm_set1_epi16 (0x00FF));
  x = _mm_and_si128(_mm_or_si128(x, _mm_slli_epi16 (x, 4)),
                    _mm_set1_epi8 (0x0F));
  x = _mm_and_si128(_mm_or_si128(x, _mm_slli_epi16 (x, 2)),
                    _mm_set1_epi8 (0x33));
  x = _mm_and_si128(_mm_or_si128(x, _mm_slli_epi16 (x, 1)),
                    _mm_set1_epi8 (0x55));
  return x;
}


void hilbert_coordinate_sse (uint32_t * xs, uint32_t * ys, uint64_t * zs) {
  __m128i zero = _mm_setzero_si128 ();
  __m128i ones = _mm_cmpeq_epi8 (zero, zero);
 
  __m128i x = _mm_loadu_si128 ((__m128i *) xs);
  __m128i y = _mm_loadu_si128 ((__m128i *) ys);

  __m128i heven = _mm_xor_si128 (x, y);
  __m128i notx = _mm_xor_si128 (x, ones);
  __m128i noty = _mm_xor_si128 (y, ones);
  __m128i temp = _mm_xor_si128 (notx, y);
  __m128i v1 = zero;
  __m128i v0 = zero;
  int k;
  // XXX Should we perform several computations in parallel?
  for (k = 1; k < 32; k++) {
    v1 = _mm_srli_epi32 (_mm_or_si128 (_mm_and_si128 (v1, heven),
                                       _mm_and_si128 (_mm_xor_si128 (v0, noty),
                                                      temp)),
                         1);
    v0 = _mm_srli_epi32 (_mm_or_si128 (_mm_and_si128 (v0,
                                                      _mm_xor_si128 (v1,
                                                                     notx)),
                                       _mm_and_si128 (_mm_xor_si128 (v0,
                                                                     ones),
                                                      _mm_xor_si128 (v1,
                                                                     noty))),
                         1);
  }
  __m128i hodd = _mm_or_si128 (_mm_and_si128 (_mm_xor_si128 (v0, ones),
                                              _mm_xor_si128 (v1, x)),
                               _mm_and_si128 (v0,
                                              _mm_xor_si128 (v1, noty)));
  // XXX Use _mm_unpackhi_epi8?
  __m128i heven2 = _mm_unpackhi_epi32 (heven, zero);
  __m128i heven1 = _mm_unpacklo_epi32 (heven, zero);
  __m128i hodd2 = _mm_unpackhi_epi32 (hodd, zero);
  __m128i hodd1 = _mm_unpacklo_epi32 (hodd, zero);

  __m128i z1 = _mm_or_si128(_mm_slli_epi16(dilate_sse (hodd1), 1),
                            dilate_sse (heven1));
  __m128i z2 = _mm_or_si128(_mm_slli_epi16(dilate_sse (hodd2), 1),
                            dilate_sse (heven2));

  _mm_storeu_si128((__m128i *) zs, z1);
  _mm_storeu_si128((__m128i *) (zs + 2), z2);
}

#endif

/*
0 0 0 0
0 1 3 3
0 2 4 4
0 3 5 5
1 0 1 1
1 1 2 2
1 2 7 7
1 3 6 6
2 0 14 14
2 1 13 13
2 2 8 8
2 3 9 9
3 0 15 15
3 1 12 12
3 2 11 11
3 3 10 10
 */

#include <stdio.h>

int main (void) {
  /*
  int i, j;
  for (i = 0; i < 4; i++) {
    for (j = 0; j < 4; j++) {
      printf ("%d %d %ld %ld\n", i, j,
              hilbert_coordinate_1(i,j), hilbert_coordinate_2(i,j));
    }
  }
  */

#if 0
  uint32_t x = 1;
  uint32_t y = 1;
  uint64_t accu = 0;
  long i;
  for (i = 0; i < 30000000; i++) {
    x *= 2234039081;
    y *= 269614307;
    accu += hilbert_coordinate_1(x, y);
  }
  printf ("%ld\n", accu);

#else

  uint32_t xs[4];
  uint32_t ys[4];
  uint64_t zs[4];

  uint32_t x = 1;
  uint32_t y = 1;
  uint64_t accu = 0;
  long i;
  for (i = 0; i < 30000000 / 4; i++) {
    x *= 2234039081;
    y *= 269614307;
    xs[0] = x;
    ys[0] = y;
    x *= 2234039081;
    y *= 269614307;
    xs[1] = x;
    ys[1] = y;
    x *= 2234039081;
    y *= 269614307;
    xs[2] = x;
    ys[2] = y;
    x *= 2234039081;
    y *= 269614307;
    xs[3] = x;
    ys[3] = y;
    hilbert_coordinate_sse(xs, ys, zs);
    accu = accu + zs[0] + zs[1] + zs[2] + zs[3];
  }
  printf ("%ld\n", accu);

#endif

  /*
  hilbert_coordinate_sse(x, y, z);

  printf ("%ld %ld %ld %ld\n", z[0], z[1], z[2], z[3]);

  int i;
  for (i = 0; i < 4; i++)
    z[i] = hilbert_coordinate_1 (x[i], y[i]);
  printf ("%ld %ld %ld %ld\n", z[0], z[1], z[2], z[3]);
  */

  return 0;
}

