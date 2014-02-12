#include <stdio.h>
#include <stdint.h>

/* http://graphics.stanford.edu/~seander/bithacks.html#InterleaveBMN */

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
    uint32_t v2 = ((v1 & heven) | ((v0 ^ noty) & temp)) >> 1;
    v0 = ((v0 & (v1 ^ notx)) | ((~v0) & (v1 ^ noty))) >> 1;
    v1 = v2;
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

__m128i static dilate_sse_8 (__m128i x) {
  /*
  x = _mm_and_si128(_mm_or_si128(x, _mm_slli_epi64 (x, 16)),
                    _mm_set1_epi32 (0x0000FFFF));
  x = _mm_and_si128(_mm_or_si128(x, _mm_slli_epi32 (x, 8)),
                    _mm_set1_epi16 (0x00FF));
  */
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
  __m128i v2;
  __m128i v1 = zero;
  __m128i v0 = zero;
  int k;
  // XXX Should we perform several computations in parallel?
  for (k = 1; k < 32; k++) {
    v2 = _mm_srli_epi32 (_mm_or_si128 (_mm_and_si128 (v1, heven),
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
    v1 = v2;
  }
  __m128i hodd = _mm_or_si128 (_mm_and_si128 (_mm_xor_si128 (v0, ones),
                                              _mm_xor_si128 (v1, x)),
                               _mm_and_si128 (v0,
                                              _mm_xor_si128 (v1, noty)));
  __m128i heven2 = _mm_unpackhi_epi8 (heven, zero);
  __m128i heven1 = _mm_unpacklo_epi8 (heven, zero);
  __m128i hodd2 = _mm_unpackhi_epi8 (hodd, zero);
  __m128i hodd1 = _mm_unpacklo_epi8 (hodd, zero);

  __m128i z1 = _mm_or_si128(_mm_slli_epi16(dilate_sse_8 (hodd1), 1),
                            dilate_sse_8 (heven1));
  __m128i z2 = _mm_or_si128(_mm_slli_epi16(dilate_sse_8 (hodd2), 1),
                            dilate_sse_8 (heven2));

  _mm_storeu_si128((__m128i *) zs, z1);
  _mm_storeu_si128((__m128i *) (zs + 2), z2);

  /* Perform 8 computations at a time:
     compute higher bytes; save them, next bytes, ... */
}

#endif

uint8_t bytes [1024] =
  {0x00, 0x03, 0x04, 0x05, 0x3a, 0x3b, 0x3c, 0x3f,
   0x40, 0x41, 0x4e, 0x4f, 0x50, 0x53, 0x54, 0x55,
   0x01, 0x02, 0x07, 0x06, 0x39, 0x38, 0x3d, 0x3e,
   0x43, 0x42, 0x4d, 0x4c, 0x51, 0x52, 0x57, 0x56,
   0x0e, 0x0d, 0x08, 0x09, 0x36, 0x37, 0x32, 0x31,
   0x44, 0x47, 0x48, 0x4b, 0x5e, 0x5d, 0x58, 0x59,
   0x0f, 0x0c, 0x0b, 0x0a, 0x35, 0x34, 0x33, 0x30,
   0x45, 0x46, 0x49, 0x4a, 0x5f, 0x5c, 0x5b, 0x5a,
   0x10, 0x11, 0x1e, 0x1f, 0x20, 0x21, 0x2e, 0x2f,
   0x7a, 0x79, 0x76, 0x75, 0x60, 0x63, 0x64, 0x65,
   0x13, 0x12, 0x1d, 0x1c, 0x23, 0x22, 0x2d, 0x2c,
   0x7b, 0x78, 0x77, 0x74, 0x61, 0x62, 0x67, 0x66,
   0x14, 0x17, 0x18, 0x1b, 0x24, 0x27, 0x28, 0x2b,
   0x7c, 0x7d, 0x72, 0x73, 0x6e, 0x6d, 0x68, 0x69,
   0x15, 0x16, 0x19, 0x1a, 0x25, 0x26, 0x29, 0x2a,
   0x7f, 0x7e, 0x71, 0x70, 0x6f, 0x6c, 0x6b, 0x6a,
   0xea, 0xe9, 0xe6, 0xe5, 0xda, 0xd9, 0xd6, 0xd5,
   0x80, 0x81, 0x8e, 0x8f, 0x90, 0x93, 0x94, 0x95,
   0xeb, 0xe8, 0xe7, 0xe4, 0xdb, 0xd8, 0xd7, 0xd4,
   0x83, 0x82, 0x8d, 0x8c, 0x91, 0x92, 0x97, 0x96,
   0xec, 0xed, 0xe2, 0xe3, 0xdc, 0xdd, 0xd2, 0xd3,
   0x84, 0x87, 0x88, 0x8b, 0x9e, 0x9d, 0x98, 0x99,
   0xef, 0xee, 0xe1, 0xe0, 0xdf, 0xde, 0xd1, 0xd0,
   0x85, 0x86, 0x89, 0x8a, 0x9f, 0x9c, 0x9b, 0x9a,
   0xf0, 0xf3, 0xf4, 0xf5, 0xca, 0xcb, 0xcc, 0xcf,
   0xba, 0xb9, 0xb6, 0xb5, 0xa0, 0xa3, 0xa4, 0xa5,
   0xf1, 0xf2, 0xf7, 0xf6, 0xc9, 0xc8, 0xcd, 0xce,
   0xbb, 0xb8, 0xb7, 0xb4, 0xa1, 0xa2, 0xa7, 0xa6,
   0xfe, 0xfd, 0xf8, 0xf9, 0xc6, 0xc7, 0xc2, 0xc1,
   0xbc, 0xbd, 0xb2, 0xb3, 0xae, 0xad, 0xa8, 0xa9,
   0xff, 0xfc, 0xfb, 0xfa, 0xc5, 0xc4, 0xc3, 0xc0,
   0xbf, 0xbe, 0xb1, 0xb0, 0xaf, 0xac, 0xab, 0xaa,
   0x00, 0x01, 0x0e, 0x0f, 0x10, 0x13, 0x14, 0x15,
   0xea, 0xeb, 0xec, 0xef, 0xf0, 0xf1, 0xfe, 0xff,
   0x03, 0x02, 0x0d, 0x0c, 0x11, 0x12, 0x17, 0x16,
   0xe9, 0xe8, 0xed, 0xee, 0xf3, 0xf2, 0xfd, 0xfc,
   0x04, 0x07, 0x08, 0x0b, 0x1e, 0x1d, 0x18, 0x19,
   0xe6, 0xe7, 0xe2, 0xe1, 0xf4, 0xf7, 0xf8, 0xfb,
   0x05, 0x06, 0x09, 0x0a, 0x1f, 0x1c, 0x1b, 0x1a,
   0xe5, 0xe4, 0xe3, 0xe0, 0xf5, 0xf6, 0xf9, 0xfa,
   0x3a, 0x39, 0x36, 0x35, 0x20, 0x23, 0x24, 0x25,
   0xda, 0xdb, 0xdc, 0xdf, 0xca, 0xc9, 0xc6, 0xc5,
   0x3b, 0x38, 0x37, 0x34, 0x21, 0x22, 0x27, 0x26,
   0xd9, 0xd8, 0xdd, 0xde, 0xcb, 0xc8, 0xc7, 0xc4,
   0x3c, 0x3d, 0x32, 0x33, 0x2e, 0x2d, 0x28, 0x29,
   0xd6, 0xd7, 0xd2, 0xd1, 0xcc, 0xcd, 0xc2, 0xc3,
   0x3f, 0x3e, 0x31, 0x30, 0x2f, 0x2c, 0x2b, 0x2a,
   0xd5, 0xd4, 0xd3, 0xd0, 0xcf, 0xce, 0xc1, 0xc0,
   0x40, 0x43, 0x44, 0x45, 0x7a, 0x7b, 0x7c, 0x7f,
   0x80, 0x83, 0x84, 0x85, 0xba, 0xbb, 0xbc, 0xbf,
   0x41, 0x42, 0x47, 0x46, 0x79, 0x78, 0x7d, 0x7e,
   0x81, 0x82, 0x87, 0x86, 0xb9, 0xb8, 0xbd, 0xbe,
   0x4e, 0x4d, 0x48, 0x49, 0x76, 0x77, 0x72, 0x71,
   0x8e, 0x8d, 0x88, 0x89, 0xb6, 0xb7, 0xb2, 0xb1,
   0x4f, 0x4c, 0x4b, 0x4a, 0x75, 0x74, 0x73, 0x70,
   0x8f, 0x8c, 0x8b, 0x8a, 0xb5, 0xb4, 0xb3, 0xb0,
   0x50, 0x51, 0x5e, 0x5f, 0x60, 0x61, 0x6e, 0x6f,
   0x90, 0x91, 0x9e, 0x9f, 0xa0, 0xa1, 0xae, 0xaf,
   0x53, 0x52, 0x5d, 0x5c, 0x63, 0x62, 0x6d, 0x6c,
   0x93, 0x92, 0x9d, 0x9c, 0xa3, 0xa2, 0xad, 0xac,
   0x54, 0x57, 0x58, 0x5b, 0x64, 0x67, 0x68, 0x6b,
   0x94, 0x97, 0x98, 0x9b, 0xa4, 0xa7, 0xa8, 0xab,
   0x55, 0x56, 0x59, 0x5a, 0x65, 0x66, 0x69, 0x6a,
   0x95, 0x96, 0x99, 0x9a, 0xa5, 0xa6, 0xa9, 0xaa,
   0xaa, 0xab, 0xac, 0xaf, 0xb0, 0xb1, 0xbe, 0xbf,
   0xc0, 0xc3, 0xc4, 0xc5, 0xfa, 0xfb, 0xfc, 0xff,
   0xa9, 0xa8, 0xad, 0xae, 0xb3, 0xb2, 0xbd, 0xbc,
   0xc1, 0xc2, 0xc7, 0xc6, 0xf9, 0xf8, 0xfd, 0xfe,
   0xa6, 0xa7, 0xa2, 0xa1, 0xb4, 0xb7, 0xb8, 0xbb,
   0xce, 0xcd, 0xc8, 0xc9, 0xf6, 0xf7, 0xf2, 0xf1,
   0xa5, 0xa4, 0xa3, 0xa0, 0xb5, 0xb6, 0xb9, 0xba,
   0xcf, 0xcc, 0xcb, 0xca, 0xf5, 0xf4, 0xf3, 0xf0,
   0x9a, 0x9b, 0x9c, 0x9f, 0x8a, 0x89, 0x86, 0x85,
   0xd0, 0xd1, 0xde, 0xdf, 0xe0, 0xe1, 0xee, 0xef,
   0x99, 0x98, 0x9d, 0x9e, 0x8b, 0x88, 0x87, 0x84,
   0xd3, 0xd2, 0xdd, 0xdc, 0xe3, 0xe2, 0xed, 0xec,
   0x96, 0x97, 0x92, 0x91, 0x8c, 0x8d, 0x82, 0x83,
   0xd4, 0xd7, 0xd8, 0xdb, 0xe4, 0xe7, 0xe8, 0xeb,
   0x95, 0x94, 0x93, 0x90, 0x8f, 0x8e, 0x81, 0x80,
   0xd5, 0xd6, 0xd9, 0xda, 0xe5, 0xe6, 0xe9, 0xea,
   0x6a, 0x6b, 0x6c, 0x6f, 0x70, 0x71, 0x7e, 0x7f,
   0x2a, 0x29, 0x26, 0x25, 0x1a, 0x19, 0x16, 0x15,
   0x69, 0x68, 0x6d, 0x6e, 0x73, 0x72, 0x7d, 0x7c,
   0x2b, 0x28, 0x27, 0x24, 0x1b, 0x18, 0x17, 0x14,
   0x66, 0x67, 0x62, 0x61, 0x74, 0x77, 0x78, 0x7b,
   0x2c, 0x2d, 0x22, 0x23, 0x1c, 0x1d, 0x12, 0x13,
   0x65, 0x64, 0x63, 0x60, 0x75, 0x76, 0x79, 0x7a,
   0x2f, 0x2e, 0x21, 0x20, 0x1f, 0x1e, 0x11, 0x10,
   0x5a, 0x5b, 0x5c, 0x5f, 0x4a, 0x49, 0x46, 0x45,
   0x30, 0x33, 0x34, 0x35, 0x0a, 0x0b, 0x0c, 0x0f,
   0x59, 0x58, 0x5d, 0x5e, 0x4b, 0x48, 0x47, 0x44,
   0x31, 0x32, 0x37, 0x36, 0x09, 0x08, 0x0d, 0x0e,
   0x56, 0x57, 0x52, 0x51, 0x4c, 0x4d, 0x42, 0x43,
   0x3e, 0x3d, 0x38, 0x39, 0x06, 0x07, 0x02, 0x01,
   0x55, 0x54, 0x53, 0x50, 0x4f, 0x4e, 0x41, 0x40,
   0x3f, 0x3c, 0x3b, 0x3a, 0x05, 0x04, 0x03, 0x00,
   0xaa, 0xa9, 0xa6, 0xa5, 0x9a, 0x99, 0x96, 0x95,
   0x6a, 0x69, 0x66, 0x65, 0x5a, 0x59, 0x56, 0x55,
   0xab, 0xa8, 0xa7, 0xa4, 0x9b, 0x98, 0x97, 0x94,
   0x6b, 0x68, 0x67, 0x64, 0x5b, 0x58, 0x57, 0x54,
   0xac, 0xad, 0xa2, 0xa3, 0x9c, 0x9d, 0x92, 0x93,
   0x6c, 0x6d, 0x62, 0x63, 0x5c, 0x5d, 0x52, 0x53,
   0xaf, 0xae, 0xa1, 0xa0, 0x9f, 0x9e, 0x91, 0x90,
   0x6f, 0x6e, 0x61, 0x60, 0x5f, 0x5e, 0x51, 0x50,
   0xb0, 0xb3, 0xb4, 0xb5, 0x8a, 0x8b, 0x8c, 0x8f,
   0x70, 0x73, 0x74, 0x75, 0x4a, 0x4b, 0x4c, 0x4f,
   0xb1, 0xb2, 0xb7, 0xb6, 0x89, 0x88, 0x8d, 0x8e,
   0x71, 0x72, 0x77, 0x76, 0x49, 0x48, 0x4d, 0x4e,
   0xbe, 0xbd, 0xb8, 0xb9, 0x86, 0x87, 0x82, 0x81,
   0x7e, 0x7d, 0x78, 0x79, 0x46, 0x47, 0x42, 0x41,
   0xbf, 0xbc, 0xbb, 0xba, 0x85, 0x84, 0x83, 0x80,
   0x7f, 0x7c, 0x7b, 0x7a, 0x45, 0x44, 0x43, 0x40,
   0xc0, 0xc1, 0xce, 0xcf, 0xd0, 0xd3, 0xd4, 0xd5,
   0x2a, 0x2b, 0x2c, 0x2f, 0x30, 0x31, 0x3e, 0x3f,
   0xc3, 0xc2, 0xcd, 0xcc, 0xd1, 0xd2, 0xd7, 0xd6,
   0x29, 0x28, 0x2d, 0x2e, 0x33, 0x32, 0x3d, 0x3c,
   0xc4, 0xc7, 0xc8, 0xcb, 0xde, 0xdd, 0xd8, 0xd9,
   0x26, 0x27, 0x22, 0x21, 0x34, 0x37, 0x38, 0x3b,
   0xc5, 0xc6, 0xc9, 0xca, 0xdf, 0xdc, 0xdb, 0xda,
   0x25, 0x24, 0x23, 0x20, 0x35, 0x36, 0x39, 0x3a,
   0xfa, 0xf9, 0xf6, 0xf5, 0xe0, 0xe3, 0xe4, 0xe5,
   0x1a, 0x1b, 0x1c, 0x1f, 0x0a, 0x09, 0x06, 0x05,
   0xfb, 0xf8, 0xf7, 0xf4, 0xe1, 0xe2, 0xe7, 0xe6,
   0x19, 0x18, 0x1d, 0x1e, 0x0b, 0x08, 0x07, 0x04,
   0xfc, 0xfd, 0xf2, 0xf3, 0xee, 0xed, 0xe8, 0xe9,
   0x16, 0x17, 0x12, 0x11, 0x0c, 0x0d, 0x02, 0x03,
   0xff, 0xfe, 0xf1, 0xf0, 0xef, 0xec, 0xeb, 0xea,
   0x15, 0x14, 0x13, 0x10, 0x0f, 0x0e, 0x01, 0x00};

uint8_t quadrant [1024] =
 {0,2,1,0,2,1,0,2,1,0,2,1,0,2,1,0,1,1,3,0,2,3,1,1,3,0,2,3,1,1,3,0,
  3,3,1,0,2,1,3,3,0,2,0,2,3,3,1,0,0,2,3,0,2,3,0,2,1,1,1,1,0,2,3,0,
  1,0,2,1,1,0,2,1,3,3,3,3,0,2,1,0,3,0,2,3,3,0,2,3,0,2,0,2,1,1,3,0,
  0,2,0,2,0,2,0,2,1,0,2,1,3,3,1,0,1,1,1,1,1,1,1,1,3,0,2,3,0,2,3,0,
  3,3,3,3,3,3,3,3,1,0,2,1,0,2,1,0,0,2,0,2,0,2,0,2,3,0,2,3,1,1,3,0,
  1,0,2,1,1,0,2,1,0,2,0,2,3,3,1,0,3,0,2,3,3,0,2,3,1,1,1,1,0,2,3,0,
  0,2,1,0,2,1,0,2,3,3,3,3,0,2,1,0,1,1,3,0,2,3,1,1,0,2,0,2,1,1,3,0,
  3,3,1,0,2,1,3,3,1,0,2,1,3,3,1,0,0,2,3,0,2,3,0,2,3,0,2,3,0,2,3,0,
  1,0,2,1,0,2,1,0,2,1,0,2,1,0,2,1,3,0,2,3,1,1,3,0,2,3,1,1,3,0,2,3,
  0,2,0,2,3,3,1,0,2,1,3,3,0,2,0,2,1,1,1,1,0,2,3,0,2,3,0,2,1,1,1,1,
  3,3,3,3,0,2,1,0,2,1,0,2,3,3,3,3,0,2,0,2,1,1,3,0,2,3,1,1,0,2,0,2,
  1,0,2,1,3,3,1,0,2,1,3,3,1,0,2,1,3,0,2,3,0,2,3,0,2,3,0,2,3,0,2,3,
  0,2,1,0,2,1,0,2,0,2,1,0,2,1,0,2,1,1,3,0,2,3,1,1,1,1,3,0,2,3,1,1,
  3,3,1,0,2,1,3,3,3,3,1,0,2,1,3,3,0,2,3,0,2,3,0,2,0,2,3,0,2,3,0,2,
  1,0,2,1,1,0,2,1,1,0,2,1,1,0,2,1,3,0,2,3,3,0,2,3,3,0,2,3,3,0,2,3,
  0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,1,0,2,1,0,2,1,0,2,1,0,2,1,0,2,2,3,1,1,3,0,2,3,1,1,3,0,2,3,1,1,
  2,1,3,3,0,2,0,2,3,3,1,0,2,1,3,3,2,3,0,2,1,1,1,1,0,2,3,0,2,3,0,2,
  2,1,0,2,3,3,3,3,1,0,2,1,1,0,2,1,2,3,1,1,0,2,0,2,3,0,2,3,3,0,2,3,
  2,1,3,3,1,0,2,1,0,2,0,2,0,2,0,2,2,3,0,2,3,0,2,3,1,1,1,1,1,1,1,1,
  2,1,0,2,1,0,2,1,3,3,3,3,3,3,3,3,2,3,1,1,3,0,2,3,0,2,0,2,0,2,0,2,
  2,1,3,3,0,2,0,2,1,0,2,1,1,0,2,1,2,3,0,2,1,1,1,1,3,0,2,3,3,0,2,3,
  2,1,0,2,3,3,3,3,0,2,1,0,2,1,0,2,2,3,1,1,0,2,0,2,1,1,3,0,2,3,1,1,
  2,1,3,3,1,0,2,1,3,3,1,0,2,1,3,3,2,3,0,2,3,0,2,3,0,2,3,0,2,3,0,2,
  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,2,0,2,0,2,0,2,0,2,0,2,0,2,0,2,
  1,0,2,1,1,0,2,1,1,0,2,1,1,0,2,1,3,0,2,3,3,0,2,3,3,0,2,3,3,0,2,3,
  0,2,1,0,2,1,0,2,0,2,1,0,2,1,0,2,1,1,3,0,2,3,1,1,1,1,3,0,2,3,1,1,
  3,3,1,0,2,1,3,3,3,3,1,0,2,1,3,3,0,2,3,0,2,3,0,2,0,2,3,0,2,3,0,2,
  1,0,2,1,0,2,1,0,2,1,0,2,1,0,2,1,3,0,2,3,1,1,3,0,2,3,1,1,3,0,2,3,
  0,2,0,2,3,3,1,0,2,1,3,3,0,2,0,2,1,1,1,1,0,2,3,0,2,3,0,2,1,1,1,1,
  3,3,3,3,0,2,1,0,2,1,0,2,3,3,3,3,0,2,0,2,1,1,3,0,2,3,1,1,0,2,0,2,
  1,0,2,1,3,3,1,0,2,1,3,3,1,0,2,1,3,0,2,3,0,2,3,0,2,3,0,2,3,0,2,3};

uint64_t hilbert_coordinate_3 (uint32_t x, uint32_t y) {
  uint64_t z = 0;
  int i, n = 0;
  x = (x << 4) | (x >> 28);
  for (i = 0; i < 8; i++) {
    x = (x << 4) | (x >> 28);
    y = (y << 4) | (y >> 28);
    int j = (x & 0xf0) | (y & 0xf) | n;
    n = ((int) quadrant[j]) << 8;
    z = (z << 8) | bytes[j];
    //    printf ("%x %x %lx\n", j, n, z);
  }
  return z;
}

static uint64_t dilate_4 (uint32_t x0) {
  uint64_t x = x0;
  x = (x | (x << 16)) & 0x0000FFFF0000FFFF;
  x = (x | (x << 8)) &  0x00FF00FF00FF00FF;
  x = (x | (x << 4)) &  0x0F0F0F0F0F0F0F0F;
  return x;
}

uint64_t hilbert_coordinate_4 (uint32_t x, uint32_t y) {
  uint64_t c = (dilate_4(x) << 4) | dilate_4(y);
  uint64_t z = 0;
  int i, n = 0;
  for (i = 7; i >= 0; i--) {
    c = (c << 8) | (c >> 56);
    int j = (c & 0xff) | n;
    n = quadrant[j] << 8;
    z = (z << 8) | bytes[j];
  }
  return z;
}

// Only about 10% improvement over hilbert_coordinate_4
void hilbert_coordinate_5 (uint32_t * x, uint32_t * y, uint64_t * z) {
  uint64_t c0 = (dilate_4(x[0]) << 4) | dilate_4(y[0]);
  uint64_t c1 = (dilate_4(x[1]) << 4) | dilate_4(y[1]);
  uint64_t z0 = 0;
  uint64_t z1 = 0;
  int i, n0 = 0, n1 = 0;
  for (i = 7; i >= 0; i--) {
    c0 = (c0 << 8) | (c0 >> 56);
    c1 = (c1 << 8) | (c1 >> 56);
    //    int j = ((c >> (i << 3)) & 0xff) | n;
    int j0 = (c0 & 0xff) | n0;
    int j1 = (c1 & 0xff) | n1;
    n0 = quadrant[j0] << 8;
    n1 = quadrant[j1] << 8;
    z0 = (z0 << 8) | bytes[j0];
    z1 = (z1 << 8) | bytes[j1];
  }
  z[0] = z0;
  z[1] = z1;
//  printf ("%lx\n", z0);
//  printf ("%lx\n", z1);
}

// Fastest one!
void hilbert_coordinate_6 (uint32_t * x, uint32_t * y, uint64_t * z) {
  uint32_t x0 = x[0];
  uint32_t y0 = y[0];
  uint32_t x1 = x[1];
  uint32_t y1 = y[1];
  uint64_t z0 = 0;
  uint64_t z1 = 0;
  int i, n0 = 0, n1 = 0;
  x0 = (x0 << 4) | (x0 >> 28);
  x1 = (x1 << 4) | (x1 >> 28);
  for (i = 0; i < 8; i++) {
    x0 = (x0 << 4) | (x0 >> 28);
    y0 = (y0 << 4) | (y0 >> 28);
    x1 = (x1 << 4) | (x1 >> 28);
    y1 = (y1 << 4) | (y1 >> 28);
    int j0 = (x0 & 0xf0) | (y0 & 0xf) | n0;
    int j1 = (x1 & 0xf0) | (y1 & 0xf) | n1;
    n0 = ((int) quadrant[j0]) << 8;
    n1 = ((int) quadrant[j1]) << 8;
    z0 = (z0 << 8) | bytes[j0];
    z1 = (z1 << 8) | bytes[j1];
    //    printf ("%x %x %lx\n", j, n, z);
  }
  z[0] = z0;
  z[1] = z1;
}

uint64_t hilbert_coordinate_7 (uint32_t x, uint32_t y) {
  uint32_t heven = x ^ y;
  int32_t a = -1;
  uint32_t b = heven;
  uint32_t c = x & ~y;
  uint32_t d = ~heven;
  int32_t e = -1;
  uint32_t f = ~x & ~y;
  int i, k;
  for (i = 0; i < 5; i++) {
    k = 1 << i;
    //    printf ("%x %x %x %x %x %x\n", a, b, c, d, e, f);
    uint32_t a1 = a >> k;
    uint32_t b1 = b >> k;
    uint32_t c1 = c >> k;
    uint32_t d1 = d >> k;
    uint32_t e1 = e >> k;
    uint32_t f1 = f >> k;
    uint32_t a2 = (a & a1) ^ (b & d1);
    uint32_t b2 = (a & b1) ^ (b & e1);
    uint32_t c2 = (a & c1) ^ (b & f1) ^ c;
    uint32_t d2 = (d & a1) ^ (e & d1);
    uint32_t e2 = (d & b1) ^ (e & e1);
    uint32_t f2 = (d & c1) ^ (e & f1) ^ f;
    a = a2;
    b = b2;
    c = c2;
    d = d2;
    e = e2;
    f = f2;
  }
  uint32_t n1 = c;
  uint32_t n2 = f;
  //  printf("7: %x %x\n", n1, n2);
  uint32_t n0 = n1 ^ n2;
  uint32_t hodd = x ^ (heven & n0) ^ n1;
  return (dilate(hodd) << 1) | dilate(heven);
}

#ifdef __SSE2__

void hilbert_coordinate_7_sse (uint32_t * xs, uint32_t * ys, uint64_t * zs) {
  __m128i zero = _mm_setzero_si128 ();
  __m128i ones = _mm_cmpeq_epi8 (zero, zero);

  __m128i x = _mm_loadu_si128 ((__m128i *) xs);
  __m128i y = _mm_loadu_si128 ((__m128i *) ys);
  __m128i heven = _mm_xor_si128 (x, y);
  __m128i notx = _mm_xor_si128 (x, ones);
  __m128i noty = _mm_xor_si128 (y, ones);

  __m128i a = ones;
  __m128i b = heven;

  __m128i c = _mm_and_si128 (x, noty);
  __m128i d = _mm_xor_si128 (heven, ones);
  __m128i e = ones;
  __m128i f = _mm_and_si128 (notx, noty);
  int i, k;
  for (i = 0, k = 1; i < 5; i++, k=k+k) {
    //    k = 1 << i;
    //    printf ("%x %x %x %x %x %x\n", a, b, c, d, e, f);
    __m128i k1 = _mm_set_epi32(0, 0, 0, k);
    __m128i a1 = _mm_sra_epi32 (a, k1);
    __m128i b1 = _mm_srl_epi32 (b, k2);
    __m128i c1 = _mm_srl_epi32 (c, k2);
    __m128i d1 = _mm_srl_epi32 (d, k2);
    __m128i e1 = _mm_sra_epi32 (e, k2);
    __m128i f1 = _mm_srl_epi32 (f, k2);

    __m128i a2 = _mm_xor_si128 (_mm_and_si128 (a, a1), _mm_and_si128 (b, d1));
    __m128i b2 = _mm_xor_si128 (_mm_and_si128 (a, b1), _mm_and_si128 (b, e1));
    __m128i c2 =
      _mm_xor_si128 (_mm_xor_si128 (_mm_and_si128 (a, c1),
                                    _mm_and_si128 (b, f1)), c);
    __m128i d2 = _mm_xor_si128 (_mm_and_si128 (d, a1), _mm_and_si128 (e, d1));
    __m128i e2 = _mm_xor_si128 (_mm_and_si128 (d, b1), _mm_and_si128 (e, e1));
    __m128i f2 =
      _mm_xor_si128 (_mm_xor_si128 (_mm_and_si128 (d, c1),
                                    _mm_and_si128 (e, f1)), f);
    a = a2;
    b = b2;
    c = c2;
    d = d2;
    e = e2;
    f = f2;
  }
  __m128i n1 = c;
  __m128i n2 = f;
  //  printf("7: %x %x\n", n1, n2);
  __m128i n0 = _mm_xor_si128 (n1, n2);
  __m128i hodd = _mm_xor_si128 (_mm_xor_si128 (x, n1),
                                _mm_and_si128 (heven, n0));
  __m128i heven2 = _mm_unpackhi_epi8 (heven, zero);
  __m128i heven1 = _mm_unpacklo_epi8 (heven, zero);
  __m128i hodd2 = _mm_unpackhi_epi8 (hodd, zero);
  __m128i hodd1 = _mm_unpacklo_epi8 (hodd, zero);

  __m128i z1 = _mm_or_si128(_mm_slli_epi16(dilate_sse_8 (hodd1), 1),
                            dilate_sse_8 (heven1));
  __m128i z2 = _mm_or_si128(_mm_slli_epi16(dilate_sse_8 (hodd2), 1),
                            dilate_sse_8 (heven2));

  _mm_storeu_si128((__m128i *) zs, z1);
  _mm_storeu_si128((__m128i *) (zs + 2), z2);
}

#endif

// Very close performance-wise to hilbert_coordinate_3, but less portable...
uint64_t hilbert_coordinate_8 (uint32_t x, uint32_t y) {
  uint32_t heven = x ^ y;
  //  int32_t a = -1;
  //  uint32_t b = heven;
  //  uint32_t c = x & ~y;
  __m128i heven_vect = _mm_cvtsi32_si128(heven);
  __m128i x_vect = _mm_cvtsi32_si128(x);
  __m128i u = _mm_set_epi32 (0, -1, heven, x & ~y);
  //  uint32_t d = ~heven;
  //  int32_t e = -1;
  //  uint32_t f = ~x & ~y;
  __m128i v = _mm_set_epi32 (0, ~heven, -1, ~x & ~y);
  __m128i id_u = _mm_set_epi32 (0, -1, 0, 0);
  __m128i id_v = _mm_set_epi32 (0, 0, -1, 0);
  int i, k;
  for (i = 0; i < 5; i++) {
    k = 1 << i;
    //    uint32_t a1 = a >> k;
    //    uint32_t b1 = b >> k;
    //    uint32_t c1 = c >> k;
    //    uint32_t d1 = d >> k;
    //    uint32_t e1 = e >> k;
    //    uint32_t f1 = f >> k;
    __m128i u1 = _mm_or_si128 (_mm_srli_epi32 (u, k),
                               _mm_slli_epi32 (id_u, 32 - k));
    __m128i v1 = _mm_or_si128 (_mm_srli_epi32 (v, k),
                               _mm_slli_epi32 (id_v, 32 - k));
    __m128i a = _mm_shuffle_epi32 (u, 3 << 6 | 2 << 4 | 2 << 2 | 2);
    __m128i b = _mm_shuffle_epi32 (u, 3 << 6 | 1 << 4 | 1 << 2 | 1);
    __m128i c = _mm_shuffle_epi32 (u, 3 << 6 | 3 << 4 | 3 << 2 | 0);
    __m128i d = _mm_shuffle_epi32 (v, 3 << 6 | 2 << 4 | 2 << 2 | 2);
    __m128i e = _mm_shuffle_epi32 (v, 3 << 6 | 1 << 4 | 1 << 2 | 1);
    __m128i f = _mm_shuffle_epi32 (v, 3 << 6 | 3 << 4 | 3 << 2 | 0);
    //    printf ("%x %x %x %x %x %x\n", _mm_cvtsi128_si32(a), _mm_cvtsi128_si32(b), _mm_cvtsi128_si32(c), _mm_cvtsi128_si32(d), _mm_cvtsi128_si32(e), _mm_cvtsi128_si32(f));
    //    uint32_t a2 = (a & a1) ^ (b & d1);
    //    uint32_t b2 = (a & b1) ^ (b & e1);
    //    uint32_t c2 = (a & c1) ^ (b & f1) ^ c;
    //    uint32_t d2 = (d & a1) ^ (e & d1);
    //    uint32_t e2 = (d & b1) ^ (e & e1);
    //    uint32_t f2 = (d & c1) ^ (e & f1) ^ f;
    //    a = a2;
    //    b = b2;
    //    c = c2;
    //    d = d2;
    //    e = e2;
    //    f = f2;
    u = _mm_xor_si128 (_mm_xor_si128(_mm_and_si128(a, u1),
                                     _mm_and_si128(b, v1)),
                       c);
    v = _mm_xor_si128 (_mm_xor_si128(_mm_and_si128(d, u1),
                                     _mm_and_si128(e, v1)),
                       f);
  }
  //  uint32_t n1 = c;
  //  uint32_t n2 = f;
  //  uint32_t hodd = x ^ (heven & n0) ^ n1;
  //  return (dilate(hodd) << 1) | dilate(heven);
  __m128i n0 = _mm_xor_si128(u, v);
  __m128i hodd = _mm_xor_si128(_mm_and_si128(heven_vect, n0),
                               _mm_xor_si128(x_vect, u));
  //  printf("%x %x\n", hodd2, _mm_cvtsi128_si32(hodd));
  __m128i hoddeven = _mm_unpacklo_epi32 (hodd, heven_vect);
  __m128i zero = _mm_setzero_si128 ();
  hoddeven = dilate_sse_8(_mm_unpacklo_epi8 (hoddeven, zero));
  heven_vect = _mm_srli_si128(hoddeven, 8);
  return _mm_cvtsi128_si64(_mm_or_si128(heven_vect,
                                        _mm_slli_epi64 (hoddeven, 1)));
}

int main (void) {
  /*
  {
  int i, j;
  for (i = 0; i < 4; i++) {
    for (j = 0; j < 4; j++) {
      printf ("%d %d %ld %ld\n", i, j,
              hilbert_coordinate_7(i,j), hilbert_coordinate_8(i,j));
    }
  }
  }
  */
#if 1
  uint32_t x = 1;
  uint32_t y = 1;
  uint64_t accu = 0;
  long i;
  for (i = 0; i < 30000000; i++) {
    x *= 2234039081;
    y *= 269614307;
    accu += hilbert_coordinate_3(x, y);
  }
  printf ("%lx\n", accu);

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
#if 1
    hilbert_coordinate_6(xs, ys, zs);
    hilbert_coordinate_6(xs + 2, ys + 2, zs + 2);
#else
    hilbert_coordinate_sse(xs, ys, zs);
#endif
    accu = accu + zs[0] + zs[1] + zs[2] + zs[3];
  }
  printf ("%lx\n", accu);

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


/*
extract 4 bits from each coordinates
==> 8 bit of coordinate
    new quadrant (2 bits)

  x = ((x & mask1) | (y & ~mask1)) ^ mask2)
  y = ((y & mask1) | (x & ~mask1)) ^ mask2)

  uint64_t z;
  n = 0;
  for (i = 0; i < 8; i++) {
    j = (x >> 28) | ((y >> 24) & 0xf0) | n;
    n = quadrant[j] << 8;
    z = (z << 8) | bytes[j];
  }

quadrant 1/2:
a = (1 0
     0 1)
quadrant 0:
b = (0 1
     1 0)
quadrant 1:
c = (0  -1
     -1  0)

d = (-1  0
      0 -1)

   0   1
0  a   b
1  d   c


    a   b   c   d
a   a   b   c   d
b   b   a   d   c
c   c   d   a   b
d   d   c   b   a

    00  01  10  11
00  00  01  10  11
01  01  00  11  10
10  10  11  00  01
11  11  10  01  00

ax = xa = x
bb = cc = dd = a
bc = cb = d
bd = db = c
dc = cd = b

*/
