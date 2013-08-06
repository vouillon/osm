/* OSM tools
 * Copyright (C) 2013 Jérôme Vouillon
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */

/*
    for j = j0 to j0 + chunk_size - 1 do
      let v = read_signed_varint a pos + !last in
      buf.(j) <- v;
      last := v
    done
*/
#include <stdint.h>

#include <caml/mlvalues.h>
#include <caml/bigarray.h>

static uint64_t unsigned_of_signed(int64_t n) {
  return (n << 1) ^ (n >> 63);
}

static int64_t signed_of_unsigned(uint64_t n) {
  return (n >> 1) ^ -((int64_t) (n & 1));
}

static uint64_t read_varint (uint8_t ** a) {
  uint8_t * p = *a;
  uint64_t v = 0;
  int k;
  for (k = 0; k < 10; k++) {
    uint64_t c = *(p++);
    v |= (c & 0x7f) << (7 * k);
    if (c < 0x80) break;
  }
  *a = p;
  return v;
}

/*
void write_varint_2 (uint8_t ** a, uint64_t v) {
  uint8_t * p = *a;
  while (v >= 0x80) {
    *(p++) = (v & 0x7f) | 0x80;
    v >>= 7;
  }
  *(p++) = v;
  *a = p;
}
*/

static void write_varint (uint8_t ** a, uint64_t v) {
  uint8_t * p = *a;
  int k;
  for (k = 0; k < 9; k++) {
    if (v < 0x80) break;
    *(p++) = v | 0x80;
    v >>= 7;
  }
  *(p++) = v;
  *a = p;
}

static uint8_t * decode_chunk
(uint8_t *a, value buf, long j0, long chunk_size) {
  long j;
  int64_t v = 0;
  for (j = j0; j < j0 + chunk_size; j++) {
    v += signed_of_unsigned(read_varint(&a));
    Field(buf, j) = Val_long(v);
  }
  return a;
}

CAMLprim value decode_chunk_ml
(value va, value vpos, value vbuf, value vj0, value vchunk_size) {
  uint8_t * a = ((uint8_t *) Caml_ba_array_val(va)->data);
  long j0 = Long_val (vj0);
  long chunk_size = Long_val (vchunk_size);

  uint8_t * b = decode_chunk (a + Long_val (vpos), vbuf, j0, chunk_size);

  return Val_long(b - a);
}

static uint8_t * decode_chunk_bigarray
(uint8_t *a, int64_t * buf, long j0, long chunk_size) {
  long j;
  int64_t v = 0;
  for (j = j0; j < j0 + chunk_size; j++) {
    v += signed_of_unsigned(read_varint(&a));
    buf[j] = v;
  }
  return a;
}

CAMLprim value decode_chunk_bigarray_ml
(value va, value vpos, value vbuf, value vj0, value vchunk_size) {
  uint8_t * a = ((uint8_t *) Caml_ba_array_val(va)->data);
  long j0 = Long_val (vj0);
  long chunk_size = Long_val (vchunk_size);
  int64_t * buf = ((int64_t *) Caml_ba_array_val(vbuf)->data);

  uint8_t * b =
    decode_chunk_bigarray (a + Long_val (vpos), buf, j0, chunk_size);

  return Val_long(b - a);
}

static uint8_t * encode_chunk
(uint8_t *a, value buf, long j0, long chunk_size) {
  long j;
  int64_t last = 0;
  for (j = 0; j < chunk_size; j++) {
    int64_t v = Long_val(Field(buf, j0 + j));
    write_varint (&a, unsigned_of_signed(v - last));
    last = v;
  }
  return a;
}

CAMLprim value encode_chunk_ml
(value va, value vpos, value vbuf, value vj0, value vchunk_size) {
  uint8_t * a = ((uint8_t *) Caml_ba_array_val(va)->data);
  long j0 = Long_val (vj0);
  long chunk_size = Long_val (vchunk_size);

  uint8_t * b = encode_chunk (a + Long_val (vpos), vbuf, j0, chunk_size);

  return Val_long(b - a);
}
