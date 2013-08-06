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

#include <string.h>

#include "caml/intext.h"
#include "caml/bigarray.h"

#define Array_data(a, i) (((char *) a->data) + Long_val(i))

CAMLprim value ml_marshal_to_bigarray(value v, value flags)
{
  char *buf;
  long len;
  output_value_to_malloc(v, flags, &buf, &len);
  return alloc_bigarray(BIGARRAY_UINT8 | BIGARRAY_C_LAYOUT | BIGARRAY_MANAGED,
                        1, buf, &len);
}

CAMLprim value ml_marshal_to_bigarray_buffer(value b, value ofs,
                                             value v, value flags)
{
  struct caml_bigarray *b_arr = Bigarray_val(b);
  return Val_long(caml_output_value_to_block(v, flags, Array_data (b_arr, ofs),
					     b_arr->dim[0] - Long_val(ofs)));
}


CAMLprim value ml_unmarshal_from_bigarray(value b, value ofs)
{
  struct caml_bigarray *b_arr = Bigarray_val(b);
  return input_value_from_block (Array_data (b_arr, ofs),
                                 b_arr->dim[0] - Long_val(ofs));
}

CAMLprim value ml_blit_string_to_bigarray
(value s, value i, value a, value j, value l)
{
  char *src = String_val(s) + Int_val(i);
  char *dest = Array_data(Bigarray_val(a), j);
  memcpy(dest, src, Long_val(l));
  return Val_unit;
}

CAMLprim value ml_blit_bigarray_to_string
(value a, value i, value s, value j, value l)
{
  char *src = Array_data(Bigarray_val(a), i);
  char *dest = String_val(s) + Long_val(j);
  memcpy(dest, src, Long_val(l));
  return Val_unit;
}
