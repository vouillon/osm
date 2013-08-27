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

#include <assert.h>

#include <stdlib.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

static void merge (intnat * a1, intnat * b1, int i1, int l1,
            intnat * a2, intnat * b2, int i2, int l2,
            intnat * a, intnat * b, int i, int l) {
  assert (l1 - i1 + l2 - i2 == l - i);

  intnat v1 = a1[i1], v2 = a2[i2];

  if (a1[l1 - 1] <= a2[i2]) {
    memmove (a + i, a1 + i1, (l1 - i1) * sizeof (intnat));
    memmove (b + i, b1 + i1, (l1 - i1) * sizeof (intnat));
    memmove (a + i + l1 - i1, a2 + i2, (l2 - i2) * sizeof (intnat));
    memmove (b + i + l1 - i1, b2 + i2, (l2 - i2) * sizeof (intnat));
    return;
  }
  if (a2[l2 - 1] < a1[i1]) { /* Strict comparison for sort stability */
    memmove (a + i, a2 + i2, (l2 - i2) * sizeof (intnat));
    memmove (b + i, b2 + i2, (l2 - i2) * sizeof (intnat));
    memmove (a + i + l2 - i2, a1 + i1, (l1 - i1) * sizeof (intnat));
    memmove (b + i + l2 - i2, b1 + i1, (l1 - i1) * sizeof (intnat));
    return;
  }
  while (1) {
    if (v1 <= v2) {
      a[i] = v1; b[i] = b1[i1];
      i1++; i++;
      if (i1 < l1)
        v1 = a1 [i1];
      else {
        assert (l - i == l2 - i2);
        memmove (a + i, a2 + i2, (l2 - i2) * sizeof (intnat));
        memmove (b + i, b2 + i2, (l2 - i2) * sizeof (intnat));
        break;
      }
    } else {
      a[i] = v2; b[i] = b2[i2];
      i2++; i++;
      if (i2 < l2)
        v2 = a2 [i2];
      else {
        assert (l - i == l1 - i1);
        memmove (a + i, a1 + i1, (l1 - i1) * sizeof (intnat));
        memmove (b + i, b1 + i1, (l1 - i1) * sizeof (intnat));
        break;
      }
    }
  }
}

static void isort (intnat * a1, intnat * b1, int i1, int l1,
                   intnat * a2, intnat * b2, int i2, int l2) {
  assert (l1 - i1 == l2 - i2);
  int i, len = l1 - i1;

  for (i = 0; i < len; i++) {
    intnat v = a1[i1 + i];
    intnat w = b1[i1 + i];
    int j = i2 + i;
    while (j > i2 && a2[j - 1] > v) {
      a2[j] = a2[j - 1];
      b2[j] = b2[j - 1];
      j --;
    }
    a2[j] = v;
    b2[j] = w;
  }
}

static int cuttoff = 15;

static void sort_rec (intnat * a1, intnat * b1, int i1, int l1,
                      intnat * a2, intnat * b2, int i2, int l2) {
  int len = l1 - i1;
  assert (len == l2 - i2);
  if (len <= cuttoff)
    isort (a1, b1, i1, l1, a2, b2, i2, l2);
  else {
    int len1 = len / 2;
    int len2 = len - len1;
    sort_rec (a1, b1, i1 + len1, l1, a2, b2, i2 + len1, l2);
    sort_rec (a1, b1, i1, i1 + len1, a1, b1, i1 + len2, l1);
    merge (a1, b1, i1 + len2, l1, a2, b2, i2 + len1, l2, a2, b2, i2, l2);
  }
}

CAMLprim value sort_bigarrays (value va, value vb, value l) {
  struct caml_ba_array * a0 = Caml_ba_array_val(va);
  struct caml_ba_array * b0 = Caml_ba_array_val(vb);
  int len = Long_val (l);
  assert (a0->dim[0] >= len);
  assert (b0->dim[0] >= len);
  intnat * a1 = (intnat *) a0->data;
  intnat * b1 = (intnat *) b0->data;

  if (len <= cuttoff)
    isort (a1, b1, 0, len, a1, b1, 0, len);
  else {
    int len1 = len / 2;
    int len2 = len - len1;
    intnat * a2 = (intnat *) malloc (len2 * sizeof (intnat));
    intnat * b2 = (intnat *) malloc (len2 * sizeof (intnat));
    sort_rec (a1, b1, len1, len, a2, b2, 0, len2);
    sort_rec (a1, b1, 0, len1, a1, b1, len2, len);
    merge (a1, b1, len2, len, a2, b2, 0, len2, a1, b1, 0, len);
    free (a2);
    free (b2);
  }
  return Val_unit;
}
