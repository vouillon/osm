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

void merge (long * a, long * b, long * c, int l) {
  long * c2 = c + l;

  while (c != c2) {
    long * a2 = a;
    long * b2 = b;
    long va = *a;
    long vb = *b;
    a++;
    b++;
    if (va < vb) {
      b = b2;
    } else {
      a = a2;
      va = vb;
    }
    *c = va;
    c++;
  }
}

void merge2 (long * a, long * b, long * c, int l) {
  long * c2 = c + l;

  while (c != c2) {
    long va = *a;
    long vb = *b;
    if (va < vb) {
      a++;
      *c = va;
    } else {
      b++;
      *c = vb;
    }
    c++;
  }
}

void merge3 (long * a1, long * b1, long * c1, long * a2, long * b2, long * c2, int l1, int l2) {
  long * a3 = a1 + l1;
  long * b3 = b1 + l2;

  while ((a1 != a3) && (b1 != b3)) {
    long * p;
    if (*a1 < *b1) {
      *c1 = *a1;
      p=a2;
      a1++; a2++;
    } else {
      *c1 = *b1;
      p=b2;
      b1++; b2++;
    }
    *c2 = *p;
    c1++; c2++;
  }
}

/*
void merge4 (long * a1, long * b1, long * c1, long * a2, long * b2, long * c2, int l) {
  long * a3 = a1 + l;
  long * a3 = a1 + l;
  int ib = 0;

  while ((ia != l) && (ib != l)) {
    long va1, vb1, va2, vb2, v1, v2;
    va1 = a1[ia]; vb1 = b1[ib];
    va2 = a2[ia]; vb2 = b2[ib];
    if (va1 < va2) {
      v1 = va1;
      v2 = va2;
      ia++;
    } else {
      v1 = vb1;
      v2 = vb2;
      ib++;
    }
    *c1 = v1;
    *c2 = v2;
    c1++; c2++;
  }
}
*/

void merge4 (long * t1, volatile long * t2, long a, long b, long * c1, long * c2,
             long la, long lb) {

  while ((a != la) && (b != lb)) {
    long va1 = t1[a];
    long vb1 = t1[b];
    long va2 = t2[a];
    long vb2 = t2[b];
    long v1, v2;
    if (va1 < vb1) {
      a++;
      v1 = va1;
      v2 = va2;
    } else {
      b++;
      v1 = vb1;
      v2 = vb2;
    }
    *c1 = v1;
    *c2 = v2;
    c1++; c2++;
  }
}

void merge5 (long * t1, long * t2, long a, long b, long * c1, long * c2,
             long la, long lb) {

  while ((a != la) && (b != lb)) {
    long va1 = t1[a];
    long vb1 = t1[b];
    long v1, i;
    if (va1 < vb1) {
      i = a;
      a++;
      *c1 = va1;
    } else {
      i = b;
      b++;
      *c1 = vb1;
    }
    *c2 = t2[i];
    c1++; c2++;
  }
}

void sort6_sorting_network_simple_swap(int * d){
#define min(x, y) (x<y?x:y)
#define max(x, y) (x<y?y:x) 
#define SWAP(x,y) { const int a = min(d[x], d[y]); const int b = max(d[x], d[y]); d[x] = a; d[y] = b;}
  SWAP(1, 2);
  SWAP(4, 5);
  SWAP(0, 2);
  SWAP(3, 5);
  SWAP(0, 1);
  SWAP(3, 4);
  SWAP(1, 4);
  SWAP(0, 3);
  SWAP(2, 5);
  SWAP(1, 3);
  SWAP(2, 4);
  SWAP(2, 3);
#undef SWAP
#undef min
#undef max
}

void sort6_sorting_network(int * d, int * e){
#define min(x, y) (x<y?x:y)
#define max(x, y) (x<y?y:x) 
#define SWAP(x,y) { int a1,b1,a2,b2; a1 = d[x]; b1=d[y]; if (a1 < b1) { a2 = a1; b2= b1; } else {a2 = b1; b2 = a1; } d[x] = a2; d[y] = b2;}
  SWAP(1, 2);
  SWAP(4, 5);
  SWAP(0, 2);
  SWAP(3, 5);
  SWAP(0, 1);
  SWAP(3, 4);
  SWAP(1, 4);
  SWAP(0, 3);
  SWAP(2, 5);
  SWAP(1, 3);
  SWAP(2, 4);
  SWAP(2, 3);
#undef SWAP
#undef min
#undef max
}
