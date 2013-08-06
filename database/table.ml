(* OSM tools
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
 *)

let map_incr = 1024 * 1024 * 2

type ('a, 'b) t =
  { ch : Unix.file_descr;
    mutable ar : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t }

let int64 = Bigarray.int64
let int32 = Bigarray.int32
let int = Bigarray.int
let float = Bigarray.float64

let make ?len nm kind =
  let flags = [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] in
  let ch = Unix.openfile nm flags 0o600 in
  let l = match len with None -> map_incr | Some l -> l in
  { ch = ch;
    ar = Bigarray.Array1.map_file ch kind Bigarray.c_layout true l }

let extend a l =
  let l' = ref (Bigarray.Array1.dim a.ar) in
  if !l' < l then begin
Format.printf "RESIZE@.";
    while !l' < l do l' := !l' + map_incr done;
    let kind = Bigarray.Array1.kind a.ar in
    let layout = Bigarray.Array1.layout a.ar in
    a.ar <- Bigarray.Array1.map_file a.ch kind layout true !l'
  end

(****)

module A = Bigarray.Array1

type ta = (int, Bigarray.int_elt, Bigarray.c_layout) A.t
type tb = (int, Bigarray.int_elt, Bigarray.c_layout) A.t

let merge a1 b1 i1 l1 a2 b2 i2 l2 (a : ta) (b : tb) i l =
  assert (l1 - i1 + l2 - i2 = l - i);
  let rec loop i1 v1 i2 v2 i =
(*    assert (l1 - i1 + l2 - i2 = l - i);*)
    if compare v1 v2 <= 0 then begin
      a.{i} <- v1; b.{i} <- b1.{i1};
      let i1 = i1 + 1 in
      if i1 < l1 then
        loop i1 a1.{i1} i2 v2 (i + 1)
      else begin
        assert (l - i - 1 = l2 - i2);
        A.blit (A.sub a2 i2 (l2 - i2)) (A.sub a (i + 1) (l2 - i2));
        A.blit (A.sub b2 i2 (l2 - i2)) (A.sub b (i + 1) (l2 - i2))
      end
    end else begin
      a.{i} <- v2; b.{i} <- b2.{i2};
      let i2 = i2 + 1 in
      if i2 < l2 then
        loop i1 v1 i2 a2.{i2} (i + 1)
      else begin
        assert (l - i - 1 = l1 - i1);
        A.blit (A.sub a1 i1 (l1 - i1)) (A.sub a (i + 1) (l1 - i1));
        A.blit (A.sub b1 i1 (l1 - i1)) (A.sub b (i + 1) (l1 - i1))
      end
    end
  in
  assert (i1 < l1); assert (i2 < l2);
  loop i1 a1.{i1} i2 a2.{i2} i

(*
; for j = i to l - 1 do
Format.printf "%Ld " a.{j}
done;
Format.printf "@."
*)


let isort (a1 : ta) (b1 : tb) i1 l1 a2 (b2 : tb) i2 l2 =
  assert (l1 - i1 = l2 - i2);
  let len = l1 - i1 in
(*
for i = 0 to len - 1 do
Format.printf "%Ld " a1.{i1 + i}
done;
Format.printf "@.";
*)
  for i = 0 to len - 1 do
    let v = a1.{i1 + i} in
    let w = b1.{i1 + i} in
    let j = ref (i2 + i) in
    while !j > i2 && compare a2.{!j - 1} v > 0 do
      a2.{!j} <- a2.{!j - 1};
      b2.{!j} <- b2.{!j - 1};
      decr j;
    done;
    a2.{!j} <- v;
    b2.{!j} <- w
  done
(*
;for i = 0 to len - 1 do
Format.printf "%Ld " a2.{i2 + i}
done;
Format.printf "@."
*)

let cuttoff = 15

let rec sort_rec a1 b1 i1 l1 a2 b2 i2 l2 =
  let len = l1 - i1 in
  assert (len = l2 - i2);
  if len <= cuttoff then
    isort a1 b1 i1 l1 a2 b2 i2 l2
  else begin
    let len = len / 2 in
    assert (i1 + 2 * len <= l1);
    sort_rec a1 b1 (i1 + len) l1 a2 b2 (i2 + len) l2;
    sort_rec a1 b1 i1 (i1 + len) a1 b1 (i1 + len) (i1 + 2 * len);
    merge a2 b2 (i2 + len) l2 a1 b1 (i1 + len) (i1 + 2 * len) a2 b2 i2 l2
  end

let sort a1 b1 =
  let len = A.dim a1 in
  assert (A.dim b1 = len);
  if len <= cuttoff then
    isort a1 b1 0 len a1 b1 0 len
  else begin
    let len1 = len / 2 in
    let len2 = len - len1 in
    let a2 = A.create (A.kind a1) (A.layout a1) len2 in
    let b2 = A.create (A.kind b1) (A.layout b1) len2 in
    sort_rec a1 b1 len1 len a2 b2 0 len2;
    sort_rec a1 b1 0 len1 a1 b1 len1 (2 * len1);
    merge a2 b2 0 len2 a1 b1 len1 (2 * len1) a1 b1 0 len
  end

external sort : ta -> tb -> unit = "sort_bigarrays"

(*
let hash (a : ta) =
  let len = A.dim a in
  let len' = 2 * len in
  let tbl = A.create Bigarray.int Bigarray.c_layout len' in
  A.fill tbl (-1);
  for i = 0 to len - 1 do
    let v = a.{i} in
    let h0 =  ((Int64.to_int (Int64.mul v 1357571L)) mod len') in
    let h = ref h0 in
    let s = ref 1 in
    while tbl.{!h} <> -1 do h := (!h + !s) mod len'; s := !s + 1 done;
(*
if i mod 10000 = 0 then Format.printf "(%d)@." i;
if !h > h0 then Format.printf "%d %d %Ld@." i (!h - h0) v;
*)
    tbl.{!h} <- i
  done;
  tbl

let hash_map (tbl : tb) (a : ta) (b : ta) (c : tb) =
  let len = A.dim b in
  let len' = A.dim tbl in
  assert (A.dim c = len);
  for i = 0 to len - 1 do
    let v = b.{i} in
    let h0 =  ((Int64.to_int (Int64.mul v 1357571L)) mod len') in
    let h = ref h0 in
    let s = ref 1 in
    while a.{tbl.{!h}} <> v do h := (!h + !s) mod len'; s := !s + 1 done;
    c.{i} <- tbl.{!h}
  done
*)
