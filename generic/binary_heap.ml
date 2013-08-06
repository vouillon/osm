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

type t =
  { mutable index : int array;
    mutable weight : int array;
    mutable size : int }

let size h = h.size

let min_weight h = h.weight.(0)

let rec down_heap h j i w =
  let j' = 2 * j + 1 in
  let j'' = j' + 1 in
  let j' =
    if j'' < h.size && h.weight.(j') > h.weight.(j'') then j'' else j' in
  if j' < h.size && h.weight.(j') < w then begin
    h.index.(j) <- h.index.(j');
    h.weight.(j) <- h.weight.(j');
    down_heap h j' i w
  end else begin
    h.index.(j) <- i;
    h.weight.(j) <- w
  end

let delete_min h =
  if h.size = 0 then -1 else begin
    let i = h.index.(0) in
    let s = h.size - 1 in
    h.size <- s;
    down_heap h 0 h.index.(s) h.weight.(s);
    i
  end

let rec up_heap h j i w =
  let j' = (j - 1) / 2 in
  if j > 0 && h.weight.(j') > w then begin
    h.weight.(j) <- h.weight.(j');
    h.index.(j) <- h.index.(j');
    up_heap h j' i w
  end else begin
    h.index.(j) <- i;
    h.weight.(j) <- w
  end

let resize a =
  let l = Array.length a in
  let a' = Array.make (2 * l) 0 in
  Array.blit a 0 a' 0 l;
  a'

let insert h i w =
  let s = h.size in
  h.size <- s + 1;
  if s = Array.length h.index then begin
    h.index <- resize h.index;
    h.weight <- resize h.weight
  end;
  up_heap h s i w

let make () =
  { index = Array.make 1024 0;
    weight = Array.make 1024 0;
    size = 0 }

(*
delete_min : t -> int  (returns -1 if None)
insert : t -> int -> unit
*)
