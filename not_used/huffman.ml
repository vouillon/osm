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

type t = Leaf of int | Node of t * t

module Q = Pqueue.Make
(struct type u = int * t type t = u let compare (a, _) (b, _) = compare a b end)

let _ =
let ch = open_in "/tmp/d" in
let q = ref Q.empty in
let h = Hashtbl.create 17 in
begin try
  while true do
    let l = input_line ch in
    let i = ref 0 in
    while l.[!i] = ' ' do incr i done;
    let l = String.sub l !i (String.length l - !i) in
    let i = String.index l ' ' in
    let w = int_of_string (String.sub l 0 i) in
    let v = int_of_string (String.sub l (i + 1) (String.length l - i - 1)) in
let v = v / 5 in
    Hashtbl.replace h v (w + try Hashtbl.find h v with Not_found -> 0)
  done
with End_of_file -> () end;
Hashtbl.iter (fun v w -> q := Q.add (w, Leaf v) !q) h;
close_in ch;
let rec loop () =
  let (w1, v1) = Q.find_min !q in
  let q' = Q.remove_min !q in
  let (w2, v2) = Q.find_min q' in
  q := Q.remove_min q';
  q := Q.add (w1 + w2, Node (v1, v2)) !q;
  loop ()
in
try
  loop ()
with Not_found ->
let (_, v) = Q.find_min !q in
let rec traverse l n v =
  match v with
    Leaf i ->
      Format.eprintf "%d/%d %d@." n l i
  | Node (v1, v2) ->
      traverse (l + 1) (2 * n) v1;
      traverse (l + 1) (2 * n + 1) v2
in
traverse 0 0 v;
let rec count l v =
  match v with
    Leaf i        -> l * Hashtbl.find h i
  | Node (v1, v2) -> count (l + 1) v1 + count (l + 1) v2
in
Format.eprintf "==> %d@." (count 0 v)

(*
==> 43 Mo
~ 1byte/node

==> 30Mo when dividing values by 5

(Now: 59 Mo)
*)
