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

let keep_temp = Debug.make "temp" "keep temporary files" []

(****)

let rec unique_rec nm nm' i =
  if Sys.file_exists nm' then
    unique_rec nm (nm ^ "_" ^ string_of_int i) (i + 1)
  else
    nm'

let unique nm = unique_rec nm nm 0

(****)

(*
type ('a, 'b) output_stream =
  { ch : Unix.file_descr;
    map_incr : int;
    mutable len : int;
    mutable ar : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t }

let open_out nm ?temp map_incr kind =
  let temp = temp <> None in
  let nm = if temp then unique nm else nm in
  let flags =
    Unix.([O_RDWR; O_CREAT] @ if temp then [O_EXCL] else [O_CREAT; O_TRUNC]) in
  let ch = Unix.openfile nm flags 0o600 in
  if temp && not (keep_temp ()) then Unix.unlink nm;
  { ch = ch; map_incr = map_incr; len = 0;
    ar = Bigarray.Array1.map_file ch kind Bigarray.c_layout true map_incr }

let resize s l =
  s.len <- l;
  let l' = ref (Bigarray.Array1.dim s.ar) in
  if !l' < l then begin
    while !l' < l do l' := !l' + s.map_incr done;
    let kind = Bigarray.Array1.kind s.ar in
    s.ar <- Bigarray.Array1.map_file s.ch kind Bigarray.c_layout true !l'
  end

let output_array s = s.ar

let close_out s = Unix.ftruncate s.ch s.len; Unix.close s.ch
*)

(****)

type ('a, 'b) kind = ('a, 'b) Bigarray.kind * int

let char = (Bigarray.char, 1)
let int8_unsigned = (Bigarray.int8_unsigned, 1)
let int16_unsigned = (Bigarray.int16_unsigned, 2)
let int32 = (Bigarray.int32, 4)
let int = (Bigarray.int, 8)

(****)

type ('a, 'b) output_stream =
  { ch : Unix.file_descr;
    map_incr : int;
    mutable len : int;
    mutable ar : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t;
    element_size : int }

let open_out nm ?temp map_incr (kind, element_size) =
  let temp = temp <> None in
  let nm = if temp then unique nm else nm in
  let flags =
    Unix.([O_RDWR; O_CREAT] @ if temp then [O_EXCL] else [O_CREAT; O_TRUNC]) in
  let ch = Unix.openfile nm flags 0o600 in
  if temp then Unix.unlink nm;
  { ch = ch; map_incr = map_incr; len = 0;
    ar = Bigarray.Array1.map_file ch kind Bigarray.c_layout true map_incr;
    element_size = element_size }

let resize s l =
  s.len <- l;
  let l' = ref (Bigarray.Array1.dim s.ar) in
  if !l' < l then begin
    while !l' < l do l' := !l' + s.map_incr done;
    let kind = Bigarray.Array1.kind s.ar in
    s.ar <- Bigarray.Array1.map_file s.ch kind Bigarray.c_layout true !l'
  end

let output_array s = s.ar

let close_out s = Unix.ftruncate s.ch (s.len * s.element_size); Unix.close s.ch

(****)

type ('a, 'b) t = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

let open_in nm kind =
  let flags = [Unix.O_RDWR] in
  let ch = Unix.openfile nm flags 0o600 in
  let a = Bigarray.Array1.map_file ch kind Bigarray.c_layout true (-1) in
  Unix.close ch;
  a

let array a = a

let freeze s =
  Unix.ftruncate s.ch s.len;
  let kind = Bigarray.Array1.kind s.ar in
  let a = Bigarray.Array1.map_file s.ch kind Bigarray.c_layout true (-1) in
  Unix.close s.ch;
  a
