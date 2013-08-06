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

type ('a, 'b) t =
  { ch : Unix.file_descr;
    mutable ar : ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t }

val int64 : (int64, Bigarray.int64_elt) Bigarray.kind
val int32 : (int32, Bigarray.int32_elt) Bigarray.kind
val int : (int, Bigarray.int_elt) Bigarray.kind

val make : ?len:int -> string -> ('a, 'b) Bigarray.kind -> ('a, 'b) t
val extend : ('a, 'b) t -> int -> unit

(****)

val sort :
  (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  unit
