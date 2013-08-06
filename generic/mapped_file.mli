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

type ('a, 'b) t

val open_in : string -> ('a, 'b) Bigarray.kind -> ('a, 'b) t

val array : ('a, 'b) t -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

(****)

type ('a, 'b) kind

val char : (char, Bigarray.int8_unsigned_elt) kind
val int8_unsigned : (int, Bigarray.int8_unsigned_elt) kind
val int32 : (int32, Bigarray.int32_elt) kind

(****)

type ('a, 'b) output_stream

val open_out :
  string -> ?temp:unit -> int -> ('a, 'b) kind ->
  ('a, 'b) output_stream

val resize : ('a, 'b) output_stream -> int -> unit

val output_array :
  ('a, 'b) output_stream -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

val close_out : ('a, 'b) output_stream -> unit

val freeze : ('a, 'b) output_stream -> ('a, 'b) t
