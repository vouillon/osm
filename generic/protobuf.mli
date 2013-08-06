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

type buf

val buffer_from_substring : string -> int -> int -> buf

val buffer_from_string : string -> buf

val buffer_from_channel : in_channel -> int -> buf

val at_end_of_buffer : buf -> bool

(****)

type typ =
  [`Int32 | `Sint32 | `Int64 | `Sint64 | `String | `Bytes | `Msg | `Packed]

type spec

val spec : (int * typ) list -> spec

type msg

val parse : spec -> buf -> msg

val req_field : (buf -> 'a) -> msg -> int -> 'a
val rep_field : (buf -> 'a) -> msg -> int -> 'a list
val opt_field : (buf -> 'a) -> msg -> int -> 'a option
val opt_field_with_default : (buf -> 'a) -> 'a -> msg -> int -> 'a

val int_field : msg -> int -> int
val int64_field : msg -> int -> int64
val string_field : msg -> int -> string
val raw_field : msg -> int -> buf
val packed_field : msg -> int -> buf

val string : buf -> string
val int : buf -> int
val raw : buf -> buf

val packed_count : typ -> buf -> int
val read_int : buf -> int
val read_sint : buf -> int
val read_sint64 : buf -> Int64.t
val unpack : typ -> (buf -> 'a) -> buf -> 'a list
