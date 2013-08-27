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

val set_database : string -> unit
val file_in_database : string -> string

(****)

type spec

val temp : string -> spec
val named : string -> string -> spec

(****)

type t

val open_in : spec -> t
val identity : ?offset:int -> int -> t

val length : t -> int
val get : t -> int -> int
val get_sequence : t -> int -> int -> int array

val decode : t -> int -> int -> int array -> int -> unit
val decode_to_bigarray :
  t -> int -> int ->
  (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t -> int -> unit
val block_size : int

type input_stream

val stream : ?first:int -> ?last:int -> t -> input_stream
val read : input_stream -> int (* returns [max_int] at end of stream *)
val seek : input_stream -> int -> unit
val position : input_stream -> int
val at_end_of_stream : input_stream -> bool
  (* We are at the end of the stream *)
val beyond_end_of_stream : input_stream -> bool
  (* We are at the end of the stream and have read the sentinel *)

(****)

type output_stream

val open_out : spec -> output_stream
val append : output_stream -> int -> unit
val close_out : output_stream -> unit

val freeze : output_stream -> t

(****)

val with_spec : (output_stream -> 'a) -> string -> (?o:spec -> 'a)
val with_spec_2 :
  (output_stream -> output_stream -> 'a) -> string -> string ->
  (?o1:spec -> ?o2:spec -> 'a)

(****)

val is_column : string -> bool
