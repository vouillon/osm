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

(*
Leave overflows are supported by just appending empty bboxes

Bounded boxes are stored using signed 32bits, so beware overflows!
*)

module Bbox : sig
  type t =
    { min_lat : int;
      max_lat : int;
      min_lon : int;
      max_lon : int }
  val empty : t
  val union : t -> t -> t
  val add_point : t -> int -> int -> t
  val overlaps : t -> t -> bool
  val contains_point : t -> int -> int -> bool
  val print : Format.formatter -> t -> unit
end

type stream

val open_out : ?node_size:int -> string -> string * stream
val append : stream -> Bbox.t -> unit
val close_out : stream -> unit

type t

val open_in : ?node_size:int -> string -> string * t
val bounding_box : t -> Bbox.t
val find : t -> Bbox.t -> (int -> unit) -> unit

val find_nearest_point :
  t ->
  (Bbox.t -> int -> int -> int) ->
  (int -> int -> int -> (int * 'a) option) ->
  int -> int -> int * 'a
