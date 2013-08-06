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

val set_msg : string -> unit
val hide_msg : unit -> unit
val show_msg : unit -> unit
val enable_messages : bool -> unit

val progress_bar : float -> string

val set_warning_location : string -> unit
val reset_warning_location : unit -> unit
val print_warning : string -> unit

val fail : string -> 'a

val title : string -> unit

module Timer : sig
  type t
  val start : unit -> t
  val stop : t -> float
end

module Utimer : sig
  type t
  val start : unit -> t
  val stop : t -> float
end

module IntSet : Set.S with type elt = int
module StringSet : Set.S with type elt = string

module ListTbl : sig
  type ('a, 'b) t
  val create : int -> ('a, 'b) t
  val add : ('a, 'b) t -> 'a -> 'b -> unit
  val find : ('a, 'b) t -> 'a -> 'b list
  val mem : ('a, 'b) t -> 'a -> bool
  val iter : ('a -> 'b list -> unit) -> ('a, 'b) t -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t
  val remove : ('a, 'b) t -> 'a -> ('b -> bool) -> unit
end

module StringTbl : Hashtbl.S with type key = string

val array_extend : 'a array -> int -> 'a -> 'a array
val string_extend : string -> int -> char -> string

val print_list :
  (Format.formatter -> 'a -> unit) -> string ->
  Format.formatter -> 'a list -> unit

val make_directories : string -> unit
(* Make sure that the directory containing the file given in argument
   exists. *)

module BitVect : sig
  type t
  val make : int -> bool -> t
  val test : t -> int -> bool
  val set : t -> int -> unit
  val clear : t -> int -> unit
  val sub : t -> int -> int -> t
  val copy : t -> t
  val extend : t -> int -> bool -> t
  val implies : t -> t -> bool
  val lnot : t -> t
  val (land) : t -> t -> t
  val (lor) : t -> t -> t
end

val sort_and_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
val compare_pair :
  ('a -> 'b -> int) -> ('c -> 'd -> int) -> 'a * 'c -> 'b * 'd -> int
val compare_list : ('a -> 'b -> int) -> 'a list -> 'b list -> int
val group : ('a -> 'a -> int) -> ('a * 'b) list -> ('a * 'b list) list

module Union_find : sig
  type 'a t
  val elt : 'a -> 'a t
  val get : 'a t -> 'a
  val merge : 'a t -> 'a t -> ('a -> 'a -> 'a) -> unit
end

(*
val trim : string -> string
*)
