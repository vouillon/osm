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

module type S = sig
  type t
  val list : t list
end

module Make (X : S) : sig
  type id = int
  val none : id

  val of_id : id -> X.t
  val to_id : X.t -> id

  type filter = [`Any of string list | `Not of string list]
  type classifier = (string * (filter * X.t) list) list
  val classify : Dictionary.t -> classifier -> int -> int -> id
  val filter : Dictionary.t -> classifier -> int -> int -> bool

  module Partition : sig
    type t
    type group
    val make : unit -> t
    val add_group : t -> X.t list -> group

    type ('a, 'b) collection

    val apply : t -> 'a array -> ('a -> id) -> ('a, unit) collection
    val order_totally : ('a, 'b) collection -> ('a, int * 'b) collection
    val order_by :
      int array -> ('a, 'b) collection -> ('a, int * 'b) collection
    val order_by_group : ('a, 'b) collection -> ('a, group * 'b) collection

    type ('a, 'b) iterator
    val select : ('a, 'b) collection -> ('a, 'b) iterator
    val with_group : group -> ('a, group * 'b) iterator -> ('a, 'b) iterator
    val with_key : int -> ('a, int * 'b) iterator -> ('a, 'b) iterator
    val with_groups :
      first:group -> count: int -> ('a, group * 'b) iterator ->
      ('a, 'b) iterator
    val with_keys :
      first:int -> count:int -> ('a, int * 'b) iterator -> ('a, 'b) iterator
    val iter_by_key :
      (int -> ('a, 'b) iterator -> unit) -> ('a, int * 'b) iterator -> unit
    val iter : ('a, _) iterator -> ('a -> unit) -> unit
  end
end
