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

module Str : sig
  type t
  val make : string -> t
  val value : t -> string
end

module type ELEMENT = sig
  type t
  val iter : (t -> unit) -> unit
  val tag : t -> Str.t -> Str.t
  val iter_tags : t -> (Str.t -> Str.t -> unit) -> unit
end

module Node : sig
  include ELEMENT
  val lat : t -> int
  val lon : t -> int
end

module Way : sig
  include ELEMENT
  val length : t -> int
  val nth : t -> int -> Node.t
end
