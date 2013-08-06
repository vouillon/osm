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

module type API = sig

  type str
  val s : string -> str
  val nil : str

  module Table : sig
    type 'a t
    val create : (string * 'a) list -> 'a t
    val create_set : string list -> unit t
    val mem : 'a t -> str -> bool
    val find : 'a t -> str -> 'a
  end

  module Tags : sig
    type t
    val mem : t -> str -> str -> bool
    val find : t -> str -> str
  end

  type direction = [`BIDIRECTIONAL | `ONEWAY | `OPPOSITE]
  type params =
    { mutable speed : float;
      mutable backward_speed : float;   (* < 0 if same as speed *)
      mutable duration : float;   (* < 0 if not defined *)
      mutable direction : direction }

end

module type PROFILE =
  functor (X : API) -> 
  sig
    open X
    val name : string
    val way : Tags.t -> params -> unit
  end

module Register (F : PROFILE) : sig end

(****)

type direction = [`BIDIRECTIONAL | `ONEWAY | `OPPOSITE]
type params =
  { mutable speed : float;
    mutable backward_speed : float;
    mutable duration : float;   (* < 0 if not defined *)
    mutable direction : direction }

val find : Dictionary.t -> string ->
           (int * int) list -> params
