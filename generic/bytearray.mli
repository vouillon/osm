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

type t =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

val create : int -> t

val length : t -> int

val to_string : t -> string

val of_string : string -> t

val sub : t -> int -> int -> string

val blit_from_string : string -> int -> t -> int -> int -> unit

val blit_to_string : t -> int -> string -> int -> int -> unit

val prefix : t -> t -> int -> bool

val marshal : 'a -> Marshal.extern_flags list -> t

val unmarshal : t -> int -> 'a

val marshal_to_buffer : t -> int -> 'a -> Marshal.extern_flags list -> int
