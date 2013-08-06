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

type 'a t

val make : (unit -> 'a option) -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val consume : 'a t -> ('a -> unit) -> unit

val group : (int * 'a) t -> (int * 'a list) t

val unique_join :
  ?def1:'a -> (int * 'a) t -> ?def2:'b -> (int * 'b) t -> (int * ('a * 'b)) t
(* Performs a join, assume joined columns contain unique values *)
