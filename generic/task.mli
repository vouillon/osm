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
type 'a future

val spawn : (unit -> 'a) -> 'a t
val funct : ('a -> 'b -> 'c) -> 'a t -> 'b -> 'c future
val wait : 'a future -> 'a
val kill : 'a t -> unit

val map : 'a list -> ('a -> 'b future) -> ('b -> 'c) -> 'c list
val iter : 'a list -> ('a -> 'b future) -> ('b -> unit) -> unit
val iteri : 'a list -> ('a -> ('b * 'c future)) -> ('b -> 'c -> unit) -> unit
val iter_ordered : 'a list -> ('a -> 'b future) -> ('b -> unit) -> unit
val iteri_ordered :
  'a list -> ('a -> ('b * 'c future)) -> ('b -> 'c -> unit) -> unit

type scheduler

val scheduler : unit -> scheduler
val async : scheduler -> 'a future -> ('a -> unit) -> unit
val run : scheduler -> unit

val get_processor_count : unit -> int
val set_processor_count : int -> unit
