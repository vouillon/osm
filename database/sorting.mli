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

val perform :
  ?o1:Column.spec -> ?o2:Column.spec ->
  Column.t -> Column.t -> Column.t * Column.t
(* Sort on the first column. The sort is stable. *)

val permute :
  ?o:Column.spec ->
  Column.t -> Column.t -> Column.t
(* Sort on the first column when it is a permutation. The sort is stable. *)

(*
val check_sorted : Column.t -> unit

val check_sorted_unique : Column.t -> unit
*)
