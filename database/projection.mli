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

val filter : ?o:Column.spec -> Column.t -> int -> Column.t

val filter_pred : ?o:Column.spec -> Column.t -> (int -> bool) -> Column.t

val filter_pred_2 :
  ?o:Column.spec -> Column.t -> Column.t -> (int -> int -> bool) -> Column.t

val project : ?o:Column.spec -> Column.t -> Column.t -> Column.t
(*
   [project index input]
   The index must be sorted. There can be duplicated entries in the index.
*)

(*
val project_unique : Column.t -> Column.t -> Column.output_stream
(*
   [project_unique index input]
   Both the index and the input table must be sorted.
*)
*)

val inter : ?o:Column.spec -> Column.t -> Column.t -> Column.t
val diff : ?o:Column.spec -> Column.t -> Column.t -> Column.t
