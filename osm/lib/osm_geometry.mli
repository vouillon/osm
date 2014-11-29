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

val distance : int -> int -> int -> int -> int

val pi : float

(****)

val lat_to_y : float -> float
val y_to_lat : float -> float

(****)

val is_simple_polygon : int array -> int array -> bool
val point_on_polygon_boundary : int -> int -> int array -> int array -> bool
val point_in_polygon : int -> int -> int array -> int array -> bool
  (* Unspecified when point on polygon boundary *)
val polygon_area : int array -> int array -> int
val polygon_area_float : float array -> float array -> float
val polygon_in_polygon :
  int array * int array -> int array * int array -> bool
     (* Assumes that polygons are not overlapping... *)
val polygon_mostly_in_polygon :
  int array * int array -> int array * int array -> bool

(***)

val hilbert_coordinate : int -> int -> int
      (* Any of the arguments: 32 bits; the other: 31 bits *)
val z_order : int -> int -> int
      (* First argument: 31 bits; second argument: 32 bits *)
