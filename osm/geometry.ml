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

(* http://en.wikipedia.org/wiki/Geographical_distance *)
(* http://geographiclib.sourceforge.net/1.1/geodesic.html *)

let haversin th = let s = sin (th /. 2.) in s *. s
let pi = acos (-1.)
let conv_factor = pi /. 180. /. 10_000_000.
let to_rad a = float a *. conv_factor
let r = 6371. *. 1e6

let distance lat1 lon1 lat2 lon2 =
  let lat1 = to_rad lat1 in
  let lon1 = to_rad lon1 in
  let lat2 = to_rad lat2 in
  let lon2 = to_rad lon2 in
  let a =
    haversin (lat2 -. lat1) +.
    cos (lat1) *. cos(lat2) *. haversin (lon2 -. lon1) in
  truncate (2. *. r *. asin (sqrt a))

(*
function lat2y(a) { return 180/Math.PI * Math.log(Math.tan(Math.PI/4+a*(Math.PI/180)/2)); }
*)

(****)

let coeff_1 = 180. /. pi *. 10_000_000.
let pi_4 = pi /. 4.
let coeff_2 = pi /. 180. /. 10_000_000. /. 2.

let lat_to_y lat = coeff_1 *. log (tan (pi_4 +. lat *. coeff_2))

let y_to_lat y = (atan (exp (y /. coeff_1)) -. pi_4) /. coeff_2

(****)

let in_between a b (c : int) = (* c is between a and b *)
  if a <= b then
    (a <= c && c <= b)
  else
    (b <= c && c <= a)

let min x y : int = if x < y then x else y
let max x y : int = if x < y then y else x

let interval_intersect x1 x1' x2 x2' =
  min x1 x1' <= max x2 x2' && min x2 x2' <= max x1 x1'

let segment_intersect x1 y1 x1' y1' x2 y2 x2' y2' =
  let dx1 = x1' - x1 in
  let dy1 = y1' - y1 in
  let dx2 = x2' - x2 in
  let dy2 = y2' - y2 in
  let dx = x2 - x1 in
  let dy = y2 - y1 in
  let d = dx1 * dy2 - dx2 * dy1 in
  let d1 = dx * dy2 - dx2 * dy in
  let d2 = dx * dy1 - dx1 * dy in
  if d <> 0 then begin
    in_between 0 d d1 && in_between 0 d d2
  end else begin
    (* Parallel segments *)
    d1 = 0 && d2 = 0 &&
    begin
      (* Colinear segments *)
      if dx1 = 0 && dx2 = 0 then
        (* Vertical segments *)
        interval_intersect y1 y1' y2 y2'
      else
(*(Format.eprintf "XXX %d %d %d %d@." x1 x1' x2 x2';*)
        interval_intersect x1 x1' x2 x2'
    end
  end

(*
Simple polygons:
  - consecutive segments do not overlap
     dx1*dx2 + dy1* dy2 > 0  ||  dx1*dy2 <> dx2*dy1
  - non-consecutive segments do not intersect
*)

let segment_intersect x1 y1 x1' y1' x2 y2 x2' y2' =
  interval_intersect x1 x1' x2 x2'
    &&
  interval_intersect y1 y1' y2 y2'
    &&
  segment_intersect x1 y1 x1' y1' x2 y2 x2' y2'

(* XXX Use faster algorithm for detecting intersections? *)
let is_simple_polygon px py =
  let l = Array.length px - 1 in
  let res = ref true in
  for i = 1 to l - 1 do
    (* Non-consecutive segments should not intersect *)
    for j = 0 to i - 2 do
      if
        (j > 0 || i < l - 1) &&
        segment_intersect
          px.(i) py.(i) px.(i + 1) py.(i + 1)
          px.(j) py.(j) px.(j + 1) py.(j + 1)
      then
(Format.eprintf "I %d %d: %d %d %d %d %d %d %d %d@." i j
          px.(i) py.(i) px.(i + 1) py.(i + 1)
          px.(j) py.(j) px.(j + 1) py.(j + 1)
;
        res := false
)
    done;
    (* Consecutive segments should not overlap *)
    let dx1 = px.(i + 1) - px.(i) in
    let dy1 = py.(i + 1) - py.(i) in
    let dx2 = px.(i) - px.(i - 1) in
    let dy2 = py.(i) - py.(i - 1) in
    if dx1 * dx2 + dy1 * dy2 <= 0 && dx1 * dy2 = dx2 * dy1 then
(prerr_endline "O";
 res := false
)
  done;
  !res

let point_on_segment x y x1 y1 x1' y1' =
  in_between x1 x1' x &&
  in_between y1 y1' y &&
  let dx1 = x1' - x1 in
  let dy1 = y1' - y1 in
  let dx = x - x1 in
  let dy = y - y1 in
  let d1 = dx * dy1 - dx1 * dy in
  d1 = 0

let point_on_polygon_boundary x y px py =
  let l = Array.length px - 1 in
  let res = ref false in
  for i = 0 to l - 1 do
    if point_on_segment x y px.(i) py.(i) px.(i + 1) py.(i + 1) then
      res := true
  done;
  !res

let point_in_polygon x y px py =
  let n = ref false in
  let l = Array.length px - 1 in
  let pi = ref (py.(0) >= y) in
  for i = 0 to l - 1 do
    let j = i + 1 in
    let pj = py.(j) >= y in
    if (!pi && not pj) || (not !pi && pj) then begin
      let d = 
        (px.(j) - px.(i)) * (y - py.(i)) - (x - px.(i)) * (py.(j) - py.(i))
      in
      let p = (d >= 0) in
      if (p && not !pi) || (not p && !pi) then
        n := not !n
    end;
    pi := pj
  done;
  !n

let midpoint_on_polygon_boundary x1 y1 x2 y2 px py =
  let x = x1 + x2 in
  let y = y1 + y2 in
  let l = Array.length px - 1 in
  let res = ref false in
  for i = 0 to l - 1 do
    if
      point_on_segment x y
        (2 * px.(i)) (2 * py.(i)) (2 * px.(i + 1)) (2 * py.(i + 1))
    then
      res := true
  done;
  !res

let midpoint_in_polygon x1 y1 x2 y2 px py =
  let x = x1 + x2 in
  let y = y1 + y2 in
  let n = ref false in
  let l = Array.length px - 1 in
  let pi = ref (2 * py.(0) >= y) in
  for i = 0 to l - 1 do
    let j = i + 1 in
    let pj = 2 * py.(j) >= y in
    if !pi <> pj then begin
      let d = 
        (px.(j) - px.(i)) * (y - 2 * py.(i)) -
        (x - 2 * px.(i)) * (py.(j) - py.(i))
      in
      if (d >= 0) = !pi then
        n := not !n
    end;
    pi := pj
  done;
  !n

let polygon_area px py =
  let l = Array.length px - 1 in
  let a = ref 0 in
  let x0 = px.(0) in
  let y0 = py.(0) in
  for i = 0 to l - 1 do
    let dx1 = px.(i) - x0 in
    let dy1 = py.(i) - y0 in
    let dx2 = px.(i + 1) - x0 in
    let dy2 = py.(i + 1) - y0 in
    a := !a + dx1 * dy2 - dx2 * dy1
  done;
  (!a + 1) / 2

let polygon_area_float px py =
  let l = Array.length px - 1 in
  let a = ref 0. in
  let x0 = px.(0) in
  let y0 = py.(0) in
  for i = 0 to l - 1 do
    let dx1 = px.(i) -. x0 in
    let dy1 = py.(i) -. y0 in
    let dx2 = px.(i + 1) -. x0 in
    let dy2 = py.(i + 1) -. y0 in
    a := !a +. dx1 *. dy2 -. dx2 *. dy1
  done;
  !a /. 2.

(*
polygon containment:
- assumption :
  either contained or with disjoint interior;
  in fact, edge intersection only on edge extremities, or same edge
==> we have a point inside but not on boundary
==> or, we have a segment midpoint inside but not on boundary
*)

exception Success

(* Slow version: in most cases, returns true when some part of the
   first polygon is in the second polygon *)
let polygon_in_polygon (px, py) (px', py') =
  try
    let l = Array.length px in
    let on_boundary =
      Array.init l
        (fun i ->
           if point_on_polygon_boundary px.(i) py.(i) px' py' then
             true
           else if point_in_polygon px.(i) py.(i) px' py' then
             raise Success
           else
             false)
    in
    for i = 0 to l - 2 do
      if
        on_boundary.(i) && on_boundary.(i + 1) &&
        not (midpoint_on_polygon_boundary
               px.(i) py.(i) px.(i + 1) py.(i + 1) px' py') &&
        midpoint_in_polygon
          px.(i) py.(i) px.(i + 1) py.(i + 1) px' py'
      then
        raise Success
    done;
    false
  with Success ->
    true

exception Failure

(* Fast version: unspecified result when polygon overlaps *)
let polygon_in_polygon (px, py) (px', py') =
  try
    let l = Array.length px in
    for i = 0 to l - 1 do
      if not (point_on_polygon_boundary px.(i) py.(i) px' py') then begin
        if point_in_polygon px.(i) py.(i) px' py' then
          raise Success
        else
          raise Failure
      end
    done;
    for i = 0 to l - 2 do
      if
        not (midpoint_on_polygon_boundary
               px.(i) py.(i) px.(i + 1) py.(i + 1) px' py')
      then begin
        if
          midpoint_in_polygon
            px.(i) py.(i) px.(i + 1) py.(i + 1) px' py'
        then
          raise Success
        else
          raise Failure
      end;
    done;
    false
  with
    Success ->
      true
  | Failure ->
      false

let polygon_mostly_in_polygon (px, py) (px', py') =
  let l = Array.length px in
  let on_boundary =
    Array.init l
      (fun i -> point_on_polygon_boundary px.(i) py.(i) px' py')
  in
  let total = ref 0 in
  let inside = ref 0 in
  for i = 0 to l - 2 do
    if not on_boundary.(i) then begin
      if point_in_polygon px.(i) py.(i) px' py' then
        incr inside;
      incr total
    end
  done;
  if !total = 0 then begin
    for i = 0 to l - 2 do
      if
        on_boundary.(i) && on_boundary.(i + 1) &&
        not (midpoint_on_polygon_boundary
               px.(i) py.(i) px.(i + 1) py.(i + 1) px' py')
      then begin
        if
          midpoint_in_polygon
            px.(i) py.(i) px.(i + 1) py.(i + 1) px' py'
        then
          incr inside;
        incr total
      end
    done
  end;
(*Format.eprintf "POS: %d/%d (%d)@." !inside !total l;*)
  !total = 0 || float !inside /. float !total >= 0.7

(****)

let dilate x =
  let x = (x lor (x lsl 16)) land 0xFFFF0000FFFF in
  let x = (x lor (x lsl 8)) land 0xFF00FF00FF00FF in
  let x = (x lor (x lsl 4)) land 0xF0F0F0F0F0F0F0F in
  let x = (x lor (x lsl 2)) land 0x333333333333333 in
  let x = (x lor (x lsl 1)) land 0x555555555555555 in
  x

let hilbert_coordinate x y =
  let mask = 0xffffffff in
  let heven = x lxor y in
  let notx = (lnot x) land mask in
  let noty = (lnot y) land mask in
  let temp = notx lxor y in
  let v1 = ref 0 in
  let v0 = ref 0 in
  for k = 1 to 31 do
    v1 := ((!v1 land heven) lor ((!v0 lxor noty) land temp)) lsr 1;
    v0 :=
      ((!v0 land (!v1 lxor notx)) lor ((lnot !v0) land (!v1 lxor noty))) lsr 1;
  done;
  let hodd = ((lnot !v0) land (!v1 lxor x)) lor (!v0 land (!v1 lxor noty)) in
  ((dilate hodd) lsl 1) lor (dilate heven)

(* Faster to compute but not as good as Hilbert coordinates *)
let z_order x y = (((dilate x) lsl 1) lor (dilate y))
