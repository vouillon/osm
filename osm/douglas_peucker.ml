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

let rec perform eps x y =
  let eps2 = eps *. eps in
  let l = Array.length x in
  let x' = Array.make l 0. in
  let y' = Array.make l 0. in
  let j = ref 0 in
  let rec perform_rec i1 i2 =
    let max_dist = ref eps2 in
    let max_i = ref (-1) in
    let dx = x.(i2) -. x.(i1) in
    let dy = y.(i2) -. y.(i1) in
    let d2 = dx *. dx +. dy *. dy in
    for i = i1 + 1 to i2 - 1 do
      let dx' = x.(i) -. x.(i1) in
      let dy' = y.(i) -. y.(i1) in
      let t = dx *. dx' +. dy *. dy' in
      let dist =
        if t <= 0. then
          dx' *. dx' +. dy' *. dy'
        else if t >= d2 then
          let dx'' = x.(i) -. x.(i2) in
          let dy'' = y.(i) -. y.(i2) in
          dx'' *. dx'' +. dy'' *. dy''
        else
          let n = dx *. dy' -. dy *. dx' in
          n *. n /. d2
      in
      if dist > !max_dist then begin
        max_dist := dist;
        max_i := i
      end
    done;
(*
if i1 < i2 - 1 then Format.eprintf "%f %f %f@." !max_dist eps2 (dx *. dx +. dy *. dy);
*)
    if !max_dist > eps2 then begin
      perform_rec i1 !max_i;
      x'.(!j) <- x.(!max_i);
      y'.(!j) <- y.(!max_i);
      incr j;
      perform_rec !max_i i2
    end
  in
  x'.(!j) <- x.(0);
  y'.(!j) <- y.(0);
  incr j;
  perform_rec 0 (l - 1);
  x'.(!j) <- x.(l - 1);
  y'.(!j) <- y.(l - 1);
  incr j;
  (Array.sub x' 0 !j, Array.sub y' 0 !j)

let perform eps x y = if Array.length x <= 2 then (x, y) else perform eps x y

let rec perform_int eps x y =
  let eps2 = eps * eps in
  let l = Array.length x in
  let x' = Array.make l 0 in
  let y' = Array.make l 0 in
  let j = ref 0 in
  let rec perform_rec i1 i2 =
    let max_i = ref (-1) in
    let dx = x.(i2) - x.(i1) in
    let dy = y.(i2) - y.(i1) in
    let d2 = dx * dx + dy * dy in
    let max_dist = ref eps2 in
    for i = i1 + 1 to i2 - 1 do
      let dx' = x.(i) - x.(i1) in
      let dy' = y.(i) - y.(i1) in
      let t = dx * dx' + dy * dy' in
      let dist =
        if t <= 0 then
          (dx' * dx' + dy' * dy')
        else if t >= d2 then
          let dx'' = x.(i) - x.(i2) in
          let dy'' = y.(i) - y.(i2) in
          (dx'' * dx'' + dy'' * dy'')
        else
          let n = dx * dy' - dy * dx' in
          truncate (float n *. float n /. float d2)
      in
(*Format.eprintf "==> %d %d@." dist eps2;*)
      if dist > !max_dist then begin
        max_dist := dist;
        max_i := i
      end
    done;
(*
if i1 < i2 - 1 then Format.eprintf "%f %f %f@." !max_dist eps2 (dx *. dx +. dy *. dy);
*)
    if !max_dist > eps2 then begin
      perform_rec i1 !max_i;
      x'.(!j) <- x.(!max_i);
      y'.(!j) <- y.(!max_i);
      incr j;
      perform_rec !max_i i2
    end
  in
  x'.(!j) <- x.(0);
  y'.(!j) <- y.(0);
  incr j;
  perform_rec 0 (l - 1);
  x'.(!j) <- x.(l - 1);
  y'.(!j) <- y.(l - 1);
  incr j;
  (Array.sub x' 0 !j, Array.sub y' 0 !j)

let perform_int eps x y =
  if Array.length x <= 2 then (x, y) else perform_int eps x y
