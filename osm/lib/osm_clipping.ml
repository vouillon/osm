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

(**** Sutherland–Hodgman algorithm ****)

let inter x0 x y i =
  (y.(i + 1) *. (x.(i) -. x0) -. y.(i) *. (x.(i + 1) -. x0))
    /. (x.(i) -. x.(i + 1))

let clip_ring x0 x y =
  let lst = ref [] in
  let start = ref 0 in
  let start_y = ref 0. in
  for i = 0 to Array.length x - 2 do
    if x.(i) > x0 && x.(i + 1) <= x0 then begin
      (* end right *)
      lst := (`Right, [(!start, i)], !start_y, inter x0 x y i) :: !lst;
      start := i + 1;
      start_y := inter x0 x y i
    end;
    if x.(i) < x0 && x.(i + 1) >= x0 then begin
      (* end left *)
      lst := (`Left, [(!start, i)], !start_y, inter x0 x y i) :: !lst;
      start := i + 1;
      start_y := inter x0 x y i
    end;
    if x.(i) <= x0 && x.(i + 1) > x0 then begin
      (* start right *)
      start := i + 1;
      start_y := inter x0 x y i
    end;
    if x.(i) >= x0 && x.(i + 1) < x0 then begin
      (* start left *)
      start := i + 1;
      start_y := inter x0 x y i
    end
  done;
  let lst = List.rev !lst in
  match lst with
    [] ->
      if x.(0) >= x0 then `Right else `Left
  | [(`Left, _, _, _)] ->
      `Left
  | [(`Right, _, _, _)] ->
      `Right
  | (side, l, y1, y2) :: rem ->
      let last = Array.length x - 2 in
      if last >= !start then 
        `Inter ((side, (!start, last) :: l, !start_y, y2) :: rem)
      else
        `Inter ((side, l, !start_y, y2) :: rem)

let rec blit x' y' l x y i =
  match l with
    [] ->
      i
  | (i1, i2) :: rem ->
      let n = i2 - i1 + 1 in
      Array.blit x' i1 x i n;
      Array.blit y' i1 y i n;
      blit x' y' rem x y (i + n)

let blit_len l = List.fold_left (fun n (i1, i2) -> n + i2 - i1 + 1) 0 l

let rec flatten_rec x0 y0 lst x y i =
  match lst with
    [] ->
      assert (i = Array.length x - 1);
      x.(i) <- x.(0);
      y.(i) <- y.(0)
  | (l, y1, y2, x', y') :: rem ->
      let i =
        if y1 <> y0 then begin
          x.(i) <- x0;
          y.(i) <- y1;
          i + 1
        end else
          i
      in
      let i = blit x' y' l x y i in
      x.(i) <- x0;
      y.(i) <- y2;
      flatten_rec x0 y2 rem x y (i + 1)

let rec flatten_len y0 lst i =
  match lst with
    [] ->
      i + 1
  | (l, y1, y2, x', y') :: rem ->
      let i = if y1 <> y0 then i + 1 else i in
      let i = i + blit_len l in
      flatten_len y2 rem (i + 1)

let rec last_coord lst =
  match lst with
    [(l, y1, y2, x', y')] -> y2
  | _ :: rem              -> last_coord rem
  | []                    -> assert false

let flatten x0 lst =
  let y0 = last_coord lst in
  let len = flatten_len y0 lst 0 in
  let x = Array.make len 0. in
  let y = Array.make len 0. in
  flatten_rec x0 y0 lst x y 0;
  (x, y)

let recompose x0 partial lst =
  let partial = Array.of_list partial in
  let starts = Array.mapi (fun i (_, y1, _, _, _) -> (y1, i)) partial in
  let ends = Array.mapi (fun i (_, _, y2, _, _) -> (y2, i)) partial in
  Array.stable_sort (fun (y, _) (y', _) -> compare y y') starts;
  Array.stable_sort (fun (y, _) (y', _) -> compare y y') ends;
  let len = Array.length partial in
  let pred = Array.make len (-1) in
  for i = 0 to len - 1 do
    pred.(snd starts.(i)) <- snd ends.(i)
  done;
  let rec concat i0 i l =
    let l = partial.(i) :: l in
    let j = pred.(i) in
    pred.(i) <- -1;
    if j <> i0 then concat i0 j l else l
  in
  let lst = ref lst in
  for i = 0 to len - 1 do
    if pred.(i) <> -1 then
      let (x, y) = flatten x0 (concat i i []) in
      (* We have overlapping edges, so we can get degenerate polygons... *)
      if Array.length x > 3 then lst := (x, y) :: !lst
  done;
  !lst

let perform x0 l =
  let left = ref [] in
  let right = ref [] in
  let left_partial = ref [] in
  let right_partial = ref [] in
  List.iter
    (fun (x, y) ->
       match clip_ring x0 x y with
         `Left    -> left := (x, y) :: !left
       | `Right   -> right := (x, y) :: !right
       | `Inter l ->
           List.iter
             (fun (side, l, y1, y2) ->
                match side with
                  `Left ->
                    left_partial := (l, y1, y2, x, y) :: !left_partial
                | `Right ->
                    right_partial := (l, y1, y2, x, y) :: !right_partial)
             l)
    l;
  (recompose x0 !left_partial !left, recompose x0 !right_partial !right)
