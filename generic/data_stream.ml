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

type 'a t = Empty | Cons of 'a * (unit -> 'a t)

let rec make f =
  match f () with
    None   -> Empty
  | Some v -> Cons (v, fun () -> make f)

let rec map f s =
  match s with
    Empty       -> Empty
  | Cons (v, s) -> Cons (f v, fun () -> map f (s ()))

let rec consume s f =
  match s with
    Empty       -> ()
  | Cons (v, s) -> f v; consume (s ()) f

let group s =
  match s with
    Empty ->
      Empty
  | Cons ((v, a), s) ->
      let rec group_rec s v l : (int * 'a list) t =
	match s () with
	  Empty ->
	    Cons ((v, List.rev l), fun () -> Empty)
	| Cons ((v', a), s') ->
	    if v = v' then
	      group_rec s' v (a :: l)
	    else
	      Cons ((v, List.rev l), fun () -> group_rec s' v' [a])
      in
      group_rec s v [a]

let rec unique_join ?def1 s1 ?def2 s2 =
  let rec merge_emp_1 s2 =
    begin match def1 with
      Some def1 -> map (fun (i, v) -> (i, (def1, v))) s2
    | None      -> Empty
    end
  and merge_emp_2 s1 =
    begin match def2 with
      Some def2 -> map (fun (i, v) -> (i, (v, def2))) s1
    | None      -> Empty
    end
  and merge_rec i1 v1 s1 i2 v2 s2 : (int * ('a * 'b)) t =
    if i1 < i2 then begin
      match def2 with
	Some def2 -> Cons ((i1, (v1, def2)), fun () -> refill_1 s1 i2 v2 s2)
      | None      -> refill_1 s1 i2 v2 s2
    end else if i1 > i2 then begin
      match def1 with
	Some def1 -> Cons ((i2, (def1, v2)), fun () -> refill_2 i1 v1 s1 s2)
      | None      -> refill_2 i1 v1 s1 s2
    end else
      Cons ((i1, (v1, v2)), fun () -> start (s1 ()) (s2 ()))
  and start s1 s2 =
    match s1, s2 with
      Empty, _ ->
	merge_emp_1 s2
    | _, Empty ->
	merge_emp_2 s1
    | Cons ((i1, v1), s1), Cons ((i2, v2), s2) ->
	merge_rec i1 v1 s1 i2 v2 s2
  and refill_1 s1 i2 v2 s2 =
    match s1 () with
      Empty               -> merge_emp_1 (Cons ((i2, v2), s2))
    | Cons ((i1, v1), s1) -> merge_rec i1 v1 s1 i2 v2 s2
  and refill_2 i1 v1 s1 s2 =
    match s2 () with
      Empty               -> merge_emp_2 (Cons ((i1, v1), s1))
    | Cons ((i2, v2), s2) -> merge_rec i1 v1 s1 i2 v2 s2
  in
  start s1 s2
