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

(*
XXX Optimize!!!
==> Stall on demand
==> Use binary heap
==> Do not use lists for path expansion
...
*)

module Queue =
  Pqueue.Make
    (struct
       type t = int * int * int
       let compare ((x : int), _, _) (y, _, _) = compare x y
     end)

let find_dist dists i =
  try
    fst (Hashtbl.find dists i)
  with Not_found ->
    max_int / 2

type t =
  { edge : Column.t;
    target : Column.t;
    weight : Column.t;
    flags : Column.t;
    shortcut : Column.t;
    mutable n : int;
    mutable best : int }

let rec advance st q1 dists1 dir1 q2 dists2 dir2 =
  let (d1, n1, n1') =
    try Queue.find_min q1 with Not_found -> (max_int, -1, -1) in
(*
Format.eprintf "%d: %d@." n1 d1;
*)
  if d1 >= st.best then begin
    if not (Queue.is_empty q2) then
      advance st q2 dists2 dir2 Queue.empty dists1 dir1
  end else begin
    let q1 = Queue.remove_min q1 in
    let d = find_dist dists1 n1 in
    if d1 >= d then
      advance st q2 dists2 dir2 q1 dists1 dir1
    else begin
      Hashtbl.replace dists1 n1 (d1, n1');
      let d = d1 + find_dist dists2 n1 in
      if d < st.best then begin st.best <- d; st.n <- n1 end;
      let q1 = ref q1 in
      for i = Column.get st.edge n1 to Column.get st.edge (n1 + 1) - 1 do
(*
Format.eprintf "  ->(%d) %d@." (Column.get st.weight i) (Column.get st.target i);
*)
        if Column.get st.flags i land dir1 <> 0 then
          q1 :=
            Queue.add (d1 + Column.get st.weight i, Column.get st.target i, n1)
            !q1
      done;
      advance st q2 dists2 dir2 !q1 dists1 dir1
    end
  end

let init () =
  let column nm = Column.open_in (Column.named "highway/routing/ordered" nm) in
  { edge = column "index";
    target = column "target";
    weight = column "weight";
    flags = column "flags";
    shortcut = column "shortcut";
    n = -1; best = -1 }

exception Found of int

let rec expand_edge st n n' dir w =
(*Format.eprintf "%d -> %d: %d@." n n' w;*)
  try
    for i = Column.get st.edge n to Column.get st.edge (n + 1) - 1 do
      if
        Column.get st.target i = n' &&
        Column.get st.flags i land dir <> 0 &&
        Column.get st.weight i = w
      then
        raise (Found i)
    done;
    assert false
  with Found i ->
    let s = Column.get st.shortcut i in
    if s >= 0 then begin
      try
        for j = Column.get st.edge s to Column.get st.edge (s + 1) - 1 do
          if
            Column.get st.target j = n &&
            Column.get st.flags j land (3 - dir) <> 0
          then begin
            let w' = Column.get st.weight j in
            for k = Column.get st.edge s to Column.get st.edge (s + 1) - 1 do
              if
                Column.get st.target k = n' &&
                Column.get st.flags k land dir <> 0 &&
                Column.get st.weight k + w' = w
              then
                raise (Found w')
            done
          end
        done;
Format.eprintf "Expected weight from %d to %d through %d: %d@." n n' s w;
for j = Column.get st.edge s to Column.get st.edge (s + 1) - 1 do
Format.eprintf "%d -> %d: %d %d@." s (Column.get st.target j) (Column.get st.weight j) (Column.get st.flags j)
done;
        assert false
      with Found w' ->
(*prerr_endline "AAA";*)
         List.rev (expand_edge st s n (3 - dir) w') @
         s :: expand_edge st s n' dir (w - w')
    end else
      []
  
let rec path st dists n l =
  let (_, n') = Hashtbl.find dists n in
  if n' = -1 then l else path st dists n' (n' :: l)

let rec path st dists n dir l =
  let (_, n') = Hashtbl.find dists n in
  if n' = -1 then l else
  path st dists n' dir
    (n' ::
     expand_edge st n' n dir (find_dist dists n - find_dist dists n') @
     l)

let find st n1 n2 =
let t1 = Unix.gettimeofday () in
  let st = {st with n = -1; best = max_int / 2 - 1} in
  let dists1 = Hashtbl.create 128 in
  let dists2 = Hashtbl.create 128 in
  let q1 = Queue.add (0, n1, -1) Queue.empty in
  let q2 = Queue.add (0, n2, -1) Queue.empty in
  let dir1 = 2 in
  let dir2 = 1 in
  advance st q1 dists1 dir1 q2 dists2 dir2;
let t1 = Unix.gettimeofday () -. t1 in
if st.n = -1 then [] else begin
  Format.eprintf "weight: %d@." st.best;
let t2 = Unix.gettimeofday () in
  let l1 = path st dists1 st.n 2 [] in
  let l2 = path st dists2 st.n 1 [] in
  let l = l1 @ st.n :: List.rev l2 in
let t2 = Unix.gettimeofday () -. t2 in
(*
  List.iter (fun n -> Format.eprintf "%d " n) l;
  Format.eprintf "@.";
*)
Format.eprintf "duration: %f / %f@." t1 t2;
  l
end

(*
let _ = Column.set_database "/tmp/osm"
let _ =
  let st = init () in
  find st (*786655 9*) 1234 45536
*)
