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

let _ = Printexc.record_backtrace true
let _ = Column.set_database "/tmp/osm"

(****)

open Osm_contraction

let _ =
  let st = load () in
  let dij_state = init_dijkstra () in
let t1 = Unix.gettimeofday () in
  iter_remaining st
    (fun _ i ->
       st.priority.{i} <- fst (prepare_node_contraction dij_state st i));
let t1 = Unix.gettimeofday () -. t1 in
Format.eprintf "====== %f@." t1;

  while st.remaining_count > 0 do

(*
    check_edges st;
*)

(*Format.eprintf "%d@." st.remaining_count;*)
    let better p q =
      st.priority.{p} < st.priority.{q} ||
      (st.priority.{p} = st.priority.{q} &&
       (Hashtbl.hash p < Hashtbl.hash q ||
        (Hashtbl.hash p = Hashtbl.hash q && p < q)))
    in
(*
    let better p q =
      let b1 = better p q in
      let b2 = better q p in
      assert (b1 or b2);
      assert ((not b1) or (not b2));
      b1
    in
*)
let t1 = Unix.gettimeofday () in
    let independent = int_array (Array1.dim st.remaining) in
    let count = ref 0 in
    iter_remaining st
      (fun i0 i ->
         let best = ref true in
         for j = first_edge st i to last_edge st i do
           best := !best && better i (st.target.{j})
         done;
         if !best then
           for j = first_edge st i to last_edge st i do
             let t = st.target.{j} in
             for k = first_edge st t to last_edge st t do
               let t = st.target.{k} in
               if t <> i then best := !best && better i t
             done
           done;
     (*
if st.remaining_count < 3000 then
         Format.eprintf "%d / %f@." i st.priority.{i};
     *)
         if !best then begin
           independent.{!count} <- i;
           incr count
         end else
           st.remaining.{i0 - !count} <- i);
    st.remaining_count <- st.remaining_count - !count;
let t1 = Unix.gettimeofday () -. t1 in
prerr_endline "===";
let t2 = Unix.gettimeofday () in
    for i = 0 to !count - 1 do
      contract_node dij_state st independent.{i}
    done;
let t2 = Unix.gettimeofday () -. t2 in
prerr_endline "===";
let t3 = Unix.gettimeofday () in
(*
    for i = 0 to !count - 1 do
      update_neighbours dij_state st independent.{i}
    done;
*)
    for i = 0 to !count - 1 do
      update_neighbour_depths st independent.{i}
    done;
    let updated = Bitvect.make (Array1.dim st.first_edge) in
    for i = 0 to !count - 1 do
      update_neighbour_priorities updated dij_state st independent.{i}
    done;
    (*Delete contracted nodes?*)
    for i = 0 to !count - 1 do
      st.edge_count.{independent.{i}} <- 0
    done;
let t3 = Unix.gettimeofday () -. t3 in
Format.eprintf "%d %f %f %f (%f)@." !count t1 t2 t3 (1000000. *. t3 /. float !count);
(*
*)
let total_arity = ref 0 in
iter_remaining st (fun _ i -> total_arity := !total_arity + st.edge_count.{i});
Format.eprintf "average arity: %f@." (float !total_arity /. float st.remaining_count);
Format.eprintf "remains: %d@." st.remaining_count;

(*
iter_remaining st (fun _ i ->
  if st.edge_count.{i} > 20 then begin
for j = first_edge st i to last_edge st i do
Format.eprintf "%d --> %d (via %d) %d@." i st.target.{j} st.shortcut_node.{j} st.weight.{j}
done
end);
*)

incr trace
  done;
  let source = Column.freeze st.out_source in
  let target = Column.freeze st.out_target in
  let weight = Column.freeze st.out_weight in
  let flags = Column.freeze st.out_flags in
  let shortcut = Column.freeze st.out_shortcut in

  let column nm = Column.named "highway/routing/ordered" nm in
  let (source, order) =
    Sorting.perform ~o1:(column "source")
      source (Column.identity (Column.length source)) in
  let rev_order =
    Sorting.permute order (Column.identity (Column.length order)) in
  ignore (Sorting.permute ~o:(column "target") rev_order target);
  ignore (Sorting.permute ~o:(column "weight") rev_order weight);
  ignore (Sorting.permute ~o:(column "flags") rev_order flags);
  ignore (Sorting.permute ~o:(column "shortcut") rev_order shortcut);
  ignore (Column_ops.build_index ~o:(column "index") source)
