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
TODO:
- use memory mappings
- implement search
- oneway edges


- Dynamic graph: add/remove edges

Dynamic graph
- bucket based
- compress buckets when mostly filled
-


Operations:
- reserve k edges
- remove an edge
- add an edge
- (remove all edges)

Keep track of the usage of each page:
--> too small -> compact/merge
--> too large -> split


Page full --> balance with next page
Next page also full --> split page


Mapping page |-> set of node on this page

Compress:
- copy to a temp location then blit back
- update nodes

*)

let debug = ref false

module Array1 = Bigarray.Array1

let way_ids = Column.open_in (Column.named "highway" "way/id")
let way_id k = Column.get way_ids k

type t =
  { (* Nodes *)
    first_edge : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    edge_count : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    priority : (float, Bigarray.float64_elt, Bigarray.c_layout) Array1.t;
    depth : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    dist : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    remaining : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    node_count : int;
    mutable remaining_count : int;
    (* Edges *)
    target : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    weight : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    flags : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Array1.t;
    original_edges : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    shortcut_node : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    id : (int, Bigarray.int_elt, Bigarray.c_layout) Array1.t;
    (* Output *)
    out_source : Column.output_stream;
    out_target : Column.output_stream;
    out_weight : Column.output_stream;
    out_flags : Column.output_stream;
    out_shortcut : Column.output_stream }

let first_edge st i = st.first_edge.{i}
let last_edge st i = st.first_edge.{i} + st.edge_count.{i} - 1

let forward_edge f = f land 2 <> 0
let backward_edge f = f land 1 <> 0
let reverse_edge f = ((f lsl 1) lor (f lsr 1)) land 3

let show_path st f i =
  let rec follow i =
    let d = st.dist.{i} in
    if d > 0 then begin
      let j = ref (-1) in
      for k = first_edge st i to last_edge st i do
	if
          forward_edge st.flags.{k} &&
          st.dist.{st.target.{k}} + st.weight.{k} = d
        then
          j := k
      done;
if (!j = -1) then begin
      for k = first_edge st i to last_edge st i do
	Format.eprintf "XXX %d: %d %d %d@." st.target.{k} st.dist.{st.target.{k}} st.weight.{k} d
      done;
end;
      Format.fprintf f "--%d(%d)--> %d" (way_id st.id.{!j}) st.weight.{!j} st.target.{!j};
      follow st.target.{!j}
    end
  in
  Format.fprintf f "%d " i;
  follow i

let int_array len = Array1.create Bigarray.int Bigarray.c_layout len
let int8_array len = Array1.create Bigarray.int8_unsigned Bigarray.c_layout len

let load () =
  let c_nodes = Column.open_in (Column.named "highway" "node/idx") in
  let c_in_node = Column.open_in (Column.named "highway" "edge/1bis") in
  let c_out_node = Column.open_in (Column.named "highway" "edge/2bis") in
  let c_weight = Column.open_in (Column.named "highway" "edge/weight_bis") in
  let c_flags = Column.open_in (Column.named "highway" "edge/flags_bis") in
  let c_id = Column.open_in (Column.named "highway" "edge/idx_bis") in

  let first_edge = int_array (Column.length c_nodes) in
  let edge_count = int_array (Column.length c_nodes) in
  let priority =
    Array1.create Bigarray.float64 Bigarray.c_layout (Column.length c_nodes) in
  let depth = int_array (Column.length c_nodes) in
  let dist = int_array (Column.length c_nodes) in
  let remaining = int_array (Column.length c_nodes) in
  Array1.fill depth 0;
  Array1.fill dist max_int;
  for i = 0 to Array1.dim remaining - 1 do
    remaining.{i} <- i
  done;

  let len = Column.length c_in_node + Column.length c_nodes + 10 in
  let target = int_array len in
  let weight = int_array len in
  let flags = int8_array len in
  let original_edges = int_array len in
  let shortcut_node = int_array len in
  let id = int_array len in
  Array1.fill original_edges 1;

  let s_in_node = Column.stream c_in_node in
  let s_out_node = Column.stream c_out_node in
  let s_weight = Column.stream c_weight in
  let s_flags = Column.stream c_flags in
  let s_id = Column.stream c_id in
  let rec loop i j =
    let n = Column.read s_in_node in
    let t = Column.read s_out_node in
    let w = Column.read s_weight in
    let f = Column.read s_flags in
    let way_id = Column.read s_id in
    if n <> max_int then begin
      let rec loop' i j =
	if i < n then begin
	  let i = i + 1 in
          if i > 0 then edge_count.{i - 1} <- j - first_edge.{i - 1};
	  first_edge.{i} <- j + 1;
(*Format.eprintf "%d (%d) %d@." i n j;*)
	  loop' i (j + 1)
	end else
          j
      in
      let j = loop' i j in
      if t <> n then begin
        target.{j} <- t;
        weight.{j} <- w;
        flags.{j} <- f;
        id.{j} <- way_id;
        loop n (j + 1)
      end else
        loop n j
    end else begin
      edge_count.{i} <- j - first_edge.{i};
      i + 1
    end
  in
  let node_count = loop (-1) 0 in
  let open_column nm =
    Column.open_out (Column.named "highway/routing/unordered" nm) in
  let out_source = open_column "source" in
  let out_target = open_column "target" in
  let out_weight = open_column "weight" in
  let out_flags = open_column "flags" in
  let out_shortcut = open_column "shortcut" in
  { first_edge; edge_count; priority; depth; dist; remaining;
    node_count; remaining_count = Column.length c_nodes;
    target; weight; flags; original_edges; shortcut_node; id;
    out_source; out_target; out_weight; out_flags; out_shortcut }

let move_edge st i j =
  st.target.{j} <- st.target.{i};
  st.weight.{j} <- st.weight.{i};
  st.flags.{j} <- st.flags.{i};
  st.original_edges.{j} <- st.original_edges.{i};
  st.shortcut_node.{j} <- st.shortcut_node.{i};
  st.id.{j} <- st.id.{i}

let rec reserve st i n =
(*
Format.eprintf "reserve: %d %d@." i n;
*)
  if i < st.node_count - 1 then begin
    let next = first_edge st (i + 1) in
    let available = next - last_edge st i - 1 in
(*
Format.eprintf "==> %d@." available;
*)
if available < 0 then Format.eprintf "%d %d %d@." i next (last_edge st i);
    assert (available >= 0);
    if available < n then begin
      let delta = n - available in
      reserve st (i + 1) delta;
      for j = last_edge st (i + 1) downto first_edge st (i + 1) do
        move_edge st j (j + delta)
      done;
      st.first_edge.{i + 1} <- st.first_edge.{i + 1} + delta
    end
  end else begin
    assert (i + n <= Array1.dim st.target)
    (*XXX*)
  end

module Heap = Binary_heap

type dij =
  { heap : Heap.t;
    mutable modif : int array;
    mutable pos : int }

let init_dijkstra () =
  { heap = Heap.make (); modif = Array.make 4096 0; pos = 0 }

let push_modif state i =
  let p = state.pos in
  let l = Array.length state.modif in
  if p = l then begin
    let a = Array.make (2 * l) 0 in
    Array.blit state.modif 0 a 0 l;
    state.modif <- a
  end;
  state.modif.(p) <- i;
  state.pos <- p + 1

let trace = ref 0

let dijkstra st state src ignored_node max_dist id i =
  (* Restore distance *)
  for i = 0 to state.pos - 1 do
    st.dist.{state.modif.(i)} <- max_int
  done;
  state.pos <- 0;
  let heap = state.heap in
  Heap.insert heap src 0;
let steps = ref 0 in
let steps' = ref 0 in
if !debug then Format.eprintf ">> max %d@." max_dist;
  let rec loop () =
incr steps;
    let d = Heap.min_weight heap in
    let n = Heap.delete_min heap in
    if n <> -1 then begin
if !debug then Format.eprintf "< %d %d (%d)@." n d st.dist.{n};
      let prev_dist = st.dist.{n} in
      if prev_dist > d then begin
        st.dist.{n} <- d;
if prev_dist = max_int then  incr steps';
        if prev_dist = max_int then push_modif state n;
if !debug then Format.eprintf "> %d children@." (st.edge_count.{n});
        for j = first_edge st n to last_edge st n do
          if forward_edge st.flags.{j} then begin
            let d' = d + st.weight.{j} in
  if !debug then Format.eprintf "!%d %d@." d' (st.dist.{st.target.{j}});
            if d' <= max_dist then begin
              let t = st.target.{j} in
              if t <> ignored_node && st.dist.{t} > d' then
  (
  if !debug then Format.eprintf "> %d %d [%d]@." t d' st.id.{j};
                Heap.insert heap t d'
)
            end
          end
        done
      end;
      loop ()
    end
  in
  loop ();
  (!steps, !steps')
(*
Format.eprintf "%d %d (%d) %d - %d -- %d@." !steps !steps' target_count (way_id id) max_dist i;
*)
(*
;(*if !trace = 2 then*) Format.eprintf "%d %d %d %d@." !steps !steps' target_count max_dist
*)
(*;print_int !steps; print_char '\n'*)

let delete_edges st i j =
  let n = ref (last_edge st i) in
  let k = ref (first_edge st i) in
  while !k <= !n do
    if st.target.{!k} = j then begin
      move_edge st !n !k;
      decr n
    end else
      incr k
  done;
(*
Format.eprintf "deleted: %d <- %d : %d %d @." i j (!n + 1 - st.first_edge.{i}) st.edge_count.{i};
*)
  st.edge_count.{i} <- !n + 1 - st.first_edge.{i}


let iter_remaining st f =
  for i = 0 to st.remaining_count - 1 do
    f i st.remaining.{i}
  done

let redundant_edge st i j dir =
  let res = ref false in
  let target = st.target.{j} in
  for k = first_edge st i to last_edge st i do
    res :=
      !res ||
      (k <> j && st.target.{k} = target && st.flags.{k} land dir <> 0 &&
       (st.weight.{k} < st.weight.{j} ||
        (st.weight.{k} = st.weight.{j} && k < j)))
  done;
  !res

let prepare_node_contraction dij_state st i =
  let deleted_edges = st.edge_count.{i} in
  let original_deleted = ref 0 in
  let added_edges = ref 0 in
  let original_added_edges = ref 0 in
  let shortcuts = ref [] in
  for j = first_edge st i to last_edge st i do
(*      Format.eprintf "%d %d@." i j;*)
    original_deleted := !original_deleted + st.original_edges.{j};
    if backward_edge st.flags.{j} && not (redundant_edge st i j 1) then begin
      let max_dist = ref (-1) in
      for k = first_edge st i to last_edge st i do
(*
print_char '/';
print_char ' ';
print_int i;
print_char ' ';
print_char '/';
print_char ' ';
print_int (min st.target.{j} st.target.{k});
print_char ' ';
print_char '/';
print_char ' ';
print_int (max st.target.{j} st.target.{k});
print_char '\n';
*)
        if
          k <> j && forward_edge st.flags.{k} && not (redundant_edge st i k 2)
        then
          max_dist := max !max_dist (st.weight.{j} + st.weight.{k})
      done;
      if !max_dist >= 0 then begin
let (steps, steps') =
        dijkstra st dij_state (st.target.{j}) i !max_dist (st.id.{j}) i;
in
let success = ref false in
        for k = first_edge st i to last_edge st i do
          if
            k = j || not (forward_edge st.flags.{k}) || redundant_edge st i k 2
          then () else
          if st.dist.{st.target.{k}} <= st.weight.{j} + st.weight.{k} then begin
success := true
(*
            Format.eprintf " %d -> %d (%d) %d@." k st.dist.{st.target.{k}}
	      (st.id.{k}) i;
	    Format.eprintf "ignored node %d <--%d(%d)-- %d --%d(%d)--> %d@." st.target.{j} (way_id st.id.{j}) st.weight.{j} i (way_id st.id.{k}) st.weight.{k} st.target.{k};
	    Format.eprintf "path: %a@." (show_path st) st.target.{k}
*)
          end else begin
            added_edges := !added_edges + 2;
            original_added_edges :=
              !original_added_edges +
              2 * (st.original_edges.{j} + st.original_edges.{k});
            shortcuts :=
              (st.target.{j}, st.target.{k}, st.weight.{j} + st.weight.{k},
               2, st.original_edges.{j} + st.original_edges.{k}) :: !shortcuts
          end
(*;Format.printf "%d / %d / %b@." steps steps' !success*)
        done
      end
    end
  done;
  let p = float st.depth.{i} in
  let p =
    if deleted_edges > 0 then
      p +. 2. *. float !added_edges /. float deleted_edges
    else
      p
  in
  let p =
    if deleted_edges > 0 then
      p +. 4. *. float !original_added_edges /. float !original_deleted
    else
      p
  in
(*
Format.eprintf "%d: %f (%d)@." i p deleted_edges;
*)
  (p, !shortcuts)

let add_edge st i j weight flags original_edges shortcut_node id =
(*
Format.eprintf "add %d -> %d (%d)@." i j weight;
*)
  reserve st i 1;
  st.edge_count.{i} <- st.edge_count.{i} + 1;
  let k = last_edge st i in
  st.target.{k} <- j;
  st.weight.{k} <- weight;
  st.flags.{k} <- flags;
  st.original_edges.{k} <- original_edges;
  st.shortcut_node.{k} <- shortcut_node;
  st.id.{k} <- 0

let contract_node dij_state st i =
  let (_, shortcuts) = prepare_node_contraction dij_state st i in
  for j = first_edge st i to last_edge st i do
    let k = st.target.{j} in
    delete_edges st k i
  done;
  (*XXX Could be done at a latter stage? *)
  for j = first_edge st i to last_edge st i do
    Column.append st.out_source i;
    Column.append st.out_target st.target.{j};
    Column.append st.out_weight st.weight.{j};
    Column.append st.out_flags st.flags.{j};
    Column.append st.out_shortcut
      (if st.original_edges.{j} > 1 then st.shortcut_node.{j} else -1)
  done;
  let shortcuts1 = Array.of_list shortcuts in
(*
  Array.sort
    (fun (j, k, _, _, _) (j', k', _, _, _) ->
       let c = compare j j' in if c <> 0 then c else compare k k')
    shortcuts1;
  for l = 1 to Array.length shortcuts1 - 1 do
    let (j, k, w, _, _) = shortcuts1.(l -1) in
    let (j', k', w', _, _) = shortcuts1.(l) in
    assert (j <> j' || k <> k')
  done;
*)
  let shortcuts2 =
    Array.map (fun (j, k, w, _, orig_edges) -> (k, j, w, 1, orig_edges))
      shortcuts1
  in
  let shortcuts = Array.append shortcuts1 shortcuts2 in
  Array.sort
    (fun (j, k, _, _, _) (j', k', _, _, _) ->
       let c = compare j j' in if c <> 0 then c else compare k k')
    shortcuts;
  let len = Array.length shortcuts in
(*
Format.eprintf "%d@." len;
*)
  let l = ref 0 in
  while !l < len do
    let (j, k, w, f, orig_edges) = shortcuts.(!l) in
    if
      !l + 1 < len &&
      let (j', k', w', _, _) = shortcuts.(!l + 1) in
(*
Format.eprintf "%d %d/%d %d/%d %d/%d@." !l j' j k' k w' w;
*)
      j' = j && k' = k && w' = w
    then begin
      add_edge st j k w 3 orig_edges i 0;
      incr l
    end else
      add_edge st j k w f orig_edges i 0;
    incr l
  done

let update_neighbours dij_state st i =
  let a = Array.make st.edge_count.{i} max_int in
  let j0 = first_edge st i in
  let depth = 1 + st.depth.{i} in
  for j = j0 to last_edge st i do
    let k = st.target.{j} in
    st.depth.{k} <- max st.depth.{k} depth;
    a.(j - j0) <- k
  done;
  Array.sort compare a;
  for j = 0 to Array.length a - 1 do
    let k = a.(j) in
    if j = 0 || k <> a.(j - 1) then begin
      st.priority.{k} <- fst (prepare_node_contraction dij_state st k)
(*;if !trace = 2 then Format.eprintf "%d / %f@." k st.priority.{k}*)
    end
  done

let max_depth = ref 0

let update_neighbour_depths st i =
  let depth = 1 + st.depth.{i} in
  for j = first_edge st i to last_edge st i do
    if depth > !max_depth then begin max_depth := depth; Format.eprintf "depth: %d@." depth end;
    let k = st.target.{j} in
    st.depth.{k} <- max st.depth.{k} depth
  done

let update_neighbour_priorities updated dij_state st i =
  for j = first_edge st i to last_edge st i do
    let k = st.target.{j} in
    if not (Bitvect.test updated k) then begin
      Bitvect.set updated k;
      st.priority.{k} <- fst (prepare_node_contraction dij_state st k)
(*
;if !trace = 2 then Format.eprintf "%d / %f@." k st.priority.{k}
*)
    end
  done

let check_edges st =
    let rem = Bitvect.make (Array1.dim st.first_edge) in
    iter_remaining st (fun _ i -> Bitvect.set rem i);
    iter_remaining st
      (fun _ i ->
         for j = first_edge st i to last_edge st i do
           let t = st.target.{j} in
           if not (Bitvect.test rem t) then
             Format.eprintf "DANGLING EDGE %d -> %d@." i t
           else begin
           let rev = ref false in
           for k = first_edge st t to last_edge st t do
             if st.target.{k} = i then rev := true
           done;
           if not !rev then
             Format.eprintf "MISSING REVERSE EDGE %d -> %d@." i t
           end
         done)
