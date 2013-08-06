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
The idea is to (virtually) add edges between nodes with an odd degree,
and then to repeatedly use Hierholzer's algorithm to find an Eularian
circuit for each connected component of the graph. By removing the
virtual edges, we get a minimal number of paths.
*)

(*
One connected component, all nodes are even -> 1 circuit of |E| + 1 nodes
One connected component, k > 0 nodes are odd -> k paths of |E| + k nodes

Count the number of connected components, of even nodes in each
component


Incremental computation?
------------------------
- from 2 odd to none: close the path
- from 2(k+1) to 2k odd:
    concatenate the paths;
    if we get a loop, we can break it at some other point
    and concatenate it with the other path

==> if we have a path ending at this node, add the edge to the path;
    if we then get a loop, try to concatenate it with another path
==> if there is a loop through this node, break it to add the edge

Invariant: we always associate exactly one path to each node
*)

let debug = false

type t = (bool ref * int) list

let initialize g =
  let g' = Array.make (Array.length g) [] in
  Array.iteri
    (fun i l ->
       List.iter
         (fun j ->
            let m = ref false in
            g'.(i) <- (m, j) :: g'.(i);
            g'.(j) <- (m, i) :: g'.(j))
         l)
    g;
  g'

let odd_degree g = (ref 0, Array.map (fun l -> (List.length l) land 1 = 1) g)

let rec next_node g i =
  match g.(i) with
    [] ->
      -1
  | (m, j) :: r ->
      g.(i) <- r;
      if !m then
        next_node g i
      else begin
        m := true;
        j
      end

let rec next_odd (i, o) =
  if !i = Array.length o then
    -1
  else if o.(!i) then begin
    o.(!i) <- false;
    !i
  end else begin
    incr i;
    next_odd (i, o)
  end

let rec find graph odd_deg i j cont lst =
  let k = next_node graph j in
if debug then Format.eprintf "Going from %d to %d@." j k;
  if k = -1 then begin
    if i = j then begin
if debug then Format.eprintf "Loop through %d@." i;
      (i :: cont, lst)
    end else begin
if debug then Format.eprintf "Path %d --> %d@." i j;
      (snd odd_deg).(j) <- false;
      let k = next_odd odd_deg in
if debug then Format.eprintf "Continuing from %d@." k;
      if k = -1 then
        ([j], lst)
      else begin
        let (path, lst) = find graph odd_deg i k cont lst in
        ([j], path :: lst)
      end
    end
  end else begin
    let (path, lst) = find graph odd_deg i k cont lst in
if debug then Format.eprintf "--@.";
    find graph odd_deg j j (path) lst
  end

let rec find_circuits graph odd_deg i lst =
  if i = Array.length graph then
    lst
  else begin
    let j = next_node graph i in
if debug then Format.eprintf "Circuit through %d-%d@." i j;
    if j = -1 then
      find_circuits graph odd_deg (i + 1) lst
    else
      let (path, lst) = find graph odd_deg i j [] lst in
      find_circuits graph odd_deg (i + 1) ((i :: path) :: lst)
  end

let rec find_unclosed_paths graph odd_deg lst =
  let i = next_odd odd_deg in
  if i <> -1 then begin
    let (path, lst) = find graph odd_deg i i [] lst in
    find_unclosed_paths graph odd_deg (path :: lst)
  end else
    lst

let find_paths graph =
  let graph = initialize graph in
  let odd_deg = odd_degree graph in
if debug then Format.eprintf "Finding unclosed paths@.";
  let lst = find_unclosed_paths graph odd_deg [] in
if debug then Format.eprintf "Finding circuits@.";
  find_circuits graph odd_deg 0 lst

(*
(* 6 edges, no odd => 1 path with 7 nodes *)
let g = [|[1; 2; 3; 4]; [2]; []; [4]; []|]
(* 7 edges, 4 odd nodes => 2 paths with 9 nodes *)
let g = [|[1; 2; 3]; [0; 2]; []; [0; 2]|]
(* 4 edges, 4 odd nodes => 2 paths with 6 nodes *)
let g = [|[1; 2; 3; 4]; [];[];[];[]|]
(* 4 edges, no odd => 1 path with 5 nodes *)
let g = [|[1]; [2;3];[0];[]|]
(* 5 edges, 4 odd nodes => 2 paths with 7 nodes *)
let g = [|[1]; [2;3];[0;4];[];[]|]
(* 6 edges, 6 odd nodes => 3 paths with 9 nodes *)
let g = [|[1;5]; [2;3];[0;4];[];[];[]|]

let _ =
List.iter
  (fun p ->
    List.iter (fun n -> Format.eprintf "%d " n) p;
    Format.eprintf "@.")
(find_paths g)
*)

(****)

type 'a u =
  Empty | Singleton of 'a | Append of 'a u * 'a u | Reverse of 'a u

let rec flatten p rem =
  match p with
    Empty -> rem
  | Singleton x -> x :: rem
  | Append (p1, p2) -> flatten p1 (flatten p2 rem)
  | Reverse Empty -> rem
  | Reverse (Singleton x) -> x :: rem
  | Reverse (Append (p1, p2)) -> flatten (Reverse p2) (flatten (Reverse p1) rem)
  | Reverse (Reverse p) -> flatten p rem

let append p q =
  match p, q with Empty, _ -> q | _, Empty -> p | _ -> Append (p, q)

let reverse p =
  match p with Reverse p -> p  | Empty | Singleton _ -> p | _ -> Reverse p

let flatten p = flatten p []


(* 4 word / elt *)
type 'a path = { elt : 'a; mutable n1 : 'a path; mutable n2 : 'a path }

let merge p1 p2 =
  let add p1 p2 =
    if p1.n1 == p1 then
      p1.n1 <- p2
    else if p1.n2 == p1 then
      p1.n2 <- p2
    else
      assert false
  in
  add p1 p2; add p2 p1


(****)

let sint_of_int i = let i' = i lsr 1 in if i land 1 = 1 then (-i' - 1) else i'

let rec read_varint_rec a p v offs =
  let i = !p in
  let c = Char.code a.[i] in
  incr p;
  if c >= 0x80 then
    read_varint_rec a p (v lor ((c land 0x7f) lsl offs)) (offs + 7)
  else
    v lor (c lsl offs)

let read_varint a p = read_varint_rec a p 0 0

let read_signed_varint a p = sint_of_int (read_varint a p)

let read_int_2 s pos = Char.code s.[pos] lor (Char.code s.[pos + 1] lsl 8)

let int_of_sint i = if i >= 0 then 2 * i else - 2 * i - 1

let rec write_varint a p v =
  if v < 128 then begin
    a.[p] <- Char.chr (v land 127);
    p + 1
  end else begin
    a.[p] <- Char.chr ((v land 127) + 128);
    write_varint a (p + 1) (v lsr 7)
  end

let write_signed_varint a p v = write_varint a p (int_of_sint v)

let output_int_2 ch v =
  output_byte ch (v land 0xff);
  output_byte ch (v lsr 8)

(****)
let _ = Column.set_database "/tmp/osm"
let (leaves, tree) = Rtree.open_in (Column.file_in_database "linear/rtree")

let leaves = open_in leaves

let linear_ratio = 50

let decode_leaf i =
  let leaf_size = 2048 in
  let buf = String.create leaf_size in
  seek_in leaves (i * leaf_size);
  really_input leaves buf 0 leaf_size;

  let node_len = read_int_2 buf 0 in
  let node_lat = Array.make (node_len / 2) 0 in
  let node_lon = Array.make (node_len / 2) 0 in
  let i = ref 0 in
  let pos = ref 4 in
  let lat = ref 0 in
  let lon = ref 0 in
  while !pos < node_len + 4 do
    let v = !lat + read_signed_varint buf pos in
    node_lat.(!i) <- v * linear_ratio;
    lat := v;
    let v = !lon + read_signed_varint buf pos in
    node_lon.(!i) <- v * linear_ratio;
    lon := v;
    incr i
  done;
  let node_lat = Array.sub node_lat 0 !i in
  let node_lon = Array.sub node_lon 0 !i in
  let nodes = (node_lat, node_lon) in

  let edge_len = read_int_2 buf 2 in
  let edges = Array.make edge_len (0, 0, 0, 0, nodes) in
  let i = ref 0 in
  let node = ref 0 in
  while !pos < edge_len + node_len + 4 do
    let n1 = !node + read_signed_varint buf pos in
    node := n1;
    let n2 = !node + read_signed_varint buf pos in
    node := n2;
    let cat = Char.code buf.[!pos] in
    let layer = Char.code buf.[!pos + 1] - 128 in
    pos := !pos + 2;
    edges.(!i) <- (n1, n2, cat, layer, nodes);
    incr i
  done;
  let edges = Array.sub edges 0 !i in
Format.eprintf "%d/%.1d %f %d/%d %.1f@." node_len (Array.length node_lat) (float node_len /. float (Array.length node_lat)) edge_len (Array.length edges) (float edge_len /. float (Array.length edges));
  edges


let v = false

let num = ref 0
let miss = ref 0
let find_node tbl i =
  incr num;
  if not tbl.(i) then begin incr miss; tbl.(i) <- true end

let chunk tbl edges i l =
  let (_, _, _, _, (node_lat, node_lon)) = edges.(i) in
  let g = Array.make (Array.length node_lat) [] in
  for j = i to i + l - 1 do
    let (n1, n2, _, _, _) = edges.(j) in
    g.(n1) <- n2 :: g.(n1)
  done;
  let l = find_paths g in
List.iter (fun p -> List.iter (fun i -> find_node tbl i) p) l;
if v then
List.iter
  (fun p ->
    Format.eprintf "+ ";
    List.iter (fun n -> Format.eprintf "%d " n) p;
    Format.eprintf "@.")
    l;
  l

(*
Header: 2 bytes + size of reference point
Categories: 3 bytes
Paths: size of length + size of first point + size of deltas
*)

let write_paths node_lat node_lon paths =
  let buf = String.create 2048 in
  let pos = ref 0 in
  (* Reference point *)
  let lat = node_lat.(0) in
  let lon = node_lon.(0) in
  pos := write_varint buf !pos lat;
  pos := write_varint buf !pos lon;
  pos := !pos + 2; (* Number of categories *)
Format.eprintf "%d CAT@." (List.length paths);
  List.iter
    (fun (cat, lay, l) ->
       pos := !pos + 2; (* cat + lay *)
       List.iter
         (fun p ->
Format.eprintf "%d PATH@." (List.length p);
            pos := write_varint buf !pos (List.length p);
            match p with
              [] ->
                assert false
            | i :: r ->
                let lat' = node_lat.(i) in
                let lon' = node_lon.(i) in
                pos := write_varint buf !pos (lat' - lat);
                pos := write_varint buf !pos (lon' - lon);
                ignore
                  (List.fold_left
                     (fun (lat, lon) i ->
                        let lat' = node_lat.(i) in
                        let lon' = node_lon.(i) in
                        pos := write_varint buf !pos (lat' - lat);
                        pos := write_varint buf !pos (lon' - lon);
                        (lat', lon'))
                     (lat', lon') r))
        l;
       (* no more path *)
       pos := write_varint buf !pos 0
      )
    paths;
Format.eprintf "ZZZ %d@." !pos

let build_paths edges =
  Array.sort
    (fun (_, _, cat, lay, _) (_, _, cat', lay', _) ->
       let c = compare cat cat' in
       if c <> 0 then c else
       compare lay lay')
    edges;
  let len = Array.length edges in
  let i0 = ref 0 in
  let prev_cat = ref (-1) in
  let prev_lay = ref (-128) in
  let (_, _, _, _, (node_lat, node_lon)) = edges.(0) in
  let tbl = Array.make (Array.length node_lat) false in
  let paths = ref [] in
  for i = 0 to len - 1 do
    let (_, _, cat, lay, _) = edges.(i) in
    if (cat <> !prev_cat || lay <> !prev_lay) && i > !i0 then begin
      paths :=
        (!prev_cat, !prev_lay, chunk tbl edges !i0 (i - !i0)) :: !paths;
if v then Format.eprintf "----@.";
      i0 := i
    end;
    prev_cat := cat;
    prev_lay := lay
  done;
  paths :=
    (!prev_cat, !prev_lay, chunk tbl edges !i0 (len - !i0)) :: !paths;
  for i = 0 to Array.length tbl - 1 do
    assert (tbl.(i))
  done;
  List.map
    (fun (cat, lay, l) ->
       (cat, lay,
        List.map
          (fun p ->
             Array.of_list (List.map (fun i -> node_lat.(i)) p),
             Array.of_list (List.map (fun i -> node_lon.(i)) p))))
    !paths

let _ =
Rtree.find tree
  {Rtree.Bbox.min_lat = min_int; max_lat = max_int;
   min_lon = min_int; max_lon = max_int}
  (fun i ->
if v then Format.eprintf "==== %d@." i;
     let edges = decode_leaf i in
     Array.sort
       (fun (_, _, cat, lay, _) (_, _, cat', lay', _) ->
          let c = compare cat cat' in
          if c <> 0 then c else
          compare lay lay')
       edges;
     let len = Array.length edges in
     let i0 = ref 0 in
     let prev_cat = ref (-1) in
     let prev_lay = ref (-128) in
     let (_, _, _, _, (node_lat, node_lon)) = edges.(0) in
     let tbl = Array.make (Array.length node_lat) false in
     let paths = ref [] in
     for i = 0 to len - 1 do
       let (_, _, cat, lay, _) = edges.(i) in
       if (cat <> !prev_cat || lay <> !prev_lay) && i > !i0 then begin
         paths :=
           (!prev_cat, !prev_lay, chunk tbl edges !i0 (i - !i0)) :: !paths;
if v then Format.eprintf "----@.";
         i0 := i
       end;
       prev_cat := cat;
       prev_lay := lay
     done;
     paths :=
       (!prev_cat, !prev_lay, chunk tbl edges !i0 (len - !i0)) :: !paths;
     for i = 0 to Array.length tbl - 1 do
       assert (tbl.(i))
     done;
     write_paths node_lat node_lon !paths)

let _ =
Format.eprintf "miss: %d/%d@." !miss !num


(*
Choose a reference point
First node of each path relative to this point

Write way info, then number of path, then path (1 byte len, then nodes)

Keep the cost of each path in both orders
  (min of the cost in either order)
  ==> approximated using absolute value of delta between path values
      and cost of extremities

==> from this one can compute incrementally the total size
*)
