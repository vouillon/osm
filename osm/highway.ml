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

let _ = Column.set_database "/tmp/osm"

(****)

(*XXX Duplicated code... *)
let compute_order out latitude longitude =
  let o = Column.open_out out in
  let latitude = Column.stream latitude in
  let longitude = Column.stream longitude in
  let rec loop () =
    let lat = Column.read latitude in
    let lon = Column.read longitude in
    if lat <> max_int then begin
      let lat = lat +  90_0000000 in
      let lon = lon + 180_0000000 in
      Column.append o (Geometry.hilbert_coordinate lat lon);
      loop ()
    end
  in
  loop ();
  Column.freeze o

(****)

let _ =
  let t = Dictionary.load "strings" in

  let hw = Dictionary.find t "highway" in

  let idx = Column.open_in (Column.named "base" "way_assoc/idx") in
  let key = Column.open_in (Column.named "base" "way_assoc/key") in
Format.eprintf "Filtering (key = highway)@.";
  let index = Projection.filter ~o:(Column.named "foo" "index") key hw in

Format.eprintf "Projection (way index)@.";
  let hw_idx =
    Projection.project ~o:(Column.named "highway" "way/idx") index idx
  in
Format.eprintf "Projection (category)@.";
  ignore
    (Projection.project ~o:(Column.named "highway" "way/category") index
       (Column.open_in (Column.named "base" "way_assoc/val")));
Format.eprintf "Projection (way id)@.";
  ignore(
    Projection.project ~o:(Column.named "highway" "way/id") hw_idx
      (Column.open_in (Column.named "base" "way/id")));
Format.eprintf "Join (way refs)@.";
  ignore
    (Join.perform
       ~o1:(Column.named "highway" "way_refs/way")
       ~o2:(Column.named "highway" "way_refs/node")
       (Column.identity (Column.length hw_idx)) hw_idx
       (Column.open_in (Column.named "base" "way_refs/node"))
       (Column.open_in (Column.named "base" "way_refs/way")));
Format.eprintf "Join (way assoc)@.";
  let (_, indices) =
    Join.perform
      ~o1:(Column.named "highway" "way_assoc/idx")
      (Column.identity (Column.length hw_idx)) hw_idx
      (Column.identity (Column.length idx))
      (Column.open_in (Column.named "base" "way_assoc/idx"))
  in
Format.eprintf "Project (way assoc)@.";
  ignore(
    Projection.project ~o:(Column.named "highway" "way_assoc/key") indices
      (Column.open_in (Column.named "base" "way_assoc/key")));
  ignore(
    Projection.project ~o:(Column.named "highway" "way_assoc/val") indices
      (Column.open_in (Column.named "base" "way_assoc/val")));

Format.eprintf "Used nodes@.";
  let way_nodes = Column.open_in (Column.named "highway" "way_refs/node") in
  let (sorted_nodes, ways_of_sorted_nodes) =
    Sorting.perform
      ~o1:(Column.temp "sorted_nodes")
      way_nodes (Column.identity (Column.length way_nodes))
  in
  let nodes =
    Column_ops.unique ~o:(Column.named "highway" "node/idx") sorted_nodes
  in
Format.eprintf "Associated latitude and longitude.@.";
  let map_sorted input =
    let l = Column.length input in
    fun renaming output ->
    let o = Projection.project ~o:output input renaming in
    assert (Column.length o = l);
    o
  in
  let m = map_sorted (Column.open_in (Column.named "highway" "node/idx")) in
  let lat =
    m (Column.open_in (Column.named "base" "node/lat"))
      (Column.named "highway" "node/lat")
  in
  let lon =
    m (Column.open_in (Column.named "base" "node/lon"))
      (Column.named "highway" "node/lon")
  in
  let (node_ids, ways) =
    Join.perform
      (Column.identity (Column.length nodes)) nodes
      ways_of_sorted_nodes sorted_nodes
  in
  assert (Column.length ways = Column.length ways_of_sorted_nodes);
  let m input output =
    let data = Projection.project node_ids input in
    assert (Column.length data = Column.length ways);
    Sorting.permute ~o:output ways data
  in
  ignore
    (m (Column.open_in (Column.named "highway" "node/lat"))
       (Column.named "highway" "way_refs/lat"));
  ignore
    (m (Column.open_in (Column.named "highway" "node/lon"))
       (Column.named "highway" "way_refs/lon"));
Format.eprintf "Order.@.";
  let order =
    compute_order (Column.named "highway" "node/order") lat lon in
  let l = Column.length nodes in
  let (_, reordered_nodes) = Sorting.perform order (Column.identity l) in
  let node_order = Sorting.permute reordered_nodes (Column.identity l) in
  ignore (m node_order (Column.named "highway" "way_refs/node_id"));

  ignore (Sorting.permute ~o:(Column.named "highway" "sorted_node/lat")
            node_order (Column.open_in (Column.named "highway" "node/lat")));
  ignore (Sorting.permute ~o:(Column.named "highway" "sorted_node/lon")
            node_order (Column.open_in (Column.named "highway" "node/lon")))

(****)

let stream path name = Column.stream (Column.open_in (Column.named path name))

module P = Routing_profile

let _ =

Format.eprintf "Writing edges...@.";
(*
  let identity = stream "highway" "way/id" in
*)
  let way = stream "highway" "way_refs/way" in
  let node = stream "highway" "way_refs/node_id" in
  let latitude = stream "highway" "way_refs/lat" in
  let longitude = stream "highway" "way_refs/lon" in

  let assoc_way = stream "highway" "way_assoc/idx" in
  let assoc_key = stream "highway" "way_assoc/key" in
  let assoc_val = stream "highway" "way_assoc/val" in

  let dict = Dictionary.load "strings" in
  let way_info = P.find dict "pedestrian" in

  let node_1 = Column.open_out (Column.named "highway" "edge/1") in
  let node_2 = Column.open_out (Column.named "highway" "edge/2") in
  let length = Column.open_out (Column.named "highway" "edge/len") in
  let weight = Column.open_out (Column.named "highway" "edge/weight") in
  let way_id = Column.open_out (Column.named "highway" "edge/idx") in
  let flags = Column.open_out (Column.named "highway" "edge/flags") in

  (*XXX Not sure this is correct when we have ways with no node...*)
  let rec read_assoc w l =
    let k = Column.read assoc_key in
    let v = Column.read assoc_val in
    let w' = Column.read assoc_way in
    let l = (k, v) :: l in
    if w' = w then read_assoc w l else l
  in
  let compute_info w =
    let assoc = read_assoc w [] in
(*
List.iter (fun (k,v) -> Format.eprintf "%s(%d)=%s(%d)@." (Dictionary.get dict k) k (Dictionary.get dict v) v) assoc;
*)
    way_info assoc
  in
  let insert_edge w n n' dist speed dir =
    let wei = truncate (float dist /. speed *. 3.6) in
    Column.append node_1 n;
    Column.append node_2 n';
    Column.append length dist;
    Column.append weight wei;
    Column.append way_id w;
    Column.append flags
      (match dir with
        `BIDIRECTIONAL -> 3
      | `FORWARD       -> 2
      | `BACKWARD      -> 1)
  in
  let rec loop w n lat lon info =
    let w' = Column.read way in
    let n' = Column.read node in
    let lat' = Column.read latitude in
    let lon' = Column.read longitude in
    if n' < max_int then begin
      if w = w' && info.P.speed > 0. then begin
        if info.P.backward_speed <= 0. then
          info.P.backward_speed <- info.P.speed;
	let dist = Geometry.distance lat lon lat' lon' in
(*
	Format.eprintf "%d %d %d %d %d@." lon lat lon' lat' dist;
*)
        if
          info.P.direction = `BIDIRECTIONAL &&
          info.P.speed = info.P.backward_speed
        then begin
          insert_edge w n n' dist info.P.speed `BIDIRECTIONAL;
          insert_edge w n' n dist info.P.speed `BIDIRECTIONAL
        end else begin
          if
            info.P.direction = `BIDIRECTIONAL || info.P.direction = `ONEWAY
          then begin
            insert_edge w n n' dist info.P.speed `FORWARD;
            insert_edge w n' n dist info.P.speed `BACKWARD
          end;
          if
            info.P.direction = `BIDIRECTIONAL || info.P.direction = `OPPOSITE
          then begin
            insert_edge w n n' dist info.P.backward_speed `BACKWARD;
            insert_edge w n' n dist info.P.backward_speed `FORWARD
          end
        end
      end;
      let info = if w = w' then info else compute_info w' in
      loop w' n' lat' lon' info
    end
  in
  ignore (Column.read assoc_way);
  let w = Column.read way in
  loop w (Column.read node) (Column.read latitude) (Column.read longitude)
       (compute_info w);
  let node_1 = Column.freeze node_1 in
  let node_2 = Column.freeze node_2 in
  let length = Column.freeze length in
  let weight = Column.freeze weight in
  let way_id = Column.freeze way_id in
  let flags = Column.freeze flags in

Format.eprintf "Reordering edges...@.";
  let (node_1, order) =
    Sorting.perform ~o1:(Column.named "highway" "edge/1bis")
      node_1 (Column.identity (Column.length node_1))
  in
  let rev_order =
    Sorting.permute order (Column.identity (Column.length order)) in
  let _ =
    Sorting.permute ~o:(Column.named "highway" "edge/2bis") rev_order node_2 in
  let _ =
    Sorting.permute ~o:(Column.named "highway" "edge/length_bis")
      rev_order length in
  let _ =
    Sorting.permute ~o:(Column.named "highway" "edge/weight_bis")
      rev_order weight in
  let _ =
    Sorting.permute ~o:(Column.named "highway" "edge/idx_bis")
      rev_order way_id in
  let _ =
    Sorting.permute ~o:(Column.named "highway" "edge/flags_bis")
      rev_order flags in
()


(*

  let l = Column.length (Column.open_in (Column.named "highway" "way/id")) in
  for i = 0 to l - 1 do
    let id = Column.read identity in
Format.eprintf "%d@." i;
    assert (Column.read way = i);
    let n = ref (Column.read node) in
    let lat = ref (Column.read latitude) in
    let lon = ref (Column.read longitude) in

()
  done
*)

(****************************************************************)

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

module IntMap =
  Map.Make (struct type t = int let compare (x : int) y = compare x y end)

type bbox =
  { min_lat : int;
    max_lat : int;
    min_lon : int;
    max_lon : int }

type state =
  { mutable nodes : int IntMap.t;
    mutable node_prev_pos : int;
    mutable node_pos : int;
    mutable node_lat : int;
    mutable node_lon : int;
    mutable node_idx : int;
    mutable node_last : int;
    mutable node_count : int;
    mutable edge_pos : int;
    mutable edge_prev_pos : int;
    mutable edge_in : int;
    mutable edge_out : int;
    mutable edge_count : int;
    mutable last_node : int;
    mutable prev_bbox : bbox;
    mutable bbox : bbox }

type level =
  { mutable level_bbox : bbox;
    buffer : string;
    file : (int32, Bigarray.int32_elt) Mapped_file.output_stream;
    mutable idx : int }

let new_bbox () =
  { min_lat = max_int;
    max_lat = min_int;
    min_lon = max_int;
    max_lon = min_int }

let bbox_union b b' =
  { min_lat = min b.min_lat b'.min_lat;
    max_lat = max b.max_lat b'.max_lat;
    min_lon = min b.min_lon b'.min_lon;
    max_lon = max b.max_lon b'.max_lon }

let bbox_overlaps b b' =
  b.min_lat <= b'.max_lat && b'.min_lat <= b.max_lat &&
  b.min_lon <= b'.max_lon && b'.min_lon <= b.max_lon

(*
let test =
  { min_lat = 48635 * 200; max_lat = 48645 * 200; min_lon = 2445 * 200; max_lon = 2455 * 200 }

let test =
  { min_lat = 48845 * 200; max_lat = 48855 * 200; min_lon = 2345 * 200; max_lon = 2355 * 200 }
(**)
let test =
  { min_lat = 48840 * 200; max_lat = 48860 * 200; min_lon = 2340 * 200; max_lon = 2360 * 200 }
let test =
  { min_lat = 48849 * 200; max_lat = 48851 * 200; min_lon = 2349 * 200; max_lon = 2351 * 200 }
*)

let node_size = 1024
let leaf_size = 1024

let _ =
  let in_node =
    Column.stream (Column.open_in (Column.named "highway" "edge/1bis")) in
  let out_node =
    Column.stream (Column.open_in (Column.named "highway" "edge/2bis")) in
  let latitude = Column.open_in (Column.named "highway" "sorted_node/lat") in
  let longitude = Column.open_in (Column.named "highway" "sorted_node/lon") in

  let node_buf = String.create (16 * 1024) in
  let edge_buf = String.create (16 * 1024) in

  Util.make_directories (Column.file_in_database "highway/r_tree/0");
  let leaves = open_out (Column.file_in_database "highway/r_tree/0") in

  let levels = ref IntMap.empty in
  let max_level = ref 0 in

  let rec add_bbox level bbox =
    let st =
      try
        IntMap.find level !levels
      with Not_found ->
        let file =
          Column.file_in_database (Format.sprintf "highway/r_tree/%d" level)
        in
        Util.make_directories file;
        let st =
          { level_bbox = new_bbox ();
            buffer = String.create (16 * 1024);
            file = Mapped_file.open_out file node_size Mapped_file.int32;
            idx = 0 }
        in
        levels := IntMap.add level st !levels;
        max_level := max !max_level level;
        st
    in
    Mapped_file.resize st.file (4 * (st.idx + 1));
    let a = Mapped_file.output_array st.file in
    a.{4 * st.idx + 0} <- Int32.of_int bbox.min_lat;
    a.{4 * st.idx + 1} <- Int32.of_int bbox.max_lat;
    a.{4 * st.idx + 2} <- Int32.of_int bbox.min_lon;
    a.{4 * st.idx + 3} <- Int32.of_int bbox.max_lon;
    st.idx <- st.idx + 1;
    st.level_bbox <- bbox_union st.level_bbox bbox;
    if st.idx mod (node_size lsr 4) = 0 then begin
      add_bbox (level + 1) st.level_bbox;
      st.level_bbox <- new_bbox ()
    end
  in

  let output_leaf st =
    output_int_2 leaves st.node_prev_pos;
    output_int_2 leaves st.edge_prev_pos;
    output leaves node_buf 0 st.node_prev_pos;
    output leaves edge_buf 0 (leaf_size - st.node_prev_pos - 4)
  in

  let new_state () =
    { node_prev_pos = 0;
      node_pos = 0;
      nodes = IntMap.empty;
      node_lat = 0;
      node_lon = 0;
      node_idx = 0;
      node_last = 0;
      node_count = 0;
      edge_prev_pos = 0;
      edge_pos = 0;
      edge_in = 0;
      edge_out = 0;
      edge_count = 0;
      last_node = 0;
      prev_bbox = new_bbox ();
      bbox = new_bbox () }
  in

  let write_node st n =
    let lat = (Column.get latitude n + 24) / 50 in
    let lon = (Column.get longitude n + 24) / 50 in
(*
Format.eprintf ">> %d: %d %d@." n (Column.get latitude n) (Column.get longitude n);
*)
    st.node_pos <- write_signed_varint node_buf st.node_pos (lat - st.node_lat);
    st.node_lat <- lat;
    st.node_pos <- write_signed_varint node_buf st.node_pos (lon - st.node_lon);
    st.node_lon <- lon;
    st.node_pos <- write_signed_varint node_buf st.node_pos (n - st.node_idx);
    st.node_idx <- n;
    st.node_count <- st.node_count + 1;
    st.bbox <-
      { min_lat = min st.bbox.min_lat lat;
        max_lat = max st.bbox.max_lat lat;
        min_lon = min st.bbox.min_lon lon;
        max_lon = max st.bbox.max_lon lon }
  in
  let get_node st n =
    try
      IntMap.find n st.nodes
    with Not_found ->
      let n' = st.node_last in
      st.node_last <- st.node_last + 1;
      st.nodes <- IntMap.add n n' st.nodes;
      write_node st n;
      n'
  in
  let rec write_edge st i0 o0 =
(*
Format.eprintf "%d <-> %d@." i0 o0;
*)
    let i = get_node st i0 in
    let o = get_node st o0 in
    st.edge_pos <- write_signed_varint edge_buf st.edge_pos (i - st.last_node);
    st.last_node <- i;
    st.edge_pos <- write_signed_varint edge_buf st.edge_pos (o - st.last_node);
    st.last_node <- o;
    if st.edge_pos + st.node_pos > leaf_size - 4 then begin
(*
Format.eprintf "%d %d@." st.edge_count st.node_count;
Format.eprintf "%d %f %f@." st.edge_count (float(st.bbox.max_lat - st.bbox.min_lat) /. 200000.) (float (st.bbox.max_lon - st.bbox.min_lon) /. 200000.);
*)
(*
let c = 1. /. 200000. in
Format.eprintf "%.3f %.3f %.3f %.3f %b@." (c *. float st.bbox.max_lat) (c *. float st.bbox.min_lat) (c *. float st.bbox.max_lon) (c *. float st.bbox.min_lon) (bbox_overlaps test st.bbox);
*)
      output_leaf st;
      add_bbox 1 st.prev_bbox;
      write_edge (new_state ()) i0 o0
    end else begin
      st.node_prev_pos <- st.node_pos;
      st.edge_prev_pos <- st.edge_pos;
      st.prev_bbox <- st.bbox;
      st.edge_count <- st.edge_count + 1;
      st
    end
  in

  let rec loop st =
    let i = Column.read in_node in
    let o = Column.read out_node in
    if i <> max_int then begin
      if i < o then
        loop (write_edge st i o)
      else
        loop st
    end else  if st.node_prev_pos > 0 then begin
      output_leaf st;
      add_bbox 1 st.prev_bbox;
      close_out leaves
    end
  in
  loop (new_state ());
  let level = ref 1 in
  while !level <= !max_level do
    let st = IntMap.find !level !levels in
    if st.idx mod (node_size lsr 4) > 0 && st.idx > node_size lsr 4 then
      add_bbox (!level + 1) st.level_bbox;
    Mapped_file.close_out st.file;
    incr level
  done
