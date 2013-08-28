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
TODO
====
* More compact representation
*)


(* Linear features *)

let leaf_size = 2048

(****)

let _ = Printexc.record_backtrace true

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

let table t l =
  let find v = try Dictionary.find t v with Not_found -> -1 in
  let h = Hashtbl.create 17 in
  List.iter (fun (s, cat) -> Hashtbl.add h (find s) cat) l;
  h

module IntMap =
  Map.Make (struct type t = int let compare (x : int) y = compare x y end)

module Bbox = Rtree.Bbox

type state =
  { mutable nodes : int IntMap.t;
    mutable node_prev_pos : int;
    mutable node_pos : int;
    mutable node_lat : int;
    mutable node_lon : int;
(*
    mutable node_idx : int;
*)
    mutable node_last : int;
    mutable node_count : int;
    mutable edge_pos : int;
    mutable edge_prev_pos : int;
    mutable edge_in : int;
    mutable edge_out : int;
    mutable edge_count : int;
    mutable last_node : int;
    mutable prev_bbox : Bbox.t;
    mutable bbox : Bbox.t }

module Feature = Category.Make (struct
  type t =
    [ `Motorway | `Trunk | `Primary | `Secondary | `Tertiary
    | `Motorway_link | `Trunk_link | `Primary_link
    | `Secondary_link | `Tertiary_link
    | `Residential | `Unclassified | `Living_street | `Road | `Service
    | `Pedestrian | `Track | `Cycleway | `Bridleway | `Footway | `Path | `Steps
    | `River | `Canal | `Stream
    | `Runway | `Taxiway
    | `Rail | `Tram | `Subway ]
  let list =
    [ `Motorway; `Trunk; `Primary; `Secondary; `Tertiary;
      `Motorway_link; `Trunk_link; `Primary_link;
      `Secondary_link; `Tertiary_link;
      `Residential; `Unclassified; `Living_street; `Road; `Service;
      `Pedestrian; `Track; `Cycleway; `Bridleway; `Footway; `Path; `Steps;
      `River; `Canal; `Stream;
      `Runway; `Taxiway;
      `Rail; `Tram; `Subway ]
end)

let features : Feature.classifier =
  [("highway",
    [(`Any ["motorway"], `Motorway);
     (`Any ["trunk"], `Trunk);
     (`Any ["primary"], `Primary);
     (`Any ["secondary"], `Secondary);
     (`Any ["tertiary"], `Tertiary);
     (`Any ["motorway_link"], `Motorway_link);
     (`Any ["trunk_link"], `Trunk_link);
     (`Any ["primary_link"], `Primary_link);
     (`Any ["secondary_link"], `Secondary_link);
     (`Any ["tertiary_link"], `Tertiary_link);
     (`Any ["residential"], `Residential);
     (`Any ["unclassified"], `Unclassified);
     (`Any ["living_street"], `Living_street);
     (`Any ["road"], `Road);
     (`Any ["service"], `Service);
     (`Any ["pedestrian"], `Pedestrian);
     (`Any ["track"], `Track);
     (`Any ["cycleway"], `Cycleway);
     (`Any ["bridleway"], `Bridleway);
     (`Any ["footway"], `Footway);
     (`Any ["path"], `Path);
     (`Any ["steps"], `Steps)]);
   ("waterway",
    [(`Any ["river"], `River);
     (`Any ["canal"], `Canal);
     (`Any ["stream"; "drain"; "ditch"], `Stream)]);
   ("aeroway",
    [(`Any ["runway"], `Runway);
     (`Any ["taxiway"], `Taxiway)]);
   ("railway",
    [(`Any ["rail"], `Rail); (`Any ["tram"], `Tram);
     (`Any ["subway"], `Subway)])]

let _ =
  let dict = Dictionary.load "strings" in
  let s v = try Dictionary.find dict v with Not_found -> -1 in
  let idx = Column.open_in (Column.named "base" "way_assoc/idx") in
  let key = Column.open_in (Column.named "base" "way_assoc/key") in
  let value = Column.open_in (Column.named "base" "way_assoc/val") in

Format.eprintf "Filtering@.";
  let index =
    Column_ops.unique
      (Projection.filter_pred_2 key value (Feature.filter dict features))
  in
Format.eprintf "Projection (way index)@.";
  let index = Column_ops.unique (Projection.project index idx) in

Format.eprintf "Join (way refs)@.";
  ignore
    (Join.perform
       ~o1:(Column.named "linear" "way_refs/way")
       ~o2:(Column.named "linear" "way_refs/node")
       (Column.identity (Column.length index)) index
       (Column.open_in (Column.named "base" "way_refs/node"))
       (Column.open_in (Column.named "base" "way_refs/way")));

Format.eprintf "Join (way assoc)@.";
  let (assoc_idx, indices) =
    Join.perform
      ~o1:(Column.named "linear" "way_assoc/idx")
      (Column.identity (Column.length index)) index
      (Column.identity (Column.length idx))
      (Column.open_in (Column.named "base" "way_assoc/idx"))
  in

Format.eprintf "Project (way assoc)@.";
  let assoc_key =
    Projection.project ~o:(Column.named "linear" "way_assoc/key") indices
      (Column.open_in (Column.named "base" "way_assoc/key"))
  in
  let assoc_val =
    Projection.project ~o:(Column.named "linear" "way_assoc/val") indices
      (Column.open_in (Column.named "base" "way_assoc/val"))
  in

Format.eprintf "Categories@.";
  let assoc_categories =
    Column_ops.map_2 (Feature.classify dict features) assoc_key assoc_val in
  let (_, category) =
    Column_ops.group
      ~o2:(Column.named "linear" "way/category")
      min assoc_idx assoc_categories
  in
  Format.eprintf "Layers, bridges and tunnels@.";
  let _layer = s"layer" in
  let _bridge = s"bridge" in
  let _tunnel = s"tunnel" in
  let _no = s"no" in
  let layers = Hashtbl.create 17 in
  for i = -5 to 5 do
    Hashtbl.add layers (s (string_of_int i)) i
  done;
  let idx =
    Projection.filter_pred assoc_key
      (fun k -> k = _layer || k = _bridge || k = _tunnel) in
  let key = Projection.project idx assoc_key in
  let value = Projection.project idx assoc_val in
  let layer_info =
    Column_ops.map_2
      (fun k v ->
         if k = _layer then
           ((try Hashtbl.find layers v with Not_found -> 0) land 15) lsl 2
         else if k = _bridge && v <> _no then
           1
         else (* k = _tunnel *) if v <> _no then
           2
         else
           0)
      key value
  in
  ignore
    (let way = Projection.project idx assoc_idx in
     let (way, layer_info) = Column_ops.group (lor) way layer_info in
     (* XXX Could be optimized (this is a kind of projection *)
     Join.perform
       ~o2:(Column.named "linear" "way/layer")
       (Column.identity (Column.length category))
       (Column.identity (Column.length category))
       ~def2:0
       layer_info way);

Format.eprintf "Used nodes@.";
  let way_nodes = Column.open_in (Column.named "linear" "way_refs/node") in
  let (sorted_nodes, ways_of_sorted_nodes) =
    Sorting.perform
      ~o1:(Column.temp "sorted_nodes")
      way_nodes (Column.identity (Column.length way_nodes))
  in
  let nodes =
    Column_ops.unique ~o:(Column.named "linear" "node/idx") sorted_nodes
  in

Format.eprintf "Associated latitude and longitude.@.";
  let map_sorted input =
    let l = Column.length input in
    fun renaming output ->
    let o = Projection.project ~o:output input renaming in
    assert (Column.length o = l);
    o
  in
  let m = map_sorted (Column.open_in (Column.named "linear" "node/idx")) in
  let lat =
    m (Column.open_in (Column.named "base" "node/lat"))
      (Column.named "linear" "node/lat")
  in
  let lon =
    m (Column.open_in (Column.named "base" "node/lon"))
      (Column.named "linear" "node/lon")
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
    (m (Column.open_in (Column.named "linear" "node/lat"))
       (Column.named "linear" "way_refs/lat"));
  ignore
    (m (Column.open_in (Column.named "linear" "node/lon"))
       (Column.named "linear" "way_refs/lon"));
Format.eprintf "Order.@.";
  let order =
    compute_order (Column.named "linear" "node/order") lat lon in
  let l = Column.length nodes in
  let (_, reordered_nodes) = Sorting.perform order (Column.identity l) in

  let node_order = Sorting.permute reordered_nodes (Column.identity l) in
  ignore (m node_order (Column.named "linear" "way_refs/node_id"));
(*
  let (o1, o1') = Sorting.perform reordered_nodes (Column.identity l) in
  let way_nodes = Column.open_in (Column.named "linear" "way_refs/node") in
  let l = Column.length way_nodes in
  let (o2, o2') = Sorting.perform way_nodes (Column.identity l) in
  let (o, o') = Join.perform o1' o1 o2' o2 in
  ignore
    (Sorting.perform ~o2:(Column.named "linear" "way_refs/node_id") o' o);
*)
(*
  ignore (Sorting.perform
            ~o1:(Column.named "linear" "sorted/node/order")
            ~o2:(Column.named "linear" "sorted/node/lat")
            order (Column.open_in (Column.named "linear" "node/lat")));
  ignore (Sorting.perform ~o2:(Column.named "linear" "sorted/node/lon")
            order (Column.open_in (Column.named "linear" "node/lon")));
*)

Format.eprintf "Writing edges...@.";
  let stream path name =
    Column.stream (Column.open_in (Column.named path name)) in
  let way = stream "linear" "way_refs/way" in
  let node = stream "linear" "way_refs/node_id" in
  let latitude = stream "linear" "way_refs/lat" in
  let longitude = stream "linear" "way_refs/lon" in
  let way_category = stream "linear" "way/category" in
  let way_layer = stream "linear" "way/layer" in
  let way_idx =
    let layer = Column.open_in (Column.named "linear" "way/layer") in
    Column.stream (Column.identity (Column.length layer))
  in
  let node1 = Column.open_out (Column.named "linear" "edge/1") in
  let latitude1 = Column.open_out (Column.named "linear" "edge/lat1") in
  let longitude1 = Column.open_out (Column.named "linear" "edge/lon1") in
  let node2 = Column.open_out (Column.named "linear" "edge/2") in
  let latitude2 = Column.open_out (Column.named "linear" "edge/lat2") in
  let longitude2 = Column.open_out (Column.named "linear" "edge/lon2") in
  let category = Column.open_out (Column.named "linear" "edge/category") in
  let layer = Column.open_out (Column.named "linear" "edge/layer") in

  let insert_edge n1 lat1 lon1 n2 lat2 lon2 cat lay =
    Column.append node1 n1;
    Column.append latitude1 lat1;
    Column.append longitude1 lon1;
    Column.append node2 n2;
    Column.append latitude2 lat2;
    Column.append longitude2 lon2;
    Column.append category cat;
    Column.append layer lay
  in
  let rec find_cat w =
    let w' = Column.read way_idx in
    let cat = Column.read way_category in
    let lay = Column.read way_layer in
    if w <> w' then
      find_cat w (* Will happen with bogus ways with no node *)
    else
      (cat, lay)
  in
  let rec loop w n lat lon cat lay =
    let w' = Column.read way in
    let n' = Column.read node in
    let lat' = Column.read latitude in
    let lon' = Column.read longitude in
    if n' < max_int then
      if w = w' then begin
        if n < n' then
          insert_edge n lat lon n' lat' lon' cat lay
        else
          insert_edge n' lat' lon' n lat lon cat lay;
        loop w' n' lat' lon' cat lay
      end else begin
        let (cat, lay) = find_cat w' in
        loop w' n' lat' lon' cat lay
      end
  in
  let w = Column.read way in
  let (cat, lay) = find_cat w in
  loop w (Column.read node) (Column.read latitude) (Column.read longitude)
    cat lay;
  let node1 = Column.freeze node1 in
  let latitude1 = Column.freeze latitude1 in
  let longitude1 = Column.freeze longitude1 in
  let node2 = Column.freeze node2 in
  let latitude2 = Column.freeze latitude2 in
  let longitude2 = Column.freeze longitude2 in
  let category = Column.freeze category in
  let layer = Column.freeze layer in

Format.eprintf "Reordering edges...@.";
  let (node1, order) =
    Sorting.perform ~o1:(Column.named "linear" "sorted/edge/1")
      node1 (Column.identity (Column.length node1))
  in
  let rev_order =
    Sorting.permute order (Column.identity (Column.length order)) in
  let latitude1 =
    Sorting.permute ~o:(Column.named "linear" "sorted/edge/lat1")
      rev_order latitude1 in
  let longitude1 =
    Sorting.permute ~o:(Column.named "linear" "sorted/edge/lon1")
      rev_order longitude1 in
  let node2 =
    Sorting.permute ~o:(Column.named "linear" "sorted/edge/2")
      rev_order node2 in
  let latitude2 =
    Sorting.permute ~o:(Column.named "linear" "sorted/edge/lat2")
      rev_order latitude2 in
  let longitude2 =
    Sorting.permute ~o:(Column.named "linear" "sorted/edge/lon2")
      rev_order longitude2 in
  let category =
    Sorting.permute ~o:(Column.named "linear" "sorted/edge/category")
      rev_order category in
  let layer =
    Sorting.permute ~o:(Column.named "linear" "sorted/edge/layer")
      rev_order layer in

Format.eprintf "Build R-tree@.";
  let node1 = Column.stream node1 in
  let latitude1 = Column.stream latitude1 in
  let longitude1 = Column.stream longitude1 in
  let node2 = Column.stream node2 in
  let latitude2 = Column.stream latitude2 in
  let longitude2 = Column.stream longitude2 in
  let category = Column.stream category in
  let layer = Column.stream layer in

  let large_feature_1 cat =
    match Feature.of_id cat with
      `Footway | `Steps | `Service | `Tram | `Subway | `Taxiway | `Pedestrian
    | `Track | `Cycleway | `Bridleway | `Path  | `Residential | `Unclassified
    | `Living_street | `Road | `Stream ->
        false
    | _ ->
        true
  in
  let large_feature_2 cat =
    match Feature.of_id cat with
      `Footway | `Steps | `Service | `Tram | `Subway | `Taxiway | `Pedestrian
    | `Track | `Cycleway | `Bridleway | `Path  | `Residential | `Unclassified
    | `Living_street | `Road | `Tertiary_link | `Tertiary | `Canal | `Stream ->
        false
    | _ ->
        true
  in
  let large_feature_3 cat =
    match Feature.of_id cat with
      `Footway | `Steps | `Service | `Tram | `Subway | `Taxiway | `Pedestrian
    | `Track | `Cycleway | `Bridleway | `Path  | `Residential | `Unclassified
    | `Living_street | `Road | `Tertiary_link | `Tertiary | `Canal | `Stream
    | `Secondary_link | `Secondary ->
        false
    | _ ->
        true
  in


  let rtrees = ref [] in
  let open_rtree name =
    let node_buf = String.create (16 * 1024) in
    let edge_buf = String.create (16 * 1024) in

    let (leaves, tree) = Rtree.open_out name in
    let leaves = open_out leaves in

    let new_state () =
      { node_prev_pos = 0;
        node_pos = 0;
        nodes = IntMap.empty;
        node_lat = 0;
        node_lon = 0;
  (*
        node_idx = 0;
  *)
        node_last = 0;
        node_count = 0;
        edge_prev_pos = 0;
        edge_pos = 0;
        edge_in = 0;
        edge_out = 0;
        edge_count = 0;
        last_node = 0;
        prev_bbox = Bbox.empty;
        bbox = Bbox.empty }
    in
    let output_leaf st =
      output_int_2 leaves st.node_prev_pos;
      output_int_2 leaves st.edge_prev_pos;
(*
  Format.eprintf "%d %d %d %d@." st.node_prev_pos st.edge_prev_pos st.node_last st.edge_count;
*)
      output leaves node_buf 0 st.node_prev_pos;
      output leaves edge_buf 0 (leaf_size - st.node_prev_pos - 4);
  (*
  Format.eprintf "%a@." Bbox.print st.prev_bbox;
  *)
      Rtree.append tree st.prev_bbox
  (*
  {Bbox.min_lat = 0; Bbox.max_lat = 0x7fffffff;
  Bbox.min_lon = 0; Bbox.max_lon = 0x7fffffff }
  *)
    in
    let write_node st n lat lon =
      let lat = (lat + 24) / 50 in
      let lon = (lon + 24) / 50 in
      st.node_pos <- write_signed_varint node_buf st.node_pos (lat - st.node_lat);
      st.node_lat <- lat;
      st.node_pos <- write_signed_varint node_buf st.node_pos (lon - st.node_lon);
      st.node_lon <- lon;
  (*
      st.node_pos <- write_signed_varint node_buf st.node_pos (n - st.node_idx);
      st.node_idx <- n;
  *)
      st.node_count <- st.node_count + 1;
      st.bbox <- Bbox.add_point st.bbox lat lon
    in
  let num = ref 0 in
  let miss = ref 0 in
    let get_node st n lat lon =
  incr num;
      try
        IntMap.find n st.nodes
      with Not_found ->
  incr miss;
        let n' = st.node_last in
        st.node_last <- st.node_last + 1;
        st.nodes <- IntMap.add n n' st.nodes;
        write_node st n lat lon;
        n'
    in
    let rec write_edge st n1 lat1 lon1 n2 lat2 lon2 cat lay =
  (*
  Format.eprintf "%d <-> %d@." i0 o0;
  *)
      let i1 = get_node st n1 lat1 lon1 in
      let i2 = get_node st n2 lat2 lon2 in
      st.edge_pos <- write_signed_varint edge_buf st.edge_pos (i1 - st.last_node);
      st.last_node <- i1;
      st.edge_pos <- write_signed_varint edge_buf st.edge_pos (i2 - st.last_node);
      st.last_node <- i2;
      edge_buf.[st.edge_pos] <- Char.chr cat;
      edge_buf.[st.edge_pos + 1] <- Char.chr lay;
      st.edge_pos <- st.edge_pos + 2;
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
        write_edge (new_state ()) n1 lat1 lon1 n2 lat2 lon2 cat lay
      end else begin
        st.node_prev_pos <- st.node_pos;
        st.edge_prev_pos <- st.edge_pos;
        st.prev_bbox <- st.bbox;
        st.edge_count <- st.edge_count + 1;
        st
      end
    in
    let st = ref (new_state ()) in
    let close () =
      if !st.node_prev_pos > 0 then begin
        output_leaf !st;
        close_out leaves;
        Rtree.close_out tree
      end;
(*
Format.eprintf "miss: %d/%d@." !miss !num
*)
    in
    rtrees := close :: !rtrees;
    fun n1 lat1 lon1 n2 lat2 lon2 cat lay ->
      st := write_edge !st n1 lat1 lon1 n2 lat2 lon2 cat lay
  in

  let write_edge = open_rtree "linear/rtrees/all" in
  let write_large_edge_1 = open_rtree "linear/rtrees/large_1" in
  let write_large_edge_2 = open_rtree "linear/rtrees/large_2" in
  let write_large_edge_3 = open_rtree "linear/rtrees/large_3" in

  let write_edge n1 lat1 lon1 n2 lat2 lon2 cat lay =
    write_edge n1 lat1 lon1 n2 lat2 lon2 cat lay;
    if large_feature_1 cat then
      write_large_edge_1 n1 lat1 lon1 n2 lat2 lon2 cat 0;
    if large_feature_2 cat then
      write_large_edge_2 n1 lat1 lon1 n2 lat2 lon2 cat 0;
    if large_feature_3 cat then
      write_large_edge_3 n1 lat1 lon1 n2 lat2 lon2 cat 0;
  in

  let len =
    Column.length (Column.open_in (Column.named "linear" "sorted/edge/1")) in
  let t = Unix.gettimeofday () in
  let rec loop i =
    if i land 4095 = 4095 then begin
      let p = float i /. float len in
      let t' = Unix.gettimeofday () in
      Util.set_msg
        (Format.sprintf "writing edges: %s %.0f%% eta %.0fs"
           (Util.progress_bar p) (p *. 100.)
           ((1. -. p) *. (t' -. t) /. p))
    end;
    let n1 = Column.read node1 in
    let lat1 = Column.read latitude1 in
    let lon1 = Column.read longitude1 in
    let n2 = Column.read node2 in
    let lat2 = Column.read latitude2 in
    let lon2 = Column.read longitude2 in
    let cat = Column.read category in
    let lay = Column.read layer in
    if n1 <> max_int then begin
      write_edge n1 lat1 lon1 n2 lat2 lon2 cat lay;
      loop (i + 1)
    end
  in
  loop 0;
  Util.set_msg "";
  List.iter (fun close -> close ()) !rtrees

(*
- Edge table: src node, dst node, category, layer
- Mapping node -> routing id
- Write R-tree
*)
