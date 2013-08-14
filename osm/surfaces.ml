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
XXXX
- Different r_trees depending on size
- Try to merge touching surfaces?
- Simplify surfaces
    http://bost.ocks.org/mike/simplify/
    http://www2.dcs.hull.ac.uk/CISRG/publications/DPs/DP10/DP10.html
    http://bost.ocks.org/mike/simplify/simplify.js
   ==> not taking in account shared nodes will produce artifacts...
- Better compression
  ==> group by category
  ==> use one byte for the size when possible
  ==> use less than one byte per coordinate for small displacements
- Adaptative quantization ???
  ==> Nodes that are not shared can be moved somewhat when not closed to
      one-another
  ==> When nodes are far apart, we can do with less precision
      (if a node is far from any of its neighbours, we can move it somewhat)
*)

let _ = Printexc.record_backtrace true

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

let resize_array size a =
  let l = Array.length !a in
  assert (l > 0);
  if l < size then begin
    let l' = ref l in
    while !l' < size do l' := 2 * !l' done;
    let a' = Array.make !l' !a.(0) in
    Array.blit !a 0 a' 0 l;
    a := a'
  end

let resize_string size s =
  let l = String.length !s in
  assert (l > 0);
  if l < size then begin
    let l' = ref l in
    while !l' < size do l' := 2 * !l' done;
    let s' = String.make !l' !s.[0] in
    String.blit !s 0 s' 0 l;
    s := s'
  end

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

let _ = Column.set_database "/tmp/osm"

module Surface = Category.Make (struct
  type t =
    [ `Water | `Forest | `Grass | `Heath | `Rock | `Sand | `Glacier
    | `Farmland | `Residential | `Commercial
    | `Industrial | `Park | `Cemetery | `Parking | `Building
    | `Highway_residential | `Highway_unclassified | `Highway_living_street
    | `Highway_service | `Highway_pedestrian | `Highway_track
    | `Highway_footway | `Highway_path ]
  let list = 
    [ `Water; `Forest; `Grass; `Heath; `Rock; `Sand; `Glacier;
      `Farmland; `Residential; `Commercial;
      `Industrial; `Park; `Cemetery; `Parking; `Building;
      `Highway_residential; `Highway_unclassified; `Highway_living_street;
      `Highway_service; `Highway_pedestrian; `Highway_track;
      `Highway_footway; `Highway_path ]
end)

let surfaces : Surface.classifier =
  [("natural",
    [(`Any ["water"; "lake"; "bay"], `Water);
     (`Any ["wood"; "scrub"], `Forest);
     (`Any ["grassland"; "fell"], `Grass);
     (`Any ["heath"], `Heath);
     (`Any ["rock"; "bare_rock"; "scree"], `Rock);
     (`Any ["sand"; "beach"], `Sand);
     (`Any ["glacier"], `Glacier)]);
   ("waterway",
    [(`Any ["riverbank"; "dock"], `Water)]);
   ("landuse",
    [(`Any ["residential"], `Residential);
     (`Any ["forest"], `Forest);
     (`Any ["grass"; "meadow"], `Grass);
     (`Any ["village_green"], `Park);
     (`Any ["basin"; "reservoir"; "water"; "salt_pond"], `Water);
     (`Any ["farm"; "farmland"; "allotments";
            "greenhouse_horticulture"; "orchard"; "vineyard"],
      `Farmland);
     (`Any ["cemetery"], `Cemetery);
     (`Any ["query"], `Rock);
     (`Any ["commercial"], `Commercial);
     (`Any ["industrial"; "railway"], `Industrial)]);
   ("leisure",
    [(`Any ["garden"; "common"; "golf_course"], `Grass);
     (`Any ["park"; "recreation_ground"; "playground"], `Park)]);
   ("building", [(`Not ["no"], `Building)]);
   ("aeroway", [(`Any ["terminal"], `Building)]);
   ("amenity",
    [(`Any ["grave_yard"], `Cemetery);
     (`Any ["parking"], `Parking)]);
   ("tourism", [(`Any ["zoo"], `Park)])]

let highways : Surface.classifier =
  [("highway",
    [(`Any ["residential"], `Highway_residential);
     (`Any ["unclassified"], `Highway_unclassified);
     (`Any ["living_street"], `Highway_living_street);
     (`Any ["service"], `Highway_service);
     (`Any ["pedestrian"], `Highway_pedestrian);
     (`Any ["track"], `Highway_track);
     (`Any ["footway"], `Highway_footway);
     (`Any ["path"], `Highway_path)])]

let _ =
  let dict = Dictionary.load "strings" in
  let s v = try Dictionary.find dict v with Not_found -> -1 in
  let filter =
    Surface.filter dict (("area", [(`Any ["yes"], `Water)]) :: surfaces) in

  (***)

  let filter_tags table =
    let idx = Column.open_in (Column.named table "idx") in
    let key = Column.open_in (Column.named table "key") in
    let value = Column.open_in (Column.named table "val") in

    Format.eprintf "Filtering@.";
    let index = Projection.filter_pred_2 key value filter in

    Format.eprintf "Projection (way index)@.";
    Column_ops.unique (Projection.project index idx)
  in
  let compute_categories index src dst =
    Format.eprintf "Join (way assoc)@.";
    let (indices, assoc_idx) =
      let idx = Column.open_in (Column.named src "idx") in
      Join.perform
        ~o2:(Column.named dst "poly_assoc/idx")
        (Column.identity (Column.length idx)) idx
        (Column.identity (Column.length index)) index
    in
    Format.eprintf "Project (way assoc)@.";
    let assoc_key =
      Projection.project
        ~o:(Column.named dst "poly_assoc/key") indices
        (Column.open_in (Column.named src "key"))
    in
    let assoc_val =
      Projection.project ~o:(Column.named dst "poly_assoc/val")
        indices
        (Column.open_in (Column.named src "val"))
    in
    Format.eprintf "Categories@.";
    let assoc_categories =
      Column_ops.map_2 (Surface.classify dict (highways @ surfaces))
        assoc_key assoc_val in
    let (_, category) =
      Column_ops.group
        ~o2:(Column.named dst "poly/category")
        min assoc_idx assoc_categories
    in
    Format.eprintf "Layer@.";
    let _layer = s"layer" in
    let layers = Hashtbl.create 17 in
    for i = -5 to 5 do
      Hashtbl.add layers (s (string_of_int i)) i
    done;
    let idx = Projection.filter assoc_key _layer in
    let layer_value = Projection.project idx assoc_val in
    let layer =
      Column_ops.map
        (fun v -> try Hashtbl.find layers v with Not_found -> 0)
        layer_value
    in
    let poly = Projection.project idx assoc_idx in
    ignore
      (Join.perform
         ~o2:(Column.named dst "poly/layer")
         (Column.identity (Column.length category))
         (Column.identity (Column.length category))
         ~def2:0
         layer poly)
  in

(****)

  Format.eprintf "==== Multipolygons ====@.";
  let poly_index = filter_tags "multipolygon/poly_assoc" in
  Format.eprintf "Join (way)@.";
  let (way_index, _) =
    let way_poly = Column.open_in (Column.named "multipolygon" "way/poly") in
    Join.perform
      ~o2:(Column.named "surfaces/multi" "way/poly")
      (Column.identity (Column.length way_poly)) way_poly
      (Column.identity (Column.length poly_index)) poly_index
  in
  Format.eprintf "Project (way)@.";
  ignore
    (Projection.project ~o:(Column.named "surfaces/multi" "way/role")
       way_index (Column.open_in (Column.named "multipolygon" "way/role")));

  Format.eprintf "Join (way refs)@.";
  let (way_ref_index, _) =
    let way_ref_way =
      Column.open_in (Column.named "multipolygon" "way_refs/way") in
    Join.perform
      ~o2:(Column.named "surfaces/multi" "way_refs/way")
      (Column.identity (Column.length way_ref_way)) way_ref_way
      (Column.identity (Column.length way_index)) way_index
  in
  Format.eprintf "Project (way refs)@.";
  ignore
    (Projection.project ~o:(Column.named "surfaces/multi" "way_refs/node")
       way_ref_index
       (Column.open_in (Column.named "multipolygon" "way_refs/node")));
  ignore
    (Projection.project ~o:(Column.named "surfaces/multi" "way_refs/lat")
       way_ref_index
       (Column.open_in (Column.named "multipolygon" "way_refs/lat")));
  ignore
    (Projection.project ~o:(Column.named "surfaces/multi" "way_refs/lon")
       way_ref_index
       (Column.open_in (Column.named "multipolygon" "way_refs/lon")));

  compute_categories poly_index "multipolygon/poly_assoc" "surfaces/multi";

(****)

  Format.eprintf "==== Ways ====@.";
  let way_index = filter_tags "base/way_assoc" in
  let way_index =
    Projection.diff
      way_index (Column.open_in (Column.named "multipolygon" "removed_ways")) in
  Format.eprintf "Join (way refs)@.";
  let offset =
    Column.length (Column.open_in (Column.named "surfaces/multi" "way/poly"))
  in
  ignore
    (Join.perform
       ~o1:(Column.named "surfaces/simple" "way_refs/way")
       ~o2:(Column.named "surfaces/simple" "way_refs/node")
       (Column.identity ~offset (Column.length way_index)) way_index
       (Column.open_in (Column.named "base" "way_refs/node"))
       (Column.open_in (Column.named "base" "way_refs/way")));

  compute_categories way_index "base/way_assoc" "surfaces/simple";

  Format.eprintf "Associated latitude and longitude.@.";
  let map input =
    let l = Column.length input in
    let (o, o') = Sorting.perform input (Column.identity l) in
    fun renaming output ->
      let (o, o') =
        Join.perform renaming (Column.identity (Column.length renaming)) o' o in
      assert (Column.length o = l);
      snd (Sorting.perform ~o2:output o' o)
  in
  let m i o =
    ignore (map (Column.open_in
                   (Column.named "surfaces/simple" "way_refs/node")) i o)
  in
  m (Column.open_in (Column.named "base" "node/lat"))
    (Column.named "surfaces/simple" "way_refs/lat");
  m (Column.open_in (Column.named "base" "node/lon"))
    (Column.named "surfaces/simple" "way_refs/lon");

  Format.eprintf "=== Common processing ====@.";

  Format.eprintf "Table unions.@.";
  List.iter
    (fun col ->
      ignore
        (Column_ops.union ~o:(Column.named "surfaces" col)
           [Column.open_in (Column.named "surfaces/multi" col);
            Column.open_in (Column.named "surfaces/simple" col)]))
    ["poly/category"; "poly/layer";
     "way_refs/way"; "way_refs/node"; "way_refs/lat"; "way_refs/lon"];
  let way_poly =
    let len loc =
      Column.length (Column.open_in (Column.named loc "poly/category")) in
    Column.identity ~offset:(len "surfaces/multi") (len "surfaces/simple")
  in
  ignore
    (Column_ops.union ~o:(Column.named "surfaces" "way/poly")
       [Column.open_in (Column.named "surfaces/multi" "/way/poly");
        way_poly]);
  ignore
    (Column_ops.union ~o:(Column.named "surfaces" "way/role")
       [Column.open_in (Column.named "surfaces/multi" "/way/role");
        Column_ops.map (fun _ -> 0) way_poly]);

  Format.eprintf "Computing surface centers@.";
  let way_index =
    Column_ops.build_index ~o:(Column.named "surfaces" "way/refs")
      (Column.open_in (Column.named "surfaces" "way_refs/way"))
  in
  let way_index = Column.stream way_index in
  let poly_index =
    Column_ops.build_index ~o:(Column.named "surfaces" "poly/ways")
      (Column.open_in (Column.named "surfaces" "way/poly"))
  in
  let poly_index = Column.stream poly_index in
  let poly_lat = Column.open_out (Column.named "surfaces" "poly/lat") in
  let poly_lon = Column.open_out (Column.named "surfaces" "poly/lon") in
  let latitude = Column.open_in (Column.named "surfaces" "way_refs/lat") in
  let longitude = Column.open_in (Column.named "surfaces" "way_refs/lon") in
  let latitude = Column.stream latitude in
  let longitude = Column.stream longitude in
  let rec compute_center n min_lat max_lat min_lon max_lon =
    if n = 0 then begin
      Column.append poly_lat ((min_lat + max_lat) / 2);
      Column.append poly_lon ((min_lon + max_lon) / 2)
    end else begin
      let lat = Column.read latitude in
      let lon = Column.read longitude in
      compute_center (n - 1)
	(min min_lat lat) (max max_lat lat)
	(min min_lon lon) (max max_lon lon)
    end
  in
  let rec scan_polys i j =
    let i' = Column.read poly_index in
    if i' <> max_int then begin
      for k = 0 to i' - i - 2 do
        ignore (Column.read way_index)
      done;
      let j' = if i' > i then  Column.read way_index else j in
      compute_center (j' - j) max_int min_int max_int min_int;
      scan_polys i' j'
    end
  in
  scan_polys (Column.read poly_index) (Column.read way_index);
  let poly_lat = Column.freeze poly_lat in
  let poly_lon = Column.freeze poly_lon in

  Format.eprintf "Order.@.";
  let order =
    compute_order (Column.named "surfaces" "poly/order") poly_lat poly_lon in

  Format.eprintf "Reorder.@.";
  let sort order col =
    ignore
      (Sorting.perform ~o2:(Column.named "surfaces/sorted" col)
         order (Column.open_in (Column.named "surfaces" col)))
  in
  let propagate_order order reference =
    let ref_col = Column.open_in (Column.named "surfaces" reference) in
    let src_id = Column.identity (Column.length order) in
    let dst_id = Column.identity (Column.length ref_col) in
    let new_numbering =
      snd (Sorting.perform (snd (Sorting.perform order src_id)) src_id) in
    let (_, new_numbering) =
      Join.perform dst_id ref_col new_numbering src_id in
    snd (Sorting.perform
     (snd (Sorting.perform ~o1:(Column.named "surfaces/sorted" reference)
           new_numbering dst_id))
      dst_id)
  in
  sort order "poly/category";
  sort order "poly/layer";
  let way_order = propagate_order order "way/poly" in
  sort way_order "way/role";
  let ref_order = propagate_order way_order "way_refs/way" in
  sort ref_order "way_refs/node";
  sort ref_order "way_refs/lat";
  sort ref_order "way_refs/lon";

module Bbox = Rtree.Bbox

let leaf_size = 2048

let to_stream_2 tbl1 tbl2 =
  let s1 = Column.stream tbl1 in
  let s2 = Column.stream tbl2 in
  Data_stream.make
    (fun () ->
       let v1 = Column.read s1 in
       if v1 = max_int then None else
       Some (v1, Column.read s2))

let to_stream_3 tbl1 tbl2 tbl3 =
  let s1 = Column.stream tbl1 in
  let s2 = Column.stream tbl2 in
  let s3 = Column.stream tbl3 in
  Data_stream.make
    (fun () ->
       let v1 = Column.read s1 in
       if v1 = max_int then None else
       Some (v1, (Column.read s2, Column.read s3)))

let to_stream_4 tbl1 tbl2 tbl3 tbl4 =
  let s1 = Column.stream tbl1 in
  let s2 = Column.stream tbl2 in
  let s3 = Column.stream tbl3 in
  let s4 = Column.stream tbl4 in
  Data_stream.make
    (fun () ->
       let v1 = Column.read s1 in
       if v1 = max_int then None else
       Some (v1, (Column.read s2, Column.read s3, Column.read s4)))

let (>>) x f = Data_stream.map f x

let rtrees = ref []

let open_rtree name ratio =
  let (nm, st) = Rtree.open_out name in
  let ch = open_out (Column.file_in_database (Filename.concat name "ratio")) in
  Printf.fprintf ch "%d\n" ratio;
  close_out ch;
  let ch = open_out nm in
  let lengths = ref (Array.make (leaf_size / 8) 0) in
  let categories = ref (Array.make (leaf_size / 8) 0) in
  let layers = ref (Array.make (leaf_size / 8) 0) in
  let n = ref 0 in
  let buf = ref (String.make (256 + leaf_size) '\000') in
  let pos = ref 0 in
  let bbox = ref Bbox.empty in
  let last_lat = ref 0 in
  let last_lon = ref 0 in
  let flush_ways () =
    let len = (!n * 4 + 4 + !pos + leaf_size - 1) / leaf_size in
(*
Format.eprintf "%d %d %d@." len !n !pos;
*)
    output_int_2 ch len;
    output_int_2 ch !n;
    for i = 0 to !n - 1 do
      output_int_2 ch !lengths.(i);
      output_byte ch !categories.(i);
      output_byte ch (!layers.(i) + 128);
    done;
    Rtree.append st !bbox;
    for i = 1 to len - 1 do
      Rtree.append st Bbox.empty
    done;
    resize_string (len * leaf_size - !n * 4 - 4) buf;
    output ch !buf 0 (len * leaf_size - !n * 4 - 4);
    n := 0;
    pos := 0;
    bbox := Bbox.empty;
    last_lat := 0;
    last_lon := 0
  in
(*
let ch = open_out "/tmp/c" in
*)
  let rec add_polygon cat layer (outer_way, inner_ways) =
    let n' = 1 + List.length inner_ways in
    let pos' =
      List.fold_left
	(fun pos (lat, lon) ->
	   let pos = ref pos in
	   for i = 0 to Array.length lat - 2 do
             resize_string (!pos + 20) buf;
	     pos := write_signed_varint !buf !pos (lat.(i) - !last_lat);
(*
Printf.fprintf ch "%d\n" (lat.(i) - !last_lat);
*)
	     last_lat := lat.(i);
	     pos := write_signed_varint !buf !pos (lon.(i) - !last_lon);
(*
Printf.fprintf ch "%d\n" (lon.(i) - !last_lon);
*)
	     last_lon := lon.(i)
	   done;
	   !pos)
	!pos (outer_way :: inner_ways)
    in
    if !n > 0 && (!n + n') * 4 + 4 + pos' > leaf_size then begin
      flush_ways ();
      add_polygon cat layer (outer_way, inner_ways)
    end else begin
      resize_array (!n + n') lengths;
      resize_array (!n + n') categories;
      resize_array (!n + n') layers;
      !lengths.(!n) <- Array.length (fst outer_way) - 1;
      !categories.(!n) <- cat;
      !layers.(!n) <- layer;
      incr n;
      List.iter
	(fun (lat, _) ->
	   !lengths.(!n) <- Array.length lat - 1;
	   !categories.(!n) <- 0;
           !layers.(!n) <- 0;
	   incr n)
	inner_ways;
      pos := pos';
      let bbox' =
        { Bbox.
          min_lat = Array.fold_left min max_int (fst outer_way);
          max_lat = Array.fold_left max min_int (fst outer_way);
          min_lon = Array.fold_left min max_int (snd outer_way);
          max_lon = Array.fold_left max min_int (snd outer_way) }
      in
      bbox := Bbox.union !bbox bbox';
      if !n * 4 + 4 + pos' > leaf_size then flush_ways ()
    end
  in
  let close () =
    if !n > 0 then flush_ways ();
    Rtree.close_out st
  in
  rtrees := close :: !rtrees;
  add_polygon

let simplify_way ratio (lat, lon) =
  let (lat, lon) = Douglas_peucker.perform_int ratio lat lon in
  let delta = ratio / 2 - 1 in
  let l = Array.length lat in
  for i = 0 to l - 1 do
    lat.(i) <- (lat.(i) + delta) / ratio;
    lon.(i) <- (lon.(i) + delta) / ratio
  done;
  let j = ref 0 in
  for i = 1 to l - 1 do
    if lat.(i) <> lat.(!j) || lon.(i) <> lon.(!j) then begin
      incr j;
      lat.(!j) <- lat.(i);
      lon.(!j) <- lon.(i)
    end
  done;
  incr j;
  if !j = l then
    (lat, lon)
  else if !j < 4 then
    ([||], [||])
  else
    (Array.sub lat 0 !j, Array.sub lon 0 !j)

let simplify_polygon ratio (outer_way, inner_ways) =
  let non_empty (lat, _) = Array.length lat > 0 in
  let outer_way = simplify_way ratio outer_way in
  if non_empty outer_way then
    Some (outer_way,
          List.filter non_empty
            (List.map (fun w -> simplify_way ratio w) inner_ways))
  else
    None

let rescale_way ratio (lat, lon) =
  let l = Array.length lat in
  let delta = ratio / 2 - 1 in
  let lat = Array.copy lat in
  let lon = Array.copy lon in
  for i = 0 to l - 1 do
    lat.(i) <- (lat.(i) + delta) / ratio;
    lon.(i) <- (lon.(i) + delta) / ratio
  done;
  (lat, lon)

let rescale_polygon ratio (outer_way, inner_ways) =
  (rescale_way ratio outer_way,
   List.map (fun w -> rescale_way ratio w) inner_ways)

let _ =
  Format.eprintf "=== Building R-trees ====@.";

  let rtree ratio name =
    let add_polygon = open_rtree name ratio in
    fun cat layer ways area ->
      add_polygon cat layer (rescale_polygon ratio ways)
  in
  let rtree_with_filter level name =
    let scale = 256. /. 360. *. 2. ** level in
    let area = truncate (8. *. (10_000_000. /. scale) ** 2.) in
    let ratio = truncate (10_000_000. /. scale /. 2.) in
    let add_polygon = open_rtree name ratio in
    fun cat layer ways area' ->
      if area' >= area then
        match simplify_polygon ratio ways with
          Some ways -> add_polygon cat layer ways
        | None      -> ()
  in
  let add_small_polygon = rtree 10 "surfaces/rtrees/small" in
  let add_large_polygon = rtree 10 "surfaces/rtrees/large" in
  let add_polygon_12 = rtree_with_filter 12. "surfaces/rtrees/12" in
  let add_polygon_10 = rtree_with_filter 10. "surfaces/rtrees/10" in
  let add_polygon_09 = rtree_with_filter 9. "surfaces/rtrees/09" in
  let add_polygon_08 = rtree_with_filter 8. "surfaces/rtrees/08" in
  let add_polygon_07 = rtree_with_filter 7. "surfaces/rtrees/07" in
  let add_polygon_06 = rtree_with_filter 6. "surfaces/rtrees/06" in

  let small_area =
    let cut_off_level = 15.5 in
    let scale = 256. /. 360. *. 2. ** cut_off_level in
    truncate (8. *. (*64. *.*) (10_000_000. /. scale) ** 2.)
  in
  let _building = Surface.to_id `Building in

  let add_polygon cat layer ways =
    let (outer_way, inner_ways) = ways in
    let area =
      List.fold_left
        (fun a (lat, lon) -> a + Geometry.polygon_area lon lat) 0
        (outer_way :: inner_ways)
    in
    if
      area <= small_area
        ||
      (area <= 50_000_000 && cat = _building)
    then
      add_small_polygon cat layer ways area
    else
      add_large_polygon cat layer ways area;
    add_polygon_12 cat layer ways area;
    add_polygon_10 cat layer ways area;
    add_polygon_09 cat layer ways area;
    add_polygon_08 cat layer ways area;
    add_polygon_07 cat layer ways area;
    add_polygon_06 cat layer ways area
  in

  let col c = Column.named "surfaces/sorted" c in
  let nodes =
    Data_stream.group
      (to_stream_4
         (Column.open_in (col "way_refs/way"))
         (Column.open_in (col "way_refs/node"))
         (Column.open_in (col "way_refs/lat"))
         (Column.open_in (col "way_refs/lon")))
  in
  let ways =
    let way_poly = Column.open_in (col "way/poly") in
    to_stream_3
      (Column.identity (Column.length way_poly))
      way_poly
      (Column.open_in (col "way/role"))
  in
  let ways =
    Data_stream.unique_join ways nodes
    >> fun (_, ((poly, role), nodes)) -> (poly, (role, Array.of_list nodes))
  in
  let polys =
    let category = Column.open_in (col "poly/category") in
    let layer = Column.open_in (col "poly/layer") in
    to_stream_3 (Column.identity (Column.length category)) category layer
  in
  let polys = Data_stream.unique_join polys (Data_stream.group ways) in
  let rec split_multipolygon_rec multipolygon polygons outer_way inner_ways =
    match multipolygon with
      [] ->
	List.rev ((outer_way, List.rev inner_ways) :: polygons)
    | (0, way) :: rem ->
	split_multipolygon_rec
	  rem ((outer_way, List.rev inner_ways) :: polygons) way []
    | (_, way) :: rem ->
	split_multipolygon_rec rem polygons outer_way (way :: inner_ways)
  in
  let split_multipolygon multipolygon =
    match multipolygon with
      (0, way) :: rem -> split_multipolygon_rec rem [] way []
    | _               -> assert false
  in
  let len = Column.length (Column.open_in (col "poly/category")) in
  let t = Unix.gettimeofday () in
  Data_stream.consume polys
    (fun (idx, ((cat, layer), ways)) ->
       if idx land 4095 = 4095 then begin
         let p = float idx /. float len in
         let t' = Unix.gettimeofday () in
         Util.set_msg
           (Format.sprintf "writing polygons: %s %.0f%% eta %.0fs"
              (Util.progress_bar p) (p *. 100.)
              ((1. -. p) *. (t' -. t) /. p))
       end;
       let (_, nodes) = List.hd ways in
       let l = Array.length nodes in
       let (n, _, _) = nodes.(0) in
       let (n', _, _) = nodes.(l - 1) in
       if l > 1 && n = n' && cat <> Surface.none then begin
         let ways =
           List.map
             (fun (role, nodes) ->
                (role,
                 (Array.map (fun (_, lat, _) -> lat) nodes,
                  Array.map (fun (_, _, lon) -> lon) nodes)))
             ways
         in
	 let reverse a =
	   let l = Array.length a in
	   for i = 0 to l / 2 - 1 do
	     let j = l - 1 - i in
	     let v = a.(j) in
	     a.(j) <- a.(i);
	     a.(i) <- v
	   done
	 in
	 List.iter
	   (fun (role, (lat, lon)) ->
	      let sign = if role = 0 then 1 else -1 in
	      if Geometry.polygon_area lon lat * sign < 0 then begin
                reverse lat; reverse lon
              end)
	   ways;
	 List.iter (add_polygon cat layer) (split_multipolygon ways)
       end);
  Util.set_msg "";
  List.iter (fun close -> close ()) !rtrees;
