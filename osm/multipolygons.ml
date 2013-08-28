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
XXX
- Try to deal with partial relations
  ===> attempt to close paths

XXX
- Paranoiac mode for detecting issues
  ==> non-simple polygons
  ==> touching/overlapping polygons
  ==> tags
  ==> ...
- Should be more robust with respect to overlapping polygons
*)

(*
XXX Check touching polygons and edge intersections

Duplicate ways...

Containment:
   - there should be no strict segment intersection
     ===> when segment intersects, either they are equal or
          they are touching on an extremity

Output:
 - list of outer polygons plus their holes + associated tags
 - ways to be ignored

XXX Option not to special-case closed ways, to help find broken
multipolygon
*)

let _ = Printexc.record_backtrace true

let debug = Debug.make "polygon" "output multipolygon issues" []
let debug () = false

let _ = Column.set_database "/tmp/osm"

let col nm = Column.named "base" nm
let multi nm = Column.named "multipolygon/temp" nm
let output nm = Column.named "multipolygon" nm

module Tags =
  Set.Make
    (struct
       type t = int * int
       let compare : t -> t -> int =
         fun (x, y) (x', y') ->
         let c = compare x x' in if c <> 0 then c else compare y y'
     end)

let dict = Dictionary.load "strings"
let s v = try Dictionary.find dict v with Not_found -> -1

(****)

let _ =
  let _type = s"type" in
  let _multipolygon = s"multipolygon" in
  let _boundary = s"boundary" in

  (* Select multipolygons *)
  let index =
    Projection.filter_pred_2
      (Column.open_in (col "relation_assoc/key"))
      (Column.open_in (col "relation_assoc/val"))
      (fun k v -> k = _type && (v = _multipolygon || v = _boundary))
  in
  let idx1 =
    Projection.project ~o:(Column.named "foo" "idx1") index (Column.open_in (col "relation_assoc/idx")) in

  (* Remove relations with missing members *)
  let index =
    Projection.filter_pred (Column.open_in (col "relation_members/member"))
      (fun v -> v = -1)
  in
  let idx2 =
    Column_ops.unique ~o:(Column.named "foo" "idx2")
      (Projection.project index
	 (Column.open_in (col "relation_members/relation")))
  in
  let idx = Projection.diff ~o:(Column.named "foo" "idx") idx1 idx2 in

  (* Set of remaining relations *)
  ignore(
    Projection.project ~o:(multi "relation/id") idx
      (Column.open_in (col "relation/id")));

  (* Extract relation assocs *)
Format.eprintf "Join (relation assoc)@.";
  let (_, indices) =
    let assoc_idx = Column.open_in (col "relation_assoc/idx") in
    Join.perform
      ~o1:(multi "relation_assoc/idx")
      (Column.identity (Column.length idx)) idx
      (Column.identity (Column.length assoc_idx)) assoc_idx
  in
Format.eprintf "Project (relation assoc)@.";
  ignore(
    Projection.project ~o:(multi "relation_assoc/key") indices
      (Column.open_in (col "relation_assoc/key")));
  ignore(
    Projection.project ~o:(multi "relation_assoc/val") indices
      (Column.open_in (col "relation_assoc/val")));

  (* Extract members *)
  let (relations, members) =
    Join.perform
      (Column.identity (Column.length idx)) idx
      (Column.open_in (col "relation_members/member"))
      (Column.open_in (col "relation_members/relation"))
  in
  let (_, roles) =
    Join.perform
      (Column.identity (Column.length idx)) idx
      (Column.open_in (col "relation_members/role"))
      (Column.open_in (col "relation_members/relation"))
  in
  let (_, types) =
    Join.perform
      (Column.identity (Column.length idx)) idx
      (Column.open_in (col "relation_members/type"))
      (Column.open_in (col "relation_members/relation"))
  in
  if debug () then begin
    let index = Projection.filter_pred types (fun t -> t <> 1) in
    let relations = Column.stream (Projection.project index relations) in
    let members = Column.stream (Projection.project index members) in
    let types = Column.stream (Projection.project index types) in
    let relation_id = Column.open_in (multi "relation/id") in
    let relation_id' = Column.open_in (col "relation/id") in
    let node_id = Column.open_in (col "node/id") in
    let rec loop () =
      let r = Column.read relations in
      let m = Column.read members in
      let t = Column.read types in
      if r <> max_int then begin
	Format.eprintf "Omitting member %d (%s) of multipolygon %d.@."
	  (Column.get (if t = 0 then node_id else relation_id') m)
	  (if t = 0 then "node" else if t = 2 then "relation" else
	   assert false)
	  (Column.get relation_id r);
	loop ()
      end
    in
    loop ()
  end;
  (* Only keep way members *)
  let index = Projection.filter types 1 in
  ignore
    (Projection.project ~o:(multi "way/relation") index relations);
  let ways =
    Projection.project ~o:(multi "way/idx") index members in
  ignore (Projection.project ~o:(multi "way/role") index roles);

  let (ways', index) =
    Sorting.perform ways (Column.identity (Column.length ways)) in

  begin
Format.eprintf "Way assoc@.";
    let (indices, _) =
      let tbl = Column.open_in (col "way_assoc/idx") in
      Join.perform
        index ways'
        (Column.identity (Column.length tbl)) tbl
    in
    ignore (Sorting.permute ~o:(multi "way_assoc/idx") indices indices);
    let (indices, tbl) =
      let tbl = Column.open_in (col "way_assoc/key") in
      Join.perform
        index ways'
        tbl (Column.open_in (col "way_assoc/idx"))
    in
    ignore (Sorting.permute ~o:(multi "way_assoc/key") indices tbl);
    let (indices, tbl) =
      let tbl = Column.open_in (col "way_assoc/val") in
      Join.perform
        index ways'
        tbl (Column.open_in (col "way_assoc/idx"))
    in
    ignore (Sorting.permute ~o:(multi "way_assoc/val") indices tbl)
  end;
  
  let (index, nodes) =
    Join.perform
      index ways'
      (Column.open_in (col "way_refs/node"))
      (Column.open_in (col "way_refs/way"))
  in
  let (index, nodes) =
    Sorting.perform ~o1:(multi "node/way") ~o2:(multi "node/node")
      index nodes in

  let (nodes, index) =
    Sorting.perform nodes (Column.identity (Column.length nodes)) in
  let (lat, _) =
    let lat = Column.open_in (col "node/lat") in
    Join.perform lat (Column.identity (Column.length lat)) index nodes
  in
  ignore (Sorting.permute ~o:(multi "node/lat") index lat);
  let (lon, _) =
    let lon = Column.open_in (col "node/lon") in
    Join.perform lon (Column.identity (Column.length lon)) index nodes
  in
  ignore (Sorting.permute ~o:(multi "node/lon") index lon)

(****)

(*
let array_for_all f a =
  let l = Array.length a - 1 in
  let rec for_all i =
    i = l || (f a.(i) && for_all (i + 1))
  in
  for_all 0

let array_exists f a =
  let l = Array.length a - 1 in
  let rec exists i =
    i < l && (f a.(i) || exists (i + 1))
  in
  exists 0
*)
let for_all n f =
  let rec for_all i = i = n || (f i && for_all (i + 1)) in
  for_all 0

let exists n f =
  let rec exists i = i < n && (f i || exists (i + 1)) in
  exists 0

let rec fold n f v =
  let n = n - 1 in
  if n < 0 then v else fold n f (f n v)

(*
let point_in_polygon x y px py =
  let n = ref false in
  let l = Array.length px in
  for i = 0 to l - 1 do
    let j = (i + 1) mod l in
    if
      ((py.(i) < y && py.(j) >= y) || (py.(j) < y && py.(i) >= y))
	&&
      (px.(i) < x || px.(j) < x)
	&&
      ((px.(i) < x && px.(j) < x) ||
       (px.(i) +. (y-.py.(i)) /. (py.(j)-.py.(i)) *. (px.(j)-.px.(i)) < x))
    then
      n := not !n
  done;
  !n
*)

let polygon_coords l =
  let n =
    List.fold_left
      (fun n (_, _, _, nodes, _) -> n + Array.length nodes - 1) 1 l in
  let pn = Array.make n 0 in
  let px = Array.make n 0 in
  let py = Array.make n 0 in
  ignore
    (List.fold_left
       (fun n (forward, _, _, nodes, _) ->
	  let len = Array.length nodes - 1 in
	  if forward then
	    for i = 0 to len - 1 do
	      let (node, lat, lon) = nodes.(i) in
              pn.(i + n) <- node;
	      px.(i + n) <- lon;
	      py.(i + n) <- lat
	    done
	  else
	    for i = 0 to len - 1 do
	      let (node, lat, lon) = nodes.(len - i) in
	      pn.(i + n) <- node;
	      px.(i + n) <- lon;
	      py.(i + n) <- lat
	    done;
	  n + len)
       0 l);
  pn.(n - 1) <- pn.(0);
  px.(n - 1) <- px.(0);
  py.(n - 1) <- py.(0);
  (pn, (px, py))

let write_coords i (px, py) =
  let ch = open_out (Format.sprintf "/tmp/%d" i) in
  for i = 0 to Array.length px - 1 do
    Printf.fprintf ch "%d %d\n" px.(i) py.(i)
  done;
  Printf.fprintf ch "%d %d\n" px.(0) py.(0);
  close_out ch

let gnuplot coords =
  for i = 0 to Array.length coords - 1 do
    write_coords i (snd coords.(i))
  done;
  ignore (
  Sys.command
    ("cd /tmp; gnuplot -p -e \"plot " ^
     String.concat ","
       (Array.to_list
	  (Array.mapi (fun i _ -> Format.sprintf "'%d' with lines" i) coords))
     ^ "\"")
 );
  ignore (input_line stdin)

let way_id = Column.open_in (col "way/id")
let node_id = Column.open_in (col "node/id")

(*
let ch = open_out "/tmp/tags"
*)
let ignored_tags = List.map s ["type"; "created_by"; "source"; "note"]
let has_tags assoc =
  not (List.for_all (fun (k, _) -> List.memq k ignored_tags) assoc)
let surface_keys =
 List.map s
   ["natural"; "amenity"; "building"; "landuse"; "leisure";
    "man_made"; "natural"; "sport"; "water"; "waterway"; "wood"]

let multipolygon_output () =
  let removed_ways = Column.open_out (multi "removed_ways") in
  let node_way = Column.open_out (output "way_refs/way") in
  let node_node = Column.open_out (output "way_refs/node") in
  let node_lat = Column.open_out (output "way_refs/lat") in
  let node_lon = Column.open_out (output "way_refs/lon") in
  let way_poly = Column.open_out (output "way/poly") in
  let way_role = Column.open_out (output "way/role") in
  let poly_assoc_idx = Column.open_out (output "poly_assoc/idx") in
  let poly_assoc_key = Column.open_out (output "poly_assoc/key") in
  let poly_assoc_val = Column.open_out (output "poly_assoc/val") in
  let last_poly = ref 0 in
  let last_way = ref 0 in
  
  let output_multipolygon removed assoc groups =
    List.iter (fun w -> Column.append removed_ways w) removed;
    List.iter
      (fun (k, v) ->
         Column.append poly_assoc_idx !last_poly;
         Column.append poly_assoc_key k;
         Column.append poly_assoc_val v)
      assoc;
    let write_nodes r (pn, (px, py)) =
      Column.append way_poly !last_poly;
      Column.append way_role r;
      for i = 0 to Array.length px - 1 do
        Column.append node_way !last_way;
        Column.append node_node pn.(i);
        Column.append node_lon px.(i);
        Column.append node_lat py.(i)
      done;
      incr last_way
    in
    List.iter
      (fun (p, l) ->
         write_nodes 0 p;
         List.iter (fun p -> write_nodes 1 p) l)
      groups;
    incr last_poly
  in
  let finish_output () =
    let removed_ways = Column.freeze removed_ways in
    let (removed_ways, _) =
      Sorting.perform ~o1:(multi "sorted_removed")
        removed_ways (Column.identity (Column.length removed_ways))
    in
    ignore (Column_ops.unique ~o:(output "removed_ways") removed_ways);
    Column.close_out node_way;
    Column.close_out node_node;
    Column.close_out node_lat;
    Column.close_out node_lon;
    Column.close_out way_poly;
    Column.close_out way_role;
    Column.close_out poly_assoc_idx;
    Column.close_out poly_assoc_key;
    Column.close_out poly_assoc_val
  in
  (output_multipolygon, finish_output)

let handle_relation output_multipolygon r ways r_assoc =
(*
  let surface_tags = List.map s ["natural"; "landuse"; "building"; "waterway"; "highway"; "leisure"; "amenity"; "place"; "shop"; "water"] in
*)

  let _outer = s"outer" in
  let _inner = s"inner" in
  let _void = s"" in
  let h = Hashtbl.create 101 in
  let add n1 n2 dir w role nodes assoc =
    Hashtbl.replace h n1
      ((n2, dir, w, role, nodes, assoc) ::
       try Hashtbl.find h n1 with Not_found -> [])
  in
  let polygons = ref [] in
  List.iter
    (fun (w, role, nodes, assoc) ->
       if Array.length nodes > 0 then begin
	 let (n1, _, _) = nodes.(0) in
	 let (n2, _, _) = nodes.(Array.length nodes - 1) in
	 if n1 = n2 then
	   polygons := [(true, w, role, nodes, assoc)] :: !polygons
	 else begin
	   add n1 n2 true w role nodes assoc;
	   add n2 n1 false w role nodes assoc
	 end
       end)
    ways;
  let nodes = ref [] in
  let ok = ref true in
  Hashtbl.iter
    (fun n l ->
       nodes := n :: !nodes;
       if List.length l <> 2 then begin
	 ok := false;
	 if debug () then begin
	   Format.eprintf
	     "Failed to assemble multipolygon %d: node %d shared by %d ways:"
	     r (Column.get node_id n)
	     (List.length l);
	   List.iter
	     (fun (_, _, w, _, _, _) ->
	        Format.eprintf " %d" (Column.get way_id w)) l;
	   Format.eprintf "@."
	 end
       end)
     h;
  if !ok then begin
    let rec get_polygon n dir w role nodes assoc lst =
      let lst = (dir, w, role, nodes, assoc) :: lst in
      match try Hashtbl.find h n with Not_found -> [] with
	[((_, _, w1, _, _, _) as i1); ((_, _, w2, _, _, _) as i2)] ->
	  let (n', dir', w', role', nodes', assoc') =
            if w1 = w then i2 else i1 in
	  Hashtbl.remove h n;
	  get_polygon n' dir' w' role' nodes' assoc' lst
      | _ ->
	  List.rev lst
    in
    let rec loop nodes =
      match nodes with
	n :: rem ->
	  if Hashtbl.mem h n then begin
	    match Hashtbl.find h n with
	      (n', dir, w, role, nodes, assoc) :: _ ->
		Hashtbl.remove h n;
		let l = get_polygon n' dir w role nodes assoc [] in
		polygons := l :: !polygons
	    | _ ->
		assert false
	  end;
	  loop rem
      | [] ->
	  ()
    in
    loop !nodes;
    if debug () then Format.eprintf "==== %d@." r;

(*
    if has_tag r_assoc &&
      not (List.exists (fun (k, _) -> List.memq k surface_tags) r_assoc)
    then begin
      Printf.fprintf ch "==== %d\n" r;
      List.iter
        (fun (k, v) ->
          Printf.fprintf ch "%s=%s\n"
            (Dictionary.get dict k) (Dictionary.get dict v))
        r_assoc
    end;

    let print_assoc l =
      List.iter
        (fun (k, v) ->
           Format.eprintf "%s=%s@."
             (Dictionary.get dict k) (Dictionary.get dict v))
        l
    in
    print_assoc r_assoc;
    List.iter
      (fun (w, role, nodes, assoc) ->
         Format.eprintf "%d (%s):@."
           (Column.get way_id w) (Dictionary.get dict role);
         print_assoc assoc)
      ways;
*)

    let print_polygon l =
      Format.eprintf "@[<2>[";
      List.iter
	(fun (dir, w, role, _, _) ->
	   Format.eprintf "@ %d(%s)/%b"
	     (Column.get way_id w) (Dictionary.get dict role) dir)
	l;
      Format.eprintf "@ ]@]@."
    in
    let polygons = Array.of_list !polygons in
    if debug () then Array.iter print_polygon polygons;
(*
*)
    let len = Array.length polygons in
    let inclusion = Array.create_matrix len len false in
    let coords = Array.map polygon_coords polygons in
(*
    let print_coords (px, py) =
      Format.eprintf "@[<2>[";
      Array.iteri
	(fun i x -> Format.eprintf "@ %.0f,%.0f" x py.(i))
	px;
      Format.eprintf "@ ]@]@."
    in
    Array.iter print_coords coords;
*)
try
    for i = 0 to len - 1 do
      if debug () then begin
        if
          let (_, (px, py)) = coords.(i) in
          not (Geometry.is_simple_polygon px py)
        then begin
          Format.eprintf "NOT SIMPLE (%d)@." i;
          raise Exit
        end
      end;
      for j = 0 to len - 1 do
	if i <> j then
	  inclusion.(i).(j) <-
            Geometry.polygon_in_polygon (snd coords.(i)) (snd coords.(j))
      done
    done;
    if debug () then
      for i = 0 to len - 1 do
        for j = 0 to len - 1 do
          Format.eprintf "%d" (if inclusion.(i).(j) then 1 else 0)
        done;
        Format.eprintf "@."
      done;
    for i = 0 to len - 1 do
      for j = 0 to i - 1 do
        if inclusion.(i).(j) && inclusion.(j).(i) then begin
          let mi =
            Geometry.polygon_mostly_in_polygon
              (snd coords.(i)) (snd coords.(j))
          in
          let mj =
            Geometry.polygon_mostly_in_polygon
              (snd coords.(j)) (snd coords.(i))
          in
	  if debug () then
            Format.eprintf "OVERLAPPING POLYGONS %d (%b) %d (%b)@." i mi j mj;
          if mi && mj then begin
            raise Exit
          end else begin
            inclusion.(i).(j) <- mi;
            inclusion.(j).(i) <- mj
          end
        end
      done
    done;
    let available = Array.make len true in
    let rec find_outer i =
      if i = len then begin
	Format.eprintf "OVERLAPPING POLYGONS@.";
	raise Exit
      end else if
	available.(i) &&
	for_all len (fun j -> not (available.(j) && inclusion.(i).(j)))
      then
	i
      else
	find_outer (i + 1)
    in
    let groups = ref [] in
    while exists len (fun i -> available.(i)) do
      let p = find_outer 0 in
      let l =
        fold len
	  (fun j l ->
	     if
	       inclusion.(j).(p)
		 &&
	       for_all len
		 (fun k -> p = k || not (available.(k) && inclusion.(j).(k)))
	     then
	       j :: l
	     else
	       l)
	  []
      in
      groups := (p, l) :: !groups;
      List.iter (fun i -> available.(i) <- false) (p :: l)
    done;
    if debug () then
      List.iter
        (fun (p, l) ->
           Format.eprintf "%d + " p; print_polygon polygons.(p);
           List.iter
             (fun p -> Format.eprintf "%d - " p; print_polygon polygons.(p)) l)
        !groups;
    let check_way_role role p =
      List.for_all (fun (_, _, role', _, _) -> role' = role || role' = _void)
	polygons.(p)
    in
    if
      List.exists
	(fun (p, l) ->
	   not (check_way_role _outer p &&
		List.for_all (fun p -> check_way_role _inner p) l))
	!groups
    then begin
      if debug () then
        Format.eprintf "ROLE MISMATCH!@.";
(*
      raise Exit
*)
    end;

(*
- Tag is union of relation tags and outer way tags
- Do not generate polygons for (closed) outer ways
- Do not generate polygons for (closed) inner ways if their tags match
*)
    let tags =
      List.map
        (fun (w, role, nodes, assoc) ->
           if role = _outer || role = _void then
             List.fold_left
               (fun t (k, v) ->
                  if List.memq k ignored_tags then t else
                    Tags.add (k, v) t)
               Tags.empty assoc
           else
             Tags.empty)
        ways
    in
    let tags = List.filter (fun t -> not (Tags.is_empty t)) tags in
    let tags =
      match tags with
        [] -> Tags.empty
      | t :: r -> List.fold_left Tags.inter t r
    in
    let w_tags = Tags.elements tags in

    let tags = Hashtbl.create 17 in
    let add_tags l =
      List.iter
        (fun (k, v) ->
           if debug () then begin try
             let  v' = Hashtbl.find tags k in
             if v <> v' && not (List.memq k ignored_tags) then
               Format.eprintf "TAG MISMATCH %d %s=%s/%s@." r
                 (Dictionary.get dict k)
                 (Dictionary.get dict v)
                 (Dictionary.get dict v')
           with Not_found ->
             ()
           end;
           Hashtbl.replace tags k v)
        l
    in
    add_tags w_tags;
    add_tags r_assoc;
    let tags = Hashtbl.fold (fun k v l -> (k, v) :: l) tags [] in

    let removed_ways = ref [] in
    List.iter
      (fun (p, l) ->
         begin match polygons.(p) with
           [(_, w, _, _, assoc)] -> removed_ways := w :: !removed_ways
         | _                     -> ()
         end;
         List.iter
          (fun p ->
             match polygons.(p) with
               [(_, w, _, _, assoc)] ->
                 if
                   List.exists (fun t -> List.mem t tags)
                     assoc &&
                   List.for_all
                     (fun ((k, v) as t) ->
                        not (List.memq k surface_keys) || List.mem t tags)
                     assoc
                 then
                   removed_ways := w :: !removed_ways
(*
Format.eprintf "- %d (%d) << %d@." w (Column.get way_id w) r;
*)
             | _ ->
                 ())
           l)
      !groups;
    let groups =
      List.map (fun (p, l) -> (coords.(p), List.map (fun p -> coords.(p)) l))
	!groups
    in
    if has_tags tags then
      output_multipolygon !removed_ways tags groups

with Exit ->
  if debug () then gnuplot coords
  end

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

let _ =
  let nodes =
    Data_stream.group
      (to_stream_4
         (Column.open_in (multi "node/way"))
         (Column.open_in (multi "node/node"))
         (Column.open_in (multi "node/lat"))
         (Column.open_in (multi "node/lon")))
(*
>> fun ((i, n) as r) -> Format.eprintf "%d: %d@." i (List.length n); r
*)
  in
  let way_assoc =
    Data_stream.group
      (to_stream_3
         (Column.open_in (multi "way_assoc/idx"))
         (Column.open_in (multi "way_assoc/key"))
         (Column.open_in (multi "way_assoc/val")))
  in
  let ways =
    let way_idx = Column.open_in (multi "way/idx") in
    to_stream_4
      (Column.identity (Column.length way_idx))
      way_idx
      (Column.open_in (multi "way/relation"))
      (Column.open_in (multi "way/role"))
  in
  let ways =
    Data_stream.unique_join
      ways
      ~def2:([],[]) (Data_stream.unique_join ~def1:[] nodes ~def2:[] way_assoc)
    >> fun (_, ((way, relation, role), (nodes, assoc))) ->
(*
Format.eprintf "%d => %d@." way (List.length nodes);
*)
         (relation, (way, role, Array.of_list nodes, assoc))
  in
  let relation_assoc =
    Data_stream.group
      (to_stream_3
         (Column.open_in (multi "relation_assoc/idx"))
         (Column.open_in (multi "relation_assoc/key"))
         (Column.open_in (multi "relation_assoc/val")))
  in
  let relations =
    let relation_id = Column.open_in (multi "relation/id") in
    to_stream_2
      (Column.identity (Column.length relation_id))
      relation_id
  in
  let relations =
    Data_stream.unique_join
      relations
      ~def2:([],[])
      (Data_stream.unique_join ~def1:[] (Data_stream.group ways)
         ~def2:[] relation_assoc)
    >> fun (idx, (relation, (ways, assoc))) -> (idx, relation, ways, assoc)
  in
  let len = Column.length (Column.open_in (multi "relation/id")) in
  let t = Unix.gettimeofday () in
  let _boundary = s"boundary" in
  let (output_multipolygon, finish_output) = multipolygon_output () in
  Data_stream.consume relations
    (fun (idx, relation, ways, assoc) ->
       if not (debug ()) then begin
         let p = float idx /. float len in
         let t' = Unix.gettimeofday () in
         Util.set_msg
           (Format.sprintf "processing multipolygons: %s %.0f%% eta %.0fs"
              (Util.progress_bar p) (p *. 100.)
              ((1. -. p) *. (t' -. t) /. p))
       end;
       handle_relation output_multipolygon relation ways assoc);
  Util.set_msg "";
  finish_output ()
