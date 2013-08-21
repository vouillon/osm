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
- simplification should depend on latitude
  (should be done after projection)
*)

let leaf_size = 2048
let max_polygon_size = 200

(****)

let _ = Printexc.record_backtrace true
let _ = Column.set_database "/tmp/osm"

(****)

let read_big_int ch =
  let i3 = input_byte ch in
  let i2 = input_byte ch in
  let i1 = input_byte ch in
  let i0 = input_byte ch in
  i0 lor (i1 lsl 8) lor (i2 lsl 16) lor (i3 lsl 24)

let read_lit_int ch =
  let i0 = input_byte ch in
  let i1 = input_byte ch in
  let i2 = input_byte ch in
  let i3 = input_byte ch in
  i0 lor (i1 lsl 8) lor (i2 lsl 16) lor (i3 lsl 24)

let read_float ch =
  let i0 = Int64.of_int (read_lit_int ch) in
  let i1 = Int64.of_int (read_lit_int ch) in
  Int64.float_of_bits (Int64.logor i0 (Int64.shift_left i1 32))

let build_rings l =
  let (lst, rem) =
    List.partition
      (fun (x, y) ->
         let l = Array.length x in x.(0) = x.(l - 1) && y.(0) = y.(l - 1))
      l
  in
  let succ = Hashtbl.create 17 in
  let pred = Hashtbl.create 17 in
  let rem = Array.of_list rem in
  Array.iteri
    (fun i (x, y) ->
       let l = Array.length x in
       Hashtbl.add succ (x.(0), y.(0)) i;
       Hashtbl.add pred (x.(l - 1), y.(l - 1)) i)
    rem;
  let lst = ref lst in
  let rec follow i l =
    let (x, y) = rem.(i) in
    let len = Array.length x in
    try
      let k = Hashtbl.find pred (x.(0), y.(0)) in
      let l = (Array.sub x 1 (len - 1), Array.sub y 1 (len - 1)) :: l in
      follow k l
    with Not_found ->
      (x, y) :: l
  in
  for i = 0 to Array.length rem - 1 do
    let (x, y) = rem.(i) in
    let len = Array.length x in
    if not (Hashtbl.mem succ (x.(len - 1), y.(len - 1))) then begin
      let l = follow i [] in
      let x = Array.concat (List.map fst l) in
      let y = Array.concat (List.map snd l) in
      let len = Array.length x in
      let (x, y) =
        if x.(0) < -1799999000. && x.(len - 1) < -1799999000. then begin
          x.(0) <- -1800000000.; x.(len - 1) <- -1800000000.;
          (Array.append x [|x.(0)|], Array.append y [|y.(0)|])
        end else if
          x.(0) > 1799999000. && x.(len - 1) > 1799999000.
        then begin
          x.(0) <- 1800000000.; x.(len - 1) <- 1800000000.;
          (Array.append x [|x.(0)|], Array.append y [|y.(0)|])
        end else if
          x.(0) = -1800000000. && x.(len -1) = 1800000000.
        then begin
          (* Antartica *)
          (Array.append x [|1800000000.; -1800000000.; x.(0)|],
           Array.append y [| 850000000.;   850000000.; y.(0)|])
        end else begin
Format.eprintf "%f %f - %f %f@." x.(0) y.(0) x.(len - 1) y.(len - 1);
          assert false
        end
      in
      lst := (x, y) :: !lst
    end
  done;
  !lst

let read_record ch =
  let num = read_big_int ch in
  let len = read_big_int ch in
  ignore (num, len);
  let typ = read_lit_int ch in
  assert (typ = 3); (* polyline *)

  for i = 0 to 3 do ignore (read_float ch) done; (* bbox *)

  let num_parts = read_lit_int ch in
  assert (num_parts = 1);

  let num_points = read_lit_int ch in

  ignore (read_lit_int ch); (* first and unique part *)

  let lon = Array.make (num_points - 1) 0. in
  let lat = Array.make (num_points - 1) 0. in

  for i = 0 to num_points - 2 do
    lon.(i) <- 10_000_000. *. read_float ch;
    lat.(i) <- 10_000_000. *. read_float ch
  done;
  let lon0 = 10_000_000. *. read_float ch in
  let lat0 = 10_000_000. *. read_float ch in
  ((lon, lat), (lon0, lat0))

let open_file () =
  let ch = open_in Sys.argv.(1) in
  let magic = read_big_int ch in
  if magic = 9994 then begin
    seek_in ch 0;
    ch
  end else if magic = 0x504b0304 then begin
    close_in ch;
    let z = Zip.open_in Sys.argv.(1) in
    let e =
      try
        Zip.find_entry z "coastlines-split-4326/lines.shp"
      with Not_found ->
        Util.fail "Zip entry 'coastlines-split-4326/lines.shp' not found"
    in
    let (r, w) = Unix.pipe () in
    match Unix.fork () with
      0 ->
        Unix.close r;
        let ch = Unix.out_channel_of_descr w in
        Zip.copy_entry_to_channel z e ch;
        flush ch;
        exit 0
    | _ ->
        Unix.close w;
        Unix.in_channel_of_descr r
  end else
    Util.fail (Format.sprintf "bad file format")

let load () =
  Format.eprintf "==== Loading.@.";

  let ch = open_file () in

  for i = 0 to 24 do
    ignore (read_big_int ch)
  done;

(*
  let a = read_float ch in 
  Format.eprintf "%f@." a;
  let a = read_float ch in 
  Format.eprintf "%f@." a;
  let a = read_float ch in 
  Format.eprintf "%f@." a;
  let a = read_float ch in 
  Format.eprintf "%f@." a;
*)

  let lst = ref [] in
  let cur = ref [] in
  let lon0' = ref 0. in
  let lat0' = ref 0. in
  begin try
    while true do
      let ((lon, lat), (lon0, lat0)) = read_record ch in
      if !cur <> [] && (lat.(0) <> !lat0' || lon.(0) <> !lon0') then begin
        let path = List.rev (([|!lon0'|], [|!lat0'|]) :: !cur) in
        let lon'' = Array.concat (List.map fst path) in
        let lat'' = Array.concat (List.map snd path) in
        lst := (lon'', lat'') :: !lst;
        cur := []
      end;
      cur := (lon, lat) :: !cur;
      lon0' := lon0;
      lat0' := lat0
    done
  with End_of_file -> () end;
  let polys = build_rings !lst in
(*
Format.eprintf "%d@." (List.length polys);
*)
  polys

(**** Quickselect ****)

let swap (tbl : float array) i j =
  let v = tbl.(i) in tbl.(i) <- tbl.(j); tbl.(j) <- v

let partition (tbl : float array) left right pivot =
  let p = tbl.(pivot) in
  swap tbl pivot right;
  let j = ref left in
  for i = left to right - 1 do
    if tbl.(i) < p then begin
      swap tbl i !j;
      incr j
    end
  done;
  swap tbl right !j;
  !j

let rec select tbl left right k =
  if left = right then
    tbl.(left)
  else begin
    let pivot = Random.int (right - left + 1) + left in
    let pivot = partition tbl left right pivot in
    if pivot = k then
      tbl.(pivot)
    else if k < pivot then
      select tbl left (pivot - 1) k
    else
      select tbl (pivot + 1) right k
  end

let median tbl =
  let len = Array.length tbl in
  select tbl 0 (len - 1) (len / 2)

let poly_median polys =
  let l = List.rev_map fst polys in (* Tail recursive! *)
  median (Array.concat l)

let stats polys =
  let a = Array.concat (List.rev_map fst polys) in (* Tail recursive! *)
  let min = ref max_float in
  let max = ref (-. max_float) in
  for i = 0 to Array.length a - 1 do
    if a.(i) < !min then min := a.(i);
    if a.(i) > !max then max := a.(i)
  done;
  let min' = ref max_float in
  let max' = ref (-. max_float) in
  for i = 0 to Array.length a - 1 do
    if a.(i) < !min' && a.(i) > !min then min' := a.(i);
    if a.(i) > !max' && a.(i) < !max then max' := a.(i)
  done;
  (!min', median a, !max')

(****)

let swap_coord polys = List.map (fun (x, y) -> (y, x)) polys

let ring_bbox (x, y) : float * float * float * float =
  let xmin = ref x.(0) in
  let ymin = ref y.(0) in
  let xmax = ref x.(0) in
  let ymax = ref y.(0) in
  for i = 1 to Array.length x - 1 do
    if x.(i) < !xmin then xmin := x.(i);
    if y.(i) < !ymin then ymin := y.(i);
    if x.(i) > !xmax then xmax := x.(i);
    if y.(i) > !ymax then ymax := y.(i)
  done;
  (!xmin, !ymin, !xmax, !ymax)

let min' x y : float = if x < y then x else y
let max' x y : float = if x < y then y else x

let bbox polys =
  match polys with
    [] -> assert false
  | r :: rem ->
      List.fold_left
        (fun (xmin, ymin, xmax, ymax) r ->
           let (xmin', ymin', xmax', ymax') = ring_bbox r in
           (min' xmin xmin', min' ymin ymin',
            max' xmax xmax', max' ymax ymax'))
        (ring_bbox r) rem

let size polys = List.fold_left (fun n (x, _) -> n + Array.length x) 0 polys
let size polys =
  List.fold_left (fun n (x, _) -> max n (Array.length x)) 0 polys

(****)

(*
let surface =
  Cairo.SVG.create ~fname:"/tmp/bar.svg"  ~width:360. ~height:180.
let ctx = Cairo.create surface
let _ = Cairo.translate ctx 180. 90.

let show = false
*)
let d = false
let next_perc = ref 0.005

let rec split_rec dir t0 t1 t2 polys l =
  let sz = size polys in
if d then Format.eprintf "size: %d (%d)@." sz (List.length polys);
  if sz > max_polygon_size then begin
    let (xmin, ymin, xmax, ymax) = bbox polys in
if d then Format.eprintf "%f %f - %f %f@." xmin ymin xmax ymax;
    if xmax -. xmin < ymax -. ymin then
      split_rec (not dir) t0 t1 t2 (swap_coord polys) l
    else begin
      let m' = ((xmax +. xmin) /. 2.) in
      let (m1, m, m2) = stats polys in
if d then Format.eprintf "%f %f %f / %f@." m1 m m2 m';
      let m =
        if m2 < m' then m2 else
        if m1 > m' then m1 else
        if m > (xmin +. m') /. 2. && m < (xmax +. m') /. 2. then
          m
        else
          m'
      in
if d then Format.eprintf "Cutting at %f@." m;
      let (left, right) = Clipping.perform m polys in
      let l = split_rec dir t0 t1 ((t1 +. t2) /. 2.) left l in
      split_rec dir t0 ((t1 +. t2) /. 2.) t2 right l
    end
  end else begin
if t2 >= !next_perc then begin
Util.set_msg
  (Format.sprintf "splitting polygons: %s %.0f%% eta %.0fs"
     (Util.progress_bar t2) (t2 *. 100.)
     ((1. -. t2) *. (Unix.gettimeofday () -. t0) /. t2));
(*Format.eprintf "%.1f%%@." t2;*)
next_perc := !next_perc +. 0.005
end;
    let polys = if dir then polys else swap_coord polys in
(*
if show then begin
List.iter
  (fun (x, y) ->
     Cairo.move_to ctx (x.(0) /. 10_000_000.) (y.(0) /. 10_000_000.);
     for i = 1 to Array.length x - 2 do
       Cairo.line_to ctx (x.(i) /. 10_000_000.) (y.(i) /. 10_000_000.)
     done;
     Cairo.Path.close ctx)
  polys;
Cairo.fill ctx;
if t2 > 0.2 then
(Cairo.Surface.finish surface; exit 0)
end;
*)
    polys @ l
  end

(****)

module Bbox = Rtree.Bbox

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

let open_rtree name ratio =
  let (nm, st) = Rtree.open_out name in
  let ch = open_out (Column.file_in_database (Filename.concat name "ratio")) in
  Printf.fprintf ch "%d\n" ratio;
  close_out ch;
  let ch = open_out nm in
  let lengths = Array.make (leaf_size / 4) 0 in
  let n = ref 0 in
  let buf = String.make (2 * leaf_size) '\000' in
  let pos = ref 0 in
  let bbox = ref Bbox.empty in
  let last_lat = ref 0 in
  let last_lon = ref 0 in
  let flush_ways () =
    output_int_2 ch !n;
    for i = 0 to !n - 1 do
      output_int_2 ch lengths.(i);
    done;
    Rtree.append st !bbox;
    output ch buf 0 (leaf_size - !n * 2 - 2);
    n := 0;
    pos := 0;
    bbox := Bbox.empty;
    last_lat := 0;
    last_lon := 0
  in
  let rec add_polygon (lon, lat) =
    let pos' = ref !pos in
    for i = 0 to Array.length lat - 2 do
      pos' := write_signed_varint buf !pos' (lat.(i) - !last_lat);
      last_lat := lat.(i);
      pos' := write_signed_varint buf !pos' (lon.(i) - !last_lon);
      last_lon := lon.(i)
    done;
    let pos' = !pos' in
    if (!n + 1) * 2 + 2 + pos' > leaf_size then begin
      assert (!n > 0);
      flush_ways ();
      add_polygon (lon, lat)
    end else begin
      lengths.(!n) <- Array.length lat - 1;
      incr n;
      pos := pos';
      let max x y : int = if x > y then x else y in
      let min x y : int = if x > y then y else x in
      let bbox' =
        { Bbox.
          min_lat = Array.fold_left min max_int lat;
          max_lat = Array.fold_left max min_int lat;
          min_lon = Array.fold_left min max_int lon;
          max_lon = Array.fold_left max min_int lon }
      in
      bbox := Bbox.union !bbox bbox';
      if !n * 2 + 2 + pos' > leaf_size then flush_ways ()
    end
  in
  let close () =
    if !n > 0 then flush_ways ();
    Rtree.close_out st;
    close_out ch
  in
  (add_polygon, close)

(****)

let simplify level polys =
  Format.eprintf "==== Simplifying.@.";
  (* We multiply by 4. to get good approximations near the poles *)
  let scale = 256. /. 360. *. 2. ** level *. 4. in
  let small_area = (10_000_000. /. scale) ** 2. in
  let ratio = float (truncate (10_000_000. /. scale /. 2.)) in
  (truncate ratio,
   List.fold_left
     (fun lst (lon, lat) ->
        let (lon, lat) = Douglas_peucker.perform ratio lon lat in
        if
          abs_float (Geometry.polygon_area_float lon lat) <= small_area
        then
          lst
        else
          (lon, lat) :: lst)
     [] polys)

(****)

let rescale_ring ratio (lon, lat) =
  let l = Array.length lat in
  let delta = ratio / 2 - 1 in
  let lat' = Array.make l 0 in
  let lon' = Array.make l 0 in
  for i = 0 to l - 1 do
    lat'.(i) <- (truncate lat.(i) + delta) / ratio;
    lon'.(i) <- (truncate lon.(i) + delta) / ratio
  done;
  (lon', lat')

let build_rtree name ratio polys =
  Format.eprintf "==== Splitting.@.";
  next_perc := 0.005;
  let l = split_rec true (Unix.gettimeofday ()) 0. 1. polys [] in
  Util.set_msg "";
(*
Format.eprintf "==> %d@." (List.length l);
*)
  Format.eprintf "==== Sorting.@.";
  let a = Array.of_list l in
  let a =
    Array.map
      (fun p ->
         let (lon_min, lat_min, lon_max, lat_max) = ring_bbox p in
         let lat = truncate ((lat_min +. lat_max) /. 2.) +  90_0000000 in
         let lon = truncate ((lon_min +. lon_max) /. 2.) + 180_0000000 in
         (Geometry.hilbert_coordinate lat lon, p))
      a
  in
  Array.sort (fun (x, _) (x', _) -> compare x x') a;

  Format.eprintf "==== Writing B-tree.@.";
  let (add_polygon, close) =
    open_rtree ("coastline/rtrees/" ^ name) (*"small"*) ratio in
  Array.iter
    (fun (_, (lon, lat)) -> add_polygon (rescale_ring ratio (lon, lat)))
    a;
  close ()

(****)

let _ =
  if Array.length Sys.argv <> 2 then
    Util.fail (Format.sprintf "no file specified");

  let polys = load () in
  Gc.compact ();
  build_rtree "small" 50 polys;
  Gc.compact ();
  let build_rtree_simpl level name =
    let (ratio, polys') = simplify level polys in
    build_rtree name ratio polys';
  in
  build_rtree_simpl 8. "8";
  build_rtree_simpl 6. "6";
  build_rtree_simpl 4. "4";
  build_rtree_simpl 2. "2"

