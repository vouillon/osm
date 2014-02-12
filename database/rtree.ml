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

let min x y : int = if x < y then x else y
let max x y : int = if x > y then x else y
  
module Bbox = struct
  let max_int = 2147483647
  let min_int = -2147483648

  type t =
    { min_lat : int;
      max_lat : int;
      min_lon : int;
      max_lon : int }

  let empty =
    { min_lat = max_int;
      max_lat = min_int;
      min_lon = max_int;
      max_lon = min_int }

  let union b b' =
    { min_lat = min b.min_lat b'.min_lat;
      max_lat = max b.max_lat b'.max_lat;
      min_lon = min b.min_lon b'.min_lon;
      max_lon = max b.max_lon b'.max_lon }

  let overlaps b b' =
    b.min_lat <= b'.max_lat && b'.min_lat <= b.max_lat &&
    b.min_lon <= b'.max_lon && b'.min_lon <= b.max_lon

  let add_point b lat lon =
    { min_lat = min b.min_lat lat;
      max_lat = max b.max_lat lat;
      min_lon = min b.min_lon lon;
      max_lon = max b.max_lon lon }

  let contains_point b lat lon =
    b.min_lat <= lat && lat <= b.max_lat &&
    b.min_lon <= lon && lon <= b.max_lon

  let print f b =
    Format.fprintf f "@[<1>[%d@ %d@ %d@ %d]@]"
      b.min_lat b.max_lat b.min_lon b.max_lon

end

type level =
  { mutable level_bbox : Bbox.t;
    buffer : string;
    file : (int32, Bigarray.int32_elt) Mapped_file.output_stream;
    mutable idx : int } 

module IntMap =
  Map.Make (struct type t = int let compare (x : int) y = compare x y end)

type stream =
  { name : string;
    node_size : int;
    mutable levels : level IntMap.t;
    mutable max_level : int }


let open_out ?(node_size=1024) name =
  let name = Column.file_in_database name in
  let leaves = Filename.concat name "0" in
  Util.make_directories leaves;
  leaves,
  { name = name;
    node_size = node_size;
    levels = IntMap.empty;
    max_level = 0 }

let rec append_rec stream level bbox =
  let st =
    try
      IntMap.find level stream.levels
    with Not_found ->
      let file =
        Column.file_in_database (Format.sprintf "%s/%d" stream.name level) in
      Util.make_directories (Column.file_in_database file);
      let st =
        { level_bbox = Bbox.empty;
          buffer = String.create (16 * 1024);
          file =
	      Mapped_file.open_out file stream.node_size Mapped_file.int32;
          idx = 0 }
      in
      stream.levels <- IntMap.add level st stream.levels;
      stream.max_level <- max stream.max_level level;
      st
  in
  Mapped_file.resize st.file (4 * (st.idx + 1));
  let a = Mapped_file.output_array st.file in
  a.{4 * st.idx + 0} <- Int32.of_int bbox.Bbox.min_lat;
  a.{4 * st.idx + 1} <- Int32.of_int bbox.Bbox.max_lat;
  a.{4 * st.idx + 2} <- Int32.of_int bbox.Bbox.min_lon;
  a.{4 * st.idx + 3} <- Int32.of_int bbox.Bbox.max_lon;
  st.idx <- st.idx + 1;
  st.level_bbox <- Bbox.union st.level_bbox bbox;
  if st.idx mod (stream.node_size lsr 4) = 0 then begin
    append_rec stream (level + 1) st.level_bbox;
    st.level_bbox <- Bbox.empty
  end

let append stream bbox = append_rec stream 1 bbox

let close_out stream =
  let level = ref 1 in
  while !level <= stream.max_level do
    let st = IntMap.find !level stream.levels in
    if
      st.idx mod (stream.node_size lsr 4) > 0 &&
      st.idx > stream.node_size lsr 4
    then
      append_rec stream (!level + 1) st.level_bbox;
    Mapped_file.close_out st.file;
    incr level
  done

(****)

type t = int * (int32, Bigarray.int32_elt) Mapped_file.t array

let open_level dir i =
  let f = Format.sprintf "%s/%d" dir i in
  Mapped_file.open_in f Bigarray.int32


let rec open_levels dir i =
  let f = Format.sprintf "%s/%d" dir i in
  if Sys.file_exists f then
    Mapped_file.open_in f Bigarray.int32 :: open_levels dir (i + 1)
  else
    []

let open_in ?(node_size=1024) dir =
  let dir = Column.file_in_database dir in
  (dir ^ "/0",
   (node_size,
    Array.of_list (open_levels dir 1)))

let bounding_box (_, levels) =
  let a = Mapped_file.array levels.(Array.length levels - 1) in
  let bbox = ref Bbox.empty in
  for i = 0 to Bigarray.Array1.dim a / 4 - 1 do
    let bbox' =
      { Bbox.
	min_lat = Int32.to_int a.{4 * i};
        max_lat = Int32.to_int a.{4 * i + 1};
        min_lon = Int32.to_int a.{4 * i + 2};
        max_lon = Int32.to_int a.{4 * i + 3} }
    in
    bbox := Bbox.union !bbox bbox'
  done;
  !bbox

let find (node_size, levels) bbox f =
  let rec find_rec i j =
    if i = 0 then begin
      f j
    end else begin
      let i = i - 1 in
      let j = j * (node_size lsr 4) in
      let a = Mapped_file.array levels.(i) in
      for k = 0 to node_size lsr 4 - 1 do
        if 4 * (j + k) < Bigarray.Array1.dim a then begin
          let bbox' =
            { Bbox.
	      min_lat = Int32.to_int a.{4 * (j + k)};
              max_lat = Int32.to_int a.{4 * (j + k) + 1};
              min_lon = Int32.to_int a.{4 * (j + k) + 2};
              max_lon = Int32.to_int a.{4 * (j + k) + 3} }
          in
          if Bbox.overlaps bbox bbox' then find_rec i (j + k)
        end
      done
    end
  in
  find_rec (Array.length levels) 0

(****)

(*
let clamp x min max = if x < min then min else if x > max then max else x
let to_deg x = x * 50

let distance_to_box lat lon bbox =
  let lat' = clamp lat (to_deg bbox.min_lat) (to_deg bbox.max_lat) in
  let lon' = clamp lon (to_deg bbox.min_lon) (to_deg bbox.max_lon) in
  Geometry.distance lat lon lat' lon'
*)

let find_nearest_point (type feature)
    (node_size, levels) distance_to_box find_in_level =
  let module M = struct
    type elt =
        Group of int * int
      | Feature of feature

    module Queue =
      Pqueue.Make
        (struct
           type t = int * elt
           let compare ((x : int), _) (y, _) = compare x y
         end)
  end in
  let open M in
  fun lat lon ->
  let expand i j pqueue =
    if i = 0 then begin
      match find_in_level j lat lon with
        Some (d, f) -> Queue.add (d, Feature f) pqueue
      | None        -> pqueue
    end else begin
      let i = i - 1 in
      let j = j * (node_size lsr 4) in
      let a = Mapped_file.array levels.(i) in
      let pqueue = ref pqueue in
      for k = 0 to node_size lsr 4 - 1 do
        if 4 * (j + k) < Bigarray.Array1.dim a then begin
          let bbox =
            { Bbox.
              min_lat = Int32.to_int a.{4 * (j + k)};
              max_lat = Int32.to_int a.{4 * (j + k) + 1};
              min_lon = Int32.to_int a.{4 * (j + k) + 2};
              max_lon = Int32.to_int a.{4 * (j + k) + 3} }
          in
          pqueue :=
            Queue.add (distance_to_box bbox lat lon, Group (i, j + k)) !pqueue
        end
      done;
      !pqueue
    end
  in
  let rec loop queue =
    match Queue.find_min queue with
      (d, Group (i, j))        -> 
(*Format.eprintf "expand %d %d %d@." d i j;*)
        loop (expand i j (Queue.remove_min queue))
    | (d, Feature f) -> (d, f)
  in
  loop (expand (Array.length levels) 0 Queue.empty)
