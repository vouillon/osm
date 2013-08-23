(*XXXX
* Layers!
* Too many R-trees...
*)

let _ = Column.set_database "/tmp/osm"

let leaf_size = 2048

(****)

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

(****)

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

let rec find graph odd_deg i j rem lst cont =
  let k = next_node graph j in
if debug then Format.eprintf "Going from %d to %d@." j k;
  if k = -1 then begin
    if i = j then begin
if debug then Format.eprintf "Loop through %d@." i;
      cont (i :: rem, lst)
    end else begin
if debug then Format.eprintf "Path %d --> %d@." i j;
      (snd odd_deg).(j) <- false;
      let k = next_odd odd_deg in
if debug then Format.eprintf "Continuing from %d@." k;
      if k = -1 then
        cont ([j], lst)
      else begin
        find graph odd_deg i k rem lst
          (fun (path, lst) -> cont ([j], path :: lst))
      end
    end
  end else begin
    find graph odd_deg i k rem lst (fun (path, lst) ->
if debug then Format.eprintf "--@.";
    find graph odd_deg j j (path) lst cont)
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
      find graph odd_deg i j [] lst (fun (path, lst) ->
      find_circuits graph odd_deg (i + 1) ((i :: path) :: lst))
  end

let rec find_unclosed_paths graph odd_deg lst =
  let i = next_odd odd_deg in
  if i <> -1 then begin
    find graph odd_deg i i [] lst (fun (path, lst) ->
    find_unclosed_paths graph odd_deg (path :: lst))
  end else
    lst

let find_paths graph =
  let graph = initialize graph in
  let odd_deg = odd_degree graph in
if debug then Format.eprintf "Finding unclosed paths@.";
  let lst = find_unclosed_paths graph odd_deg [] in
if debug then Format.eprintf "Finding circuits@.";
  find_circuits graph odd_deg 0 lst

(****)

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

(****)

let chunk_size = 10_000_000
let graph_size = chunk_size * 12 / 10

let categories_all =
  [ `Motorway; `Trunk; `Primary; `Secondary; `Tertiary;
    `Motorway_link; `Trunk_link; `Primary_link;
    `Secondary_link; `Tertiary_link;
    `Residential; `Unclassified; `Living_street; `Road; `Service;
    `Pedestrian; `Track; `Cycleway; `Bridleway; `Footway; `Path; `Steps;
    `River; `Canal; `Stream;
    `Runway; `Taxiway;
    `Rail; `Tram; `Subway ]

let categories_high =
  [ `River; `Canal;
    `Runway; `Rail; `Tertiary_link; `Tertiary;
    `Primary_link; `Primary; `Secondary_link; `Secondary;
    `Trunk_link; `Motorway_link; `Trunk; `Motorway ]

let categories_medium =
  [ `River;
    `Rail;
    `Primary_link; `Primary; `Secondary_link; `Secondary;
    `Trunk_link; `Motorway_link; `Trunk; `Motorway ]

let categories_low =
  [ `River;
    `Rail;
    `Primary_link; `Primary;
    `Trunk_link; `Motorway_link; `Trunk; `Motorway ]

let levels =
  [(categories_low, 6., "06");
   (categories_low, 8., "08");
   (categories_low, 10., "10");
   (categories_low, 11., "11");
   (categories_medium, 12., "12");
   (categories_high, 13., "13");
   (categories_all, 14., "14");
   (categories_all, 15., "15")]

let _ =
  let edge_col nm = Column.open_in (Column.named "linear/sorted/edge" nm) in
  let node1 = Column.stream (edge_col "1") in
  let node2 = Column.stream (edge_col "2") in
  let category = Column.stream (edge_col "category") in

  let node_col nm = Column.open_in (Column.named "linear/sorted/node" nm) in
  let latitude = Column.stream (node_col "lat") in
  let longitude = Column.stream (node_col "lon") in

  let len = Column.length (edge_col "1") in
  let n1 = Array.make chunk_size 0 in
  let n2 = Array.make chunk_size 0 in
  let cat = Array.make chunk_size 0 in

  let lat = Array.make graph_size 0 in
  let lon = Array.make graph_size 0 in

  let rtrees = ref [] in
  let open_rtree ratio name =
    let node_buf = String.create (16 * 1024) in
    let edge_buf = String.create (16 * 1024) in

    let (leaves, tree) = Rtree.open_out name in
    let ch =
      open_out (Column.file_in_database (Filename.concat name "ratio")) in
    Printf.fprintf ch "%d\n" ratio;
    close_out ch;
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
  Format.eprintf "%d %d %d %d@." st.node_prev_pos st.edge_prev_pos st.node_last st.edge_count;
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
Format.eprintf "miss: %d/%d@." !miss !num
    in
    rtrees := close :: !rtrees;
    fun n1 lat1 lon1 n2 lat2 lon2 cat lay ->
      st := write_edge !st n1 lat1 lon1 n2 lat2 lon2 cat lay
  in

  let levels =
    List.map
      (fun (cats, level, name) ->
         let scale = 256. /. 360. *. 2. ** level in
         let ratio = truncate (10_000_000. /. scale /. 2.) in
         (cats, ratio, open_rtree ratio ("linear/rtrees/" ^ name)))
      levels
  in

  for i = 0 to (len + chunk_size - 1) / chunk_size - 1 do
Format.eprintf "%d@." i;
    let l = min (len - i * chunk_size) chunk_size in
    for j = 0 to l - 1 do
      n1.(j) <- Column.read node1;
      n2.(j) <- Column.read node2;
      cat.(j) <- Column.read category;
      assert (n1.(j) <> max_int);
assert (n1.(j) <= n2.(j))
    done;
    Column.seek latitude n1.(0);
    Column.seek longitude n1.(0);
    for j = 0 to n1.(l - 1) - n1.(0) do
      lat.(j) <- Column.read latitude;
      lon.(j) <- Column.read longitude;
    done;
    let external_nodes = Hashtbl.create 1024 in
    let external_node_at_index = Hashtbl.create 1024 in
    let last_index = ref (n1.(l - 1) - n1.(0)) in
    let node_at_index i =
      if i <= n1.(l - 1) - n1.(0) then
        i + n1.(0)
      else
        Hashtbl.find external_node_at_index i
    in
    let node_index n =
      let i = n - n1.(0) in
      if i >= 0 && i <= n1.(l - 1) - n1.(0) then
        i
      else
        try
          Hashtbl.find external_nodes n
        with Not_found ->
          incr last_index;
          let i = !last_index in
(*
Format.eprintf "%d %d %d %d@." n n1.(0) n1.(l - 1) i;
*)
          if i >= graph_size then
            Util.fail "table too small!";
          Hashtbl.add external_nodes n i;
          Hashtbl.add external_node_at_index i n;
          Column.seek latitude n;
          Column.seek longitude n;
          lat.(i) <- Column.read latitude;
          lon.(i) <- Column.read longitude;
          i
    in
let n = ref 0 in
let m = ref 0 in
    let n1' = Array.make chunk_size 0 in
    let n2' = Array.make chunk_size 0 in
    let cat' = Array.make chunk_size 0 in

    List.iter
      (fun (cats, ratio, write_edge) ->
    let k = ref 0 in
    List.iter
      (fun c ->
Format.eprintf "----@.";
         let c = Feature.to_id c in
         let g = Array.make graph_size [] in
         for j = 0 to l - 1 do
           if cat.(j) = c then
             let k = node_index n1.(j) in
             g.(k) <- node_index n2.(j) :: g.(k);
incr n;
         done;
         let l = find_paths g in
m := !m + List.length l;
         Format.eprintf "%d@." (List.length l);
         List.iter
           (fun p ->
              let p = Array.of_list p in
              let len = Array.length p in
              let plat = Array.make len 0 in
              let plon = Array.make len 0 in
              for i = 0 to len - 1 do
                plat.(i) <- lat.(p.(i));
                plon.(i) <- lon.(p.(i))
              done;
              let idx = Douglas_peucker.perform_int_index ratio plat plon in
              Format.eprintf "%d %d@." (Array.length idx) len;
if Array.length idx = 2 then
  Format.eprintf "--> %f / %d@." (sqrt (float(plat.(0) - plat.(len - 1)) ** 2. +. float(plon.(0) - plon.(len - 1)) ** 2.)) ratio;
              for i = 0 to Array.length idx - 2 do
                let j1 = p.(idx.(i)) in
                let j2 = p.(idx.(i + 1)) in
                if j1 < j2 then begin
                  n1'.(!k) <- j1;
                  n2'.(!k) <- j2
                end else begin
                  n1'.(!k) <- j2;
                  n2'.(!k) <- j1
                end;
                cat'.(!k) <- c;
                incr k
              done)
           l)
      cats;
    let index = Array.make !k 0 in
    for i = 0 to !k - 1 do
      index.(i) <- i
    done;
    Array.sort
      (fun i1 i2 -> compare (node_at_index n1'.(i1)) (node_at_index n1'.(i2)))
      index;
    let delta = ratio / 2 - 1 in
    for i = 0 to !k - 1 do
      let i = index.(i) in
      let lat1 = (lat.(n1'.(i)) + delta) / ratio in
      let lon1 = (lon.(n1'.(i)) + delta) / ratio in
      let lat2 = (lat.(n2'.(i)) + delta) / ratio in
      let lon2 = (lon.(n2'.(i)) + delta) / ratio in
(*
Format.eprintf "AAA %d %d %x@." (node_at_index n1'.(i)) (node_at_index n2'.(i)) (Geometry.hilbert_coordinate (lat1 * ratio +  90_0000000) (lon1 * ratio +  180_0000000));
*)
      if lat1 <> lat2 || lon1 <> lon2 then
        write_edge
          (node_at_index n1'.(i)) lat1 lon1
          (node_at_index n2'.(i)) lat2 lon2
          cat'.(i) 0
    done;
Format.eprintf "===> %d / %d (%.2f / %.2f)@." !n chunk_size (float !n /. float !m) (float !n /. float !k))
      levels
  done;
  List.iter (fun close -> close ()) !rtrees

(*
- Load a chunk
- For each category:
  - build the corresponding graph, perform simplification and
    save remaining edges
*)
