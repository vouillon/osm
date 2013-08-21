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

(*XXXX
 - Check required features
*)

let _ = Printexc.record_backtrace true

let osm_dir = "/tmp/osm"
let source = Column.named "source"
let base = Column.named "base"

(****)

let debug = Debug.make "parser" "print parser information" []

(****)

let i1 ch = Char.code (input_char ch)

let b4 ch =
  let c0 = i1 ch in
  let c1 = i1 ch in
  let c2 = i1 ch in
  let c3 = i1 ch in
  c0 lsl 24 + c1 lsl 16 + c2 lsl 8 + c3

let uncompress sz inbuf =
  let zs = Zlib.inflate_init true in
  let outbuf = String.create sz in
  let (finished, used_in, used_out) =
    Zlib.inflate zs inbuf 0 (String.length inbuf) outbuf 0 sz Zlib.Z_SYNC_FLUSH
  in
  assert finished;
  assert (used_out = sz);
  assert (used_in = String.length inbuf);
  outbuf

let (>>) x f = f x

(****)

let blob_header_spec =
  Protobuf.spec [(1, `String); (2, `Bytes); (3, `Int32)]

let blob_spec =
  Protobuf.spec [(1, `Bytes); (2, `Int32); (3, `Bytes)]

exception EOF

let parse_blob ch =
  let blob_header_len = try b4 ch with End_of_file -> raise EOF in

  let b = Protobuf.buffer_from_channel ch blob_header_len in
  let m = Protobuf.parse blob_header_spec b in
  let typ = Protobuf.string_field m 1 in
  if debug () then Format.eprintf "Blob type: %s@." typ;
  let datasize = Protobuf.int_field m 3 in

  let b = Protobuf.buffer_from_channel ch datasize in
  let m = Protobuf.parse blob_spec b in
  let raw_size = Protobuf.int_field m 2 in
  let zlib_data = Protobuf.string_field m 3 in


  let data = uncompress raw_size zlib_data in

  (typ, data)

(****)

let header_spec =
  Protobuf.spec [(4, `String); (5, `String)]

let parse_header data =
  let m = Protobuf.parse header_spec (Protobuf.buffer_from_string data) in
  let l = Protobuf.rep_field Protobuf.string m 4 in
  if debug () then begin
    Format.eprintf "Required features:@.";
    List.iter (fun s -> Format.eprintf "  %s@." s) l;
    let l = Protobuf.rep_field Protobuf.string m 5 in
    Format.eprintf "Optional features:@.";
    List.iter (fun s -> Format.eprintf "  %s@." s) l
  end

(****)

let string_table_spec = Protobuf.spec [(1, `String)]

type strings =
  { mutable s_offset : int;
    mutable s_count : int;
    s_table : out_channel;
    mutable s_pos : int;
    s_index : Column.output_stream }

let make_strings () =
  { s_offset = 0;
    s_count = 0;
    s_table = open_out (Column.file_in_database "source/strings");
    s_pos = 0;
    s_index = Column.open_out (source "string_idx") }

let close_strings strings =
  close_out strings.s_table;
  Column.append strings.s_index strings.s_pos;
  Column.close_out strings.s_index

let write_string strings s =
  Column.append strings.s_index strings.s_pos;
  output_string strings.s_table s;
  strings.s_pos <- strings.s_pos + String.length s

let parse_string_table strings string_table =
  let string_table =
    string_table
    >> Protobuf.parse string_table_spec
    >> (fun m -> Protobuf.rep_field Protobuf.string m 1)
  in
  List.iter (fun s -> write_string strings s) string_table;
  strings.s_offset <- strings.s_count;
  strings.s_count <- strings.s_offset + List.length string_table

(****)

type assocs =
  { mutable a_count : int;
    a_idx : Column.output_stream;
    a_key : Column.output_stream;
    a_val : Column.output_stream }

let make_assocs nm =
  { a_count = 0;
    a_idx = Column.open_out (base (nm ^ "_assoc/idx"));
    a_key = Column.open_out (source (nm ^ "_assoc/key"));
    a_val = Column.open_out (source (nm ^ "_assoc/val")) }

let close_assocs a =
  Column.close_out a.a_idx;
  Column.close_out a.a_key;
  Column.close_out a.a_val

let rec parse_assocs string_offset assoc_tables idx keys vals =
  if not (Protobuf.at_end_of_buffer keys) then begin
    let key = Protobuf.read_int keys in
    if key <> 0 then begin
      let value = Protobuf.read_int vals in
      let l = assoc_tables.a_count + 1 in
      assoc_tables.a_count <- l;
      Column.append assoc_tables.a_idx idx;
      Column.append assoc_tables.a_key (key + string_offset);
      Column.append assoc_tables.a_val (value + string_offset);
      parse_assocs string_offset assoc_tables idx keys vals
    end
  end

(****)

type nodes =
  { mutable n_count : int;
    n_id : Column.output_stream;
    n_lat : Column.output_stream;
    n_lon : Column.output_stream;
    n_assoc : assocs;
    mutable n_last_id : int }

let make_nodes () =
  { n_count = 0;
    n_id = Column.open_out (base "node/id");
    n_lat = Column.open_out (base "node/lat");
    n_lon = Column.open_out (base "node/lon");
    n_assoc = make_assocs "node";
    n_last_id = min_int }

let close_nodes nodes =
  Column.close_out nodes.n_id;
  Column.close_out nodes.n_lat;
  Column.close_out nodes.n_lon;
  close_assocs nodes.n_assoc

type coord_transform =
  { granularity : int;
    lat_offset : int;
    lon_offset : int }

let dense_node_spec =
  Protobuf.spec
    [(1, `Packed);  (* id *)
     (8, `Packed);  (* lat *)
     (9, `Packed);  (* lon *)
     (10, `Packed)] (* keys_vals *)

let parse_dense_nodes nodes string_offset transform b =
  if debug () then Format.eprintf "dense nodes@.";
  let m = Protobuf.parse dense_node_spec b in
  let ids = Protobuf.packed_field m 1 in
  let lats = Protobuf.packed_field m 8 in
  let lons = Protobuf.packed_field m 9 in
  let assocs = Protobuf.packed_field m 10 in
  let n = Protobuf.packed_count `Sint32 ids in
  let last_id = ref 0 in
  let last_lat = ref 0 in
  let last_lon = ref 0 in
  for i = 0 to n - 1 do
    let id = Protobuf.read_sint ids + !last_id in
if id <= nodes.n_last_id then Util.fail "nodes are not sorted";
nodes.n_last_id <- id;
    last_id := id;
    Column.append nodes.n_id id;
    let lat = Protobuf.read_sint lats + !last_lat in
    last_lat := lat;
    Column.append nodes.n_lat
      ((transform.lat_offset + transform.granularity * lat) / 100);
    let lon = Protobuf.read_sint lons + !last_lon in
    last_lon := lon;
    Column.append nodes.n_lon
      ((transform.lon_offset + transform.granularity * lon) / 100);
    parse_assocs string_offset nodes.n_assoc nodes.n_count assocs assocs;
    nodes.n_count <- nodes.n_count + 1
  done

(****)

type ways =
  { mutable w_count : int;
    w_id : Column.output_stream;
    w_refs_way : Column.output_stream;
    w_refs_node : Column.output_stream;
    w_assoc : assocs;
    mutable w_last_id : int }

let make_ways () =
  { w_count = 0;
    w_id = Column.open_out (base "way/id");
    w_refs_way = Column.open_out (base "way_refs/way");
    w_refs_node = Column.open_out (source "way_refs/node_id");
    w_assoc = make_assocs "way";
    w_last_id = min_int }

let close_ways ways =
  Column.close_out ways.w_id;
  Column.close_out ways.w_refs_way;
  Column.close_out ways.w_refs_node;
  close_assocs ways.w_assoc

let way_spec =
  Protobuf.spec
    [(1, `Int64);  (* id *)
     (2, `Packed); (* keys *)
     (3, `Packed); (* vals *)
     (8, `Packed)] (* refs *)

let parse_way ways string_offset way =
  let m = Protobuf.parse way_spec way in
  let id = Protobuf.int_field m 1 in
  let keys = Protobuf.packed_field m 2 in
  let vals = Protobuf.packed_field m 3 in
  let refs = Protobuf.packed_field m 8 in
if id <= ways.w_last_id then Util.fail "ways are not sorted";
ways.w_last_id <- id;
  Column.append ways.w_id id;
  parse_assocs string_offset ways.w_assoc ways.w_count keys vals;
  let last_ref = ref 0 in
  while not (Protobuf.at_end_of_buffer refs) do
    let rf = Protobuf.read_sint refs + !last_ref in
    last_ref := rf;
    Column.append ways.w_refs_way ways.w_count;
    Column.append ways.w_refs_node rf;
  done;
  ways.w_count <- ways.w_count + 1

let parse_ways ways string_offset way_list =
  List.iter (fun w -> parse_way ways string_offset w) way_list;
  if debug () then Format.eprintf "ways: %d@." ways.w_count

(****)

type relations =
  { mutable r_count : int;
    r_id : Column.output_stream;
    r_members_rel : Column.output_stream;
    r_members_memb : Column.output_stream;
    r_members_role : Column.output_stream;
    r_members_type : Column.output_stream;
    r_assoc : assocs;
    mutable r_last_id : int }

let make_relations () =
  { r_count = 0;
    r_id = Column.open_out (base "relation/id");
    r_members_rel = Column.open_out (base "relation_members/relation");
    r_members_memb = Column.open_out (source "relation_members/member");
    r_members_role = Column.open_out (source "relation_members/role");
    r_members_type = Column.open_out (base "relation_members/type");
    r_assoc = make_assocs "relation";
    r_last_id = min_int }

let close_relations relations =
  Column.close_out relations.r_id;
  Column.close_out relations.r_members_rel;
  Column.close_out relations.r_members_memb;
  Column.close_out relations.r_members_role;
  Column.close_out relations.r_members_type;
  close_assocs relations.r_assoc

let relation_spec =
  Protobuf.spec
    [(1, `Int64);   (* id *)
     (2, `Packed);  (* keys *)
     (3, `Packed);  (* vals *)
     (8, `Packed);  (* roles_sid *)
     (9, `Packed);  (* memids *)
     (10, `Packed)] (* types *)

let parse_relation relations string_offset relation =
  let m = Protobuf.parse relation_spec relation in
  let id = Protobuf.int_field m 1 in
  let keys = Protobuf.packed_field m 2 in
  let vals = Protobuf.packed_field m 3 in
  let roles_sid = Protobuf.packed_field m 8 in
  let memids = Protobuf.packed_field m 9 in
  let types = Protobuf.packed_field m 10 in
if id <= relations.r_last_id then Util.fail "relations are not sorted";
relations.r_last_id <- id;
  Column.append relations.r_id id;
  parse_assocs string_offset relations.r_assoc relations.r_count keys vals;
  let last_memb = ref 0 in
  while not (Protobuf.at_end_of_buffer memids) do
    let memb = Protobuf.read_sint memids + !last_memb in
    last_memb := memb;
    Column.append relations.r_members_rel relations.r_count;
    Column.append relations.r_members_memb memb;
    Column.append relations.r_members_role
      (Protobuf.read_int roles_sid + string_offset);
    Column.append relations.r_members_type (Protobuf.read_int types)
  done;
  relations.r_count <- relations.r_count + 1

let parse_relations relations string_offset rel_list =
  List.iter (fun r -> parse_relation relations string_offset r) rel_list;
  if debug () then Format.eprintf "relations: %d@." relations.r_count

(****)

type state =
  { strings : strings;
    nodes : nodes;
    ways : ways;
    relations : relations }

let make_state () =
  { strings = make_strings ();
    nodes = make_nodes ();
    ways = make_ways ();
    relations = make_relations () }

let close_state state =
  close_nodes state.nodes;
  close_ways state.ways;
  close_relations state.relations;
  close_strings state.strings

(****)

let primitive_group_spec =
  Protobuf.spec [(1, `Msg); (2, `Msg); (3, `Msg); (4, `Msg)]

let parse_primitive_group state string_offset transform b =
  let m = Protobuf.parse primitive_group_spec b in
  let nodes = Protobuf.rep_field Protobuf.raw m 1 in
  if nodes <> [] then begin
    Format.eprintf "nodes: %d@." (List.length nodes);
    assert false (*XXX*)
  end else begin
    match Protobuf.opt_field Protobuf.raw m 2 with
      Some dense_nodes ->
        parse_dense_nodes state.nodes string_offset transform dense_nodes
    | None ->
        let ways = Protobuf.rep_field Protobuf.raw m 3 in
        if ways <> [] then begin
          parse_ways state.ways string_offset ways
        end else begin
          let relations = Protobuf.rep_field Protobuf.raw m 4 in
          if relations <> [] then begin
            parse_relations state.relations string_offset relations
          end else
            assert false
        end
  end

(****)

let primitive_block_spec =
  Protobuf.spec
    [(1, `Msg);    (* stringtable *)
     (2, `Msg);    (* primitivegroup *)
     (17, `Int32); (* granularity *)
     (19, `Int64); (* lat_offset *)
     (20, `Int64)]  (* lon_offset *)

let parse_primitive_block state data =
  let m =
    Protobuf.parse primitive_block_spec (Protobuf.buffer_from_string data) in
  parse_string_table state.strings (Protobuf.raw_field m 1);
  let granularity = Protobuf.opt_field_with_default Protobuf.int 100 m 17 in
  let lat_offset = Protobuf.opt_field_with_default Protobuf.int 0 m 19 in
  let lon_offset = Protobuf.opt_field_with_default Protobuf.int 0 m 20 in
  let transform = { granularity; lat_offset; lon_offset } in
  List.iter
    (fun b -> parse_primitive_group state state.strings.s_offset transform b)
    (Protobuf.rep_field Protobuf.raw m 2)

(****)

let perc_str f =
  let s = "[                                       ]" in
  let p = truncate (f *. 38.99) + 1 in
  for i = 1 to p - 1 do s.[i] <- '=' done;
  s.[p] <- '>';
  for i = p + 1 to 39 do s.[i] <- ' ' done;
  s

let parse_primitive_block = Task.funct parse_primitive_block
let close_state = Task.funct (fun state () -> close_state state)
let fut = ref None 
let synchronize f =
  begin match !fut with
    Some f -> Task.wait f
  | None   -> ()
  end;
  fut := f ()

let load ch =
  let count = ref 0 in
  let len = float (in_channel_length ch) in
  let t = Unix.gettimeofday () in
  Column.set_database osm_dir;
  let state = Task.spawn (fun () -> make_state ()) in
  begin try
    while true do
      if !count mod 256 = 0 then begin
        let t' = Unix.gettimeofday () in
        let pos = float (pos_in ch) in
        Util.set_msg
          (Format.sprintf "parsing: %s %.0f%% (%.2f MiB/s) eta %.0fs"
             (perc_str (pos /. len))
             (pos /. len *. 100.) (pos /. 1024. /. 1024. /. (t' -. t))
             ((len -. pos) *. (t' -. t) /. pos))
      end;
      count := !count + 1;
      let (typ, data) = parse_blob ch in
      begin match typ with
        "OSMHeader" -> parse_header data
      | "OSMData"   -> synchronize (fun () ->
	                 Some (parse_primitive_block state data))
      | _           -> ()
      end;
(*Gc.full_major ();*)
(*Gc.print_stat stderr; flush stdout;*)
(*
if debug () then Format.eprintf "strings: %d@." state.strings.s_count;
*)
if debug () then Format.printf "----@."
    done
  with EOF -> () end;
  synchronize (fun () -> None);
  Task.wait (close_state state ());
  Task.kill state;
  Util.set_msg "";
  let t' = Unix.gettimeofday () in
  Format.eprintf "parsing: %.0fs (%.2f MiB/s)@."
    (t' -. t) (len /. 1024. /. 1024. /. (t' -. t))

let _ =
  let f = ref "" in
  let spec =
    Arg.align
      ["--debug", Arg.String Debug.set, "NAME Activate debug option NAME"]
  in
  let msg = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... FILE\n" in
  Arg.parse spec (fun p -> f := p) msg;
  if !f = "" then
    Util.fail (Format.sprintf "no file specified");
  if not (Sys.file_exists !f) then
    Util.fail (Format.sprintf "file '%s' not found" !f);
  load (open_in !f)
