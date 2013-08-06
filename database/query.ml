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

let (>>) x f = f x

let dir = "/tmp/osm"

let not_table = ["strings"; "strings_2"; "string_hashtbl"]

let column_stats s =
  let f = Filename.concat dir s in
  let ch = open_in f in
  let l = in_channel_length ch in
  close_in ch;
  let c = Column.open_in (Column.named "" s) in
  let n = Column.length c in
  Format.printf "%s: %d element (%.2f byte/element)@."
    s n (float l /. float n)

let rec iter_files d =
  let a = Sys.readdir (Filename.concat dir d) in
  Array.sort compare a;
  Array.iter
    (fun nm ->
       let f = Filename.concat d nm in
       if Sys.is_directory (Filename.concat dir f) then iter_files f else
       if Column.is_column f then column_stats f)
    a

let _ =
  let format = ref `NUM in
  let cols = ref [] in
  let idx = ref None in
  let spec =
    Arg.align
      ["--debug", Arg.String Debug.set, "NAME Activate debug option NAME";
       "--string", Arg.Unit (fun () -> format := `STR),
       " Output string value of following columns";
       "--num", Arg.Unit (fun () -> format := `NUM),
       " Output numeric value of following columns";
       "--index", Arg.Int (fun i -> idx := Some i),
       "I Output only line I"]
  in
  let msg = "Usage: " ^ Sys.argv.(0) ^ " [OPTION]... [COLUMN]...\n" in
  Arg.parse spec (fun col -> cols := (col, !format) :: !cols) msg;
  Column.set_database "/tmp/osm";
  let dirs =
    List.filter
      (fun (f, _) -> Sys.is_directory (Filename.concat dir f)) !cols in
  if !cols = [] then
    iter_files ""
  else if dirs <> [] then
    List.iter (fun (d, _) -> iter_files d) dirs
  else begin
    let d = lazy (Dictionary.load "strings") in
    let tables =
      List.map
        (fun (nm, kind) -> (Column.open_in (Column.named "" nm), kind))
        (List.rev !cols)
    in
    let streams =
      List.map (fun (tbl, kind) -> (Column.stream tbl, kind)) tables in
    let print i vals =
      print_int i; print_char ':';
      List.iter
        (fun (v, k) ->
           match k with
             `NUM -> print_char ' '; print_int v
           | `STR -> print_string " '";
      	       print_string (Dictionary.get (Lazy.force d) v);
      	       print_string "'")
        vals;
      print_char '\n'
    in
    let rec read i =
      let vals =
        List.map (fun (stream, kind) -> (Column.read stream, kind)) streams
      in
      if not (List.exists (fun (v, _) -> v = max_int) vals) then begin
        print i vals;
	read (i + 1)
      end
    in
    match !idx with
      None ->
        read 0
    | Some i ->
        print i (List.map (fun (tbl, kind) -> (Column.get tbl i, kind)) tables)
  end
