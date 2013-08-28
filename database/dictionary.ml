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
XXXX external merge

XXX eventually: add new strings
    ==> then we need another way to know the end of each bucket,
        as buckets may be relocated
*)

let next_power_of_two k =
  let n = ref 1 in
  while !n < k do n := !n lsl 1 done;
  !n

let log_base_2 k =
  let i = ref 0 in
  let n = ref 1 in
  while !n < k do incr i; n := !n lsl 1 done;
  !i

type int_array = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

module Array1 = Bigarray.Array1

external stable_sort : int_array -> int_array -> int -> unit = "sort_bigarrays"

(*XXX FIX: resize depending on available memory... *)
let sz = 155_000_000 (*50 * 1024 * 1024*)
let str_sz = 12 * sz (* Assuming an average of 12 byte per string *)

let rec output_varint ch v =
  if v < 128 then
    output_char ch (Char.chr v)
  else begin
    output_char ch (Char.chr ((v lor 0x80) land 0xff));
    output_varint ch (v lsr 7)
  end

let file dir nm = Column.file_in_database (Filename.concat dir nm)
(*
input: 
  - column with pointer to strings
  - string table
output:
  - new string table
  - mapping from old string index to new string index
*)
let build indices table target renaming =

  let t = Column.open_in indices in
  let strings =
    Mapped_file.array
      (Mapped_file.open_in (Column.file_in_database table) Bigarray.char) in
  let st = Column.stream t in

  let sz = Column.length t in
  let index = Array1.create Bigarray.int Bigarray.c_layout sz in
  let hash = Array1.create Bigarray.int Bigarray.c_layout sz in
  let offset = Array1.create Bigarray.int Bigarray.c_layout (sz + 1) in

  let rec read i j =
    let j' = Column.read st in
    if j' <> max_int then begin
      let l = j' - j in
      let s = Bytearray.sub strings j l in
      index.{i} <- i;
      hash.{i} <- Hashtbl.hash s;
      offset.{i} <- j;
      read (i + 1) j'
    end else begin
      offset.{i} <- j;
      i
    end
  in
  let len = read 0 (Column.read st) in
  
  stable_sort hash index len;
  
  let n = log_base_2 (Column.length t / 16) in
  let table_file =
    let table_name = file target "hashtbl" in
    Util.make_directories table_name;
    Mapped_file.open_out table_name (1 lsl n + 1) Mapped_file.int32
  in
  Mapped_file.resize table_file (1 lsl n + 1);
  let table = Mapped_file.output_array table_file in
  let width = 30 - n in

  let lst = ref [] in
  let h' = ref min_int in
  let last_id = ref 0 in
  let indices = Column.open_out (Column.named target "idx") in
  let old_ids = Column.open_out (Column.temp "string_old_id") in
  let new_ids = Column.open_out (Column.temp "string_new_id") in
  let ch = open_out (file target "strings") in
  let output () =
    List.iter
      (fun (s, id) ->
         Column.append indices (pos_out ch);
         output_varint ch (String.length s);
         output_string ch s;
         output_varint ch id)
      (List.rev !lst);
    lst := []
  in
  let last_hash = ref (-1) in
  for i = 0 to len - 1 do
    let j = index.{i} in
    let k = offset.{j} in
    let k' = offset.{j + 1} in
    let l = k' - k in
    let s = Bytearray.sub strings k l in
    let h = hash.{i} in
    if h <> !h' then begin
      h' := h;
      while h lsr width > !last_hash do
        incr last_hash;
        table.{!last_hash} <- Int32.of_int (pos_out ch);
(*
if !last_hash > 0 then Format.eprintf "%d/%d: %ld@." !last_hash (1 lsl n) (Int32.sub table.{!last_hash} table.{!last_hash - 1})
*)
      done;
      output ()
    end;
    let id =
      try
        let id = List.assoc s !lst in
        id
      with Not_found ->
        let id = !last_id in
        lst := (s, id) :: !lst;
        incr last_id;
        id
    in
    Column.append old_ids index.{i};
    Column.append new_ids id
  done;
  output ();
  close_out ch;
  Column.close_out indices;
  let old_ids = Column.freeze old_ids in
  let new_ids = Column.freeze new_ids in
  table.{1 lsl n} <- Int32.of_int (pos_out ch);
  Mapped_file.close_out table_file;
(* XXX
   - sorted mapping old id -> new id
   - change string ids everywhere
*)
  Sorting.permute ~o:renaming old_ids new_ids

type char_array =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let rec read_varint_rec (a : char_array) p v offs =
  let i = !p in
  let c = Char.code a.{i} in
  incr p;
  if c >= 0x80 then
    read_varint_rec a p (v lor ((c land 0x7f) lsl offs)) (offs + 7)
  else
    v lor (c lsl offs)

let read_varint a p = read_varint_rec a p 0 0

type int32_array =
  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

type t =
  { tbl : int32_array;
    str : char_array;
    idx : Column.t;
    width : int }

let load name =
  let tbl =
    Mapped_file.array
      (Mapped_file.open_in (file name "hashtbl") Bigarray.int32) in
  let n = log_base_2 (Bigarray.Array1.dim tbl - 1) in
  let width = 30 - n in
  let str =
    Mapped_file.array
      (Mapped_file.open_in (file name "strings") Bigarray.char) in
  { tbl = tbl; str = str; width = width;
    idx = Column.open_in (Column.named name "idx") }

let find t s =
  let h = Hashtbl.hash s in
  let p = ref (Int32.to_int t.tbl.{h lsr t.width}) in
  let p' = Int32.to_int t.tbl.{h lsr t.width + 1 + 5} in (*XXX???*)
  let rec find p p' =
    if !p = p' then raise Not_found;
    let l = read_varint t.str p in
    let s' = Bytearray.sub t.str !p l in
    p := !p + l;
    let i = read_varint t.str p in
    if s' <> s then
      find p p'
    else
      i
  in
  find p p'

let get t i =
  let p = ref (Column.get t.idx i) in
  let l = read_varint t.str p in
  Bytearray.sub t.str !p l
