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

type typ =
  [`Int32 | `Sint32 | `Int64 | `Sint64 | `String | `Bytes | `Msg | `Packed]

let wire_type t =
  match t with
    `Int32 | `Sint32 | `Int64 | `Sint64 ->
      0
  | `String | `Bytes | `Msg | `Packed ->
      2

type spec = int array

let spec l =
  let s = List.fold_left (fun s (i, _) -> max s i) 0 l in
  let spec = Array.make (s + 1) (-1) in
  List.iter (fun (i, t) -> spec.(i) <- wire_type t) l;
  spec

(****)

type buf =
  { s : string;
    mutable i : int;
    l : int }

let buffer_from_substring s i l = { s = s; i = i; l = l}

let buffer_from_string s = { s = s; i = 0; l = String.length s}

let buffer_copy b = {s = b.s; i = b.i; l = b.l}

let empty_buffer = {s =""; i = 0; l = 0}

let input_string ch l =
  really_input_string ch l

let buffer_from_channel ch l =
  buffer_from_substring (really_input_string ch l) 0 l

let chunk buf l =
  let i = buf.i in
  buf.i <- i + l;
  { s = buf.s; i = i; l = i + l }

let rec varint_field_rec buf i =
  assert (i < buf.l);
  let c = Char.code buf.s.[i] in
  if c < 0x80 then chunk buf (i + 1 - buf.i) else
  varint_field_rec buf (i + 1)

let varint_field buf = varint_field_rec buf buf.i

(*XXX Should check for overflow... *)
let rec varint_rec buf v offs =
  let i = buf.i in
  assert (i < buf.l);
  let c = Char.code buf.s.[i] in
  buf.i <- i + 1;
  if c >= 0x80 then
    varint_rec buf (v lor ((c land 0x7f) lsl offs)) (offs + 7)
  else
    v lor (c lsl offs)

let varint buf = varint_rec buf 0 0

let rec varint_rec buf s i offs v =
 let c = Char.code (String.unsafe_get s i) in
 let v = v lor ((c land 0x7f) lsl offs) in
 let i = i + 1 in
 if c < 0x80 then begin
   buf.i <- i; v
 end else
   varint_rec buf s i (offs + 7) v

let varint buf =
  let s = buf.s in
  let i = buf.i in
  let c = Char.code (String.unsafe_get s i) in
  let v = c land 0x7f in
  let i = i + 1 in
  if c < 0x80 then begin
    buf.i <- i; v
  end else begin
    let c = Char.code (String.unsafe_get s i) in
    let v = v lor ((c land 0x7f) lsl 7) in
    let i = i + 1 in
    if c < 0x80 then begin
      buf.i <- i; v
    end else
      varint_rec buf s i 14 v
  end

let rec varint64_rec buf v offs =
  let i = buf.i in
  assert (i < buf.l);
  let c = Char.code buf.s.[i] in
  buf.i <- i + 1;
  if c >= 0x80 then
    varint64_rec buf
      (Int64.logor v
         (Int64.shift_left (Int64.of_int (c land 0x7f)) offs)) (offs + 7)
  else
    Int64.logor v (Int64.shift_left (Int64.of_int c)  offs)

let varint64 buf = varint64_rec buf 0L 0

let sint_of_int i = (i lsr 1) lxor (- (i land 1))

let sint64_of_int64 i =
  let i' = Int64.shift_right i 1 in
  if Int64.logand i 1L = 1L then Int64.sub (-1L) i' else i'

let int_of_sint i = if i > 0 then i lsl 1 else (- (i lsl 1) - 1)

let int_of_sint i = (i lsl 1) lxor (i asr 63)

(****)

let at_end_of_buffer b = b.i = b.l

let parse_value typ buf =
  match typ with
    0 -> varint_field buf
  | 1 -> chunk buf 8
  | 2 -> let l = varint buf in
         chunk buf l
  | 5 -> chunk buf 4
  | _ -> assert false

let parse_field spec buf res =
  let k = varint buf in
  let num = k lsr 3 in
  let typ = k land 0x7 in
  let v = parse_value typ buf in
  if num < Array.length spec then begin
    let typ' = spec.(num) in
    if typ' >= 0 then begin
      res.(num) <- v :: res.(num)
    end
  end

type msg = buf list array

let parse spec buf =
  let l = Array.length spec in
  let msg = Array.make l [] in
  while not (at_end_of_buffer buf) do
    parse_field spec buf msg
  done;
  msg

let req_field f msg i =
  match msg.(i) with
    v :: _ -> f v
  | []     -> assert false

let opt_field f msg i =
  match msg.(i) with
    v :: _ -> Some (f v)
  | []     -> None

let opt_field_with_default f d msg i =
  match msg.(i) with
    v :: _ -> f v
  | []     -> d

let rep_field f msg i = List.rev_map f msg.(i)

let raw s = s
let int = varint
let string {s = s; i = i; l = l} = String.sub s i (l - i)

let string_field msg i = req_field string msg i

let raw_field msg i = req_field raw msg i

let int_field msg i = req_field varint msg i

let int64_field msg i = req_field varint64 msg i

let packed_field msg i = opt_field_with_default raw empty_buffer msg i

(****)

let read_int = varint

let read_sint buf = sint_of_int (varint buf)

let read_sint64 buf = sint64_of_int64 (varint64 buf)

(****)

let rec unpack_rec typ f buf l =
  if at_end_of_buffer buf then
    List.rev l
  else
    unpack_rec typ f buf (f (parse_value typ buf) :: l)

let unpack typ f buf = unpack_rec (wire_type typ) f buf []

let packed_count typ buf =
  let typ = wire_type typ in
  let buf = buffer_copy buf in
  let n = ref 0 in
  while not (at_end_of_buffer buf) do
    incr n;
    ignore (parse_value typ buf)
  done;
  !n
