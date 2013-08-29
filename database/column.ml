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
File layout
===========
8 byte: magic number "COLUMN0\n"
8 byte: number of elements
8 byte: pointer to trailer
        compressed data
x * 8 bytes: trailer: chunk positions
*)

(****)

let magic = "COLUMN0\n"

(****)

let min x y : int = if x < y then x else y
let max x y : int = if x > y then x else y

let sint_of_int i = let i' = i lsr 1 in if i land 1 = 1 then (-i' - 1) else i'

let int_of_sint i = if i >= 0 then 2 * i else - 2 * i - 1

type char_array =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type int_array = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

let write_int_8 (a : char_array) p v =
  let v = ref v in
  for i = 0 to 7 do
    a.{p + i} <- !v land 0xff;
    v := !v lsr 8
  done

let write_int_2 (a : char_array) p v = a.{p} <- v; a.{p + 1} <- v lsr 8

let read_int_8 (a : char_array) p =
  let v = ref 0 in
  for i = 7 downto 0 do
    v := (!v lsl 8) lor a.{p + i}
  done;
  !v

let read_int_2 (a : char_array) p = (a.{p + 1} lsl 8) lor a.{p}

let rec write_varint (a : char_array) p v =
  if v < 128 then begin
    a.{p} <- v land 127;
    p + 1
  end else begin
    a.{p} <- (v land 127) + 128;
    write_varint a (p + 1) (v lsr 7)
  end

let write_signed_varint a p v = write_varint a p (int_of_sint v)

let rec read_varint_rec (a : char_array) p v offs =
  let i = !p in
  let c = a.{i} in
  incr p;
  if c >= 0x80 then
    read_varint_rec a p (v lor ((c land 0x7f) lsl offs)) (offs + 7)
  else
    v lor (c lsl offs)

let read_varint a p = read_varint_rec a p 0 0

let read_signed_varint a p = sint_of_int (read_varint a p)

(****)

let block_size = 6 * 1024
let chunk_size = 64

let map_incr = 8 * 1024 * 1024
let chunk_count = block_size / chunk_size
let max_overhead = block_size * 10 + 2 * chunk_count

let _ =
  assert (10 * block_size - chunk_size < 65536);
  assert (block_size mod chunk_size = 0)

(****)

external decode_chunk : char_array -> int -> int array -> int -> int -> int
  = "decode_chunk_ml"

let decode_buffer a pos buf p =
  let head = pos in
  let start = pos + 2 * chunk_count in
  for i = 0 to chunk_count - 1 do
    let pos = ref (read_int_2 a (head + 2 * i) + start) in
    let j0 = p + i * chunk_size in
    pos := decode_chunk a !pos buf j0 chunk_size
(*
    let last = ref 0 in
    for j = j0 to j0 + chunk_size - 1 do
      let v = read_signed_varint a pos + !last in
      buf.(j) <- v;
      last := v
    done
*)
  done

external decode_chunk_bigarray
  : char_array -> int -> int_array -> int -> int -> int
  = "decode_chunk_bigarray_ml"


let decode_buffer_to_bigarray a pos (buf : int_array) p =
  let head = pos in
  let start = pos + 2 * chunk_count in
  for i = 0 to chunk_count - 1 do
    let pos = ref (read_int_2 a (head + 2 * i) + start) in
    let j0 = p + i * chunk_size in
    pos := decode_chunk_bigarray a !pos buf j0 chunk_size
(*
    let last = ref 0 in
    for j = j0 to j0 + chunk_size - 1 do
      let v = read_signed_varint a pos + !last in
      buf.{j} <- v;
      last := v
    done
*)
  done

(****)

let b = Array.make block_size 0

let check a pos buf =
  decode_buffer a pos b 0;
  for i = 0 to block_size - 1 do
(*Format.eprintf "%d %d %d@." i buf.(i) b.(i);*)
    assert (buf.(i) = b.(i))
  done

(****)

let database = ref ""

let set_database s = database := s

let file_in_database s =
  if not (Filename.is_relative s) then s else begin
    if !database = "" then failwith "Database directory has not been set!";
    Filename.concat !database s
  end

type spec = Temp of string | Named of string

let temp nm = Temp (Filename.concat "tmp" nm)
let named dir f = Named (Filename.concat dir f)

(****)

module Materialized = struct

  type t =
    { data :
        (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
      len : int;
      table : int array }

  let open_from_mapped_file f =
    let a = Mapped_file.array f in
(*
    System.madvise a 0 (Bigarray.Array1.dim a) System.MADV_SEQUENTIAL;
*)
    for i = 0 to 7 do
      if a.{i} <> Char.code magic.[i] then failwith "corrupted column"
    done;
    let len = read_int_8 a 8 in
    let trailer = read_int_8 a 16 in
    let block_count = (len + block_size - 1) / block_size in
    let table = Array.make block_count 0 in
    for i = 0 to block_count - 1 do
      table.(i) <- read_int_8 a (trailer + 8 * i)
  (*
  ;Format.eprintf "block %d: %d@." i table.(i);
  *)
    done;
    { data = a;
      len = len;
      table = table }

  let open_in nm =
    open_from_mapped_file (Mapped_file.open_in nm Bigarray.int8_unsigned)

  let length t = t.len

  let get t i =
    let a = t.data in
    let block = t.table.(i / block_size) in
  (*
  Format.eprintf "block: %d@." block;
  *)
    let i = i mod block_size in
    let offset = read_int_2 a (block + (i / chunk_size) * 2) in
  (*
  Format.eprintf "offset: %d@." offset;
  *)
    let pos = ref (block + 2 * chunk_count + offset) in
    let i = i mod chunk_size in
    let v = ref 0 in
    for j = 0 to i do
      v := read_signed_varint a pos + !v
    done;
    !v

  let get_sequence t i n =
    let a = t.data in
    let b = Array.make n 0 in
    let i' = i + n - 1 in
    let block = i / block_size in
    let block' = i' / block_size in
  (*
  Format.eprintf "blocks: %d %d@." block block';
  *)
    let i = i mod block_size in
    let i' = i' mod block_size in
    for j = block to block' do
      let p = if j = block then 0 else block_size * (j - block) - i in
      let i = if j = block then i else 0 in
      let i' = if j = block' then i' else block_size - 1 in
      let block_pos = t.table.(j) in
      let chunk = i / chunk_size in
      let chunk' = i' / chunk_size in
  (*
  Format.eprintf "chunks: %d %d@." chunk chunk';
  *)
      let i = i mod chunk_size in
      let i' = i' mod chunk_size in
      for k = chunk to chunk' do
        let p = p + if k = chunk then 0 else chunk_size * (k - chunk) - i in
        let i = if k = chunk then i else 0 in
        let i' = if k = chunk' then i' else chunk_size - 1 in
        let offset = read_int_2 a (block_pos + k * 2) in
        let pos = ref (block_pos + 2 * chunk_count + offset) in
  (*Format.eprintf "%d %d %d@." p i i';*)
        let v = ref 0 in
        for l = 0 to i - 1 do
          v := read_signed_varint a pos + !v
        done;
        for l = i to i' do
          v := read_signed_varint a pos + !v;
          b.(p + l - i) <- !v
        done
      done
    done;
    b

  let decode t i n a j =
    assert (Array.length a - j >= block_size * n);
    for k = 0 to n - 1 do
      decode_buffer t.data t.table.(i + k) a (j + k * block_size)
    done

  let decode_to_bigarray t i n a j =
    assert (Bigarray.Array1.dim a - j >= block_size * n);
    for k = 0 to n - 1 do
      decode_buffer_to_bigarray t.data t.table.(i + k) a (j + k * block_size)
    done

end

(****)

type t = Materialized of Materialized.t | Identity of int * int

let open_in spec =
  match spec with
    Temp _   -> invalid_arg "Column.open_in"
  | Named nm -> Materialized (Materialized.open_in (file_in_database nm))

let identity ?(offset = 0) len = Identity (offset, len)

let length t =
  match t with
    Materialized t    -> Materialized.length t
  | Identity (_, len) -> len

let get t i =
  match t with
    Materialized t       -> Materialized.get t i
  | Identity (offset, _) -> offset + i

let get_sequence t i n =
  match t with
    Materialized t ->
      Materialized.get_sequence t i n
  | Identity (offset, _) ->
      let a = Array.make n 0 in
      for j = 0 to n - 1 do a.(i + j) <- offset + j done;
      a

let decode t i n a j =
  match t with
    Materialized t ->
      Materialized.decode t i n a j
  | Identity (offset, _) ->
      assert (Array.length a - j >= block_size * n);
      let i = offset + i * block_size in
      for k = 0 to n * block_size - 1 do
        a.(j + k) <- i + k
      done

let decode_to_bigarray t i n a j =
  match t with
    Materialized t ->
      Materialized.decode_to_bigarray t i n a j
  | Identity (offset, _) ->
      assert (Bigarray.Array1.dim a - j >= block_size * n);
      let i = offset + i * block_size in
      for k = 0 to n * block_size - 1 do
        Bigarray.Array1.unsafe_set a (j + k) (i + k)
      done

(****)

module Output_stream_write = struct

  type output_stream =
    { fd : Unix.file_descr;
      mutable pos : int;
      write_buffer : string;
      mutable buf_pos : int;
      buffer : int array;
      mutable i : int;
      mutable table : int array;
      mutable n : int }

  let write_size = 64 * 1024

  let open_out ?temp nm =
    Util.make_directories nm;
    let fd =
      Unix.openfile nm [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
    if temp <> None then Unix.unlink nm;
    ignore (Unix.lseek fd 24 Unix.SEEK_SET);
    { fd = fd; write_buffer = String.create (write_size + max_overhead);
      buffer = Array.make block_size 0; table = [||];
      pos = 24; buf_pos = 0; i = 0; n = 0 }

  let record_block_start s =
    let len = Array.length s.table in
    s.n <- s.n + 1;
    if s.n > len then begin
      let l = max (2 * len) 1024 in
      let a = Array.make l 0 in
      Array.blit s.table 0 a 0 len;
      s.table <- a
    end;
    s.table.(s.n - 1) <- s.pos

  external encode_chunk : string -> int -> int array -> int -> int -> int
    = "encode_chunk_to_string_ml"

  let write_int_2 a p v =
    a.[p] <- Char.chr (v land 0xff); a.[p + 1] <- Char.chr ((v lsr 8) land 0xff)

  let write_int_8 a p v =
    let v = ref v in
    for i = 0 to 7 do
      a.[p + i] <- Char.chr (!v land 0xff);
      v := !v lsr 8
    done

  let rec really_write fd s pos len =
    if len > 0 then
      let n = Unix.write fd s pos len in
      really_write fd s (pos + n) (len - n)

  let write_buffer s =
    let len = min write_size s.buf_pos in
    really_write s.fd s.write_buffer 0 len;
    if len < s.buf_pos then
      String.blit s.write_buffer len s.write_buffer 0 (s.buf_pos - len);
    s.buf_pos <- s.buf_pos - len

  let flush_buffer s =
    record_block_start s;
    let a = s.write_buffer in
    let head = s.buf_pos in
    let start = head + 2 * chunk_count in
    let pos = ref start in
    for i = 0 to chunk_count - 1 do
      write_int_2 a (head + 2 * i) (!pos - start);
      let j0 = i * chunk_size in
      pos := encode_chunk a !pos s.buffer j0 chunk_size;
  (*
      let last = ref 0 in
      for j = j0 to j0 + chunk_size - 1 do
        let v = s.buffer.(j) in
        pos := write_signed_varint a !pos (v - !last);
        last := v
      done
  *)
    done;
    s.pos <- s.pos + !pos - s.buf_pos;
    s.buf_pos <- !pos;
    if !pos >= write_size then write_buffer s;
    s.i <- 0

  let append s v =
    let i = s.i in
    Array.unsafe_set s.buffer i v;
    let i = i + 1 in
    s.i <- i;
    if i = block_size then flush_buffer s

  let finish_output s =
    let i = s.i in
    if i > 0 then flush_buffer s;
    write_buffer s;
    assert (s.buf_pos = 0);
    let a = String.create (8 * s.n) in
    for j = 0 to s.n - 1 do
      write_int_8 a (8 * j) s.table.(j)
    done;
    assert (Unix.lseek s.fd 0 Unix.SEEK_CUR = s.pos);
    really_write s.fd a 0 (String.length a);
    let len = (s.n - 1) * block_size + (if i = 0 then block_size else i) in

    let a = magic ^ String.create 16 in
    write_int_8 a 8 len;
    write_int_8 a 16 s.pos;
    ignore (Unix.lseek s.fd 0 Unix.SEEK_SET);
    really_write s.fd a 0 (String.length a)

  let close_out s =
    finish_output s;
    Unix.close s.fd

  let freeze s =
    finish_output s;
    Materialized
      (Materialized.open_from_mapped_file
         (Mapped_file.open_in_fd s.fd Bigarray.int8_unsigned))

end

module Output_stream_mmap = struct

  type output_stream =
    { stream : (int, Bigarray.int8_unsigned_elt) Mapped_file.output_stream;
      mutable pos : int;
      buffer : int array;
      mutable i : int;
      mutable table : int array;
      mutable n : int }

  let open_out ?temp nm =
    Util.make_directories nm;
    let stream =
      Mapped_file.open_out nm ?temp map_incr Mapped_file.int8_unsigned in
    { stream = stream; buffer = Array.make block_size 0; table = [||];
      pos = 24; i = 0; n = 0 }

  let record_block_start s =
    let len = Array.length s.table in
    s.n <- s.n + 1;
    if s.n > len then begin
      let l = max (2 * len) 1024 in
      let a = Array.make l 0 in
      Array.blit s.table 0 a 0 len;
      s.table <- a
    end;
    s.table.(s.n - 1) <- s.pos

  external encode_chunk : char_array -> int -> int array -> int -> int -> int
    = "encode_chunk_ml"

  let flush_buffer s =
    record_block_start s;
    let pos = s.pos in
    Mapped_file.resize s.stream (pos + max_overhead);
    let a = Mapped_file.output_array s.stream in
    let head = pos in
    let start = s.pos + 2 * chunk_count in
    let pos = ref start in
    for i = 0 to chunk_count - 1 do
      write_int_2 a (head + 2 * i) (!pos - start);
      let j0 = i * chunk_size in
      pos := encode_chunk a !pos s.buffer j0 chunk_size;
  (*
      let last = ref 0 in
      for j = j0 to j0 + chunk_size - 1 do
        let v = s.buffer.(j) in
        pos := write_signed_varint a !pos (v - !last);
        last := v
      done
  *)
    done;
  (*check a s.pos s.buffer;*)
    s.pos <- !pos;
    s.i <- 0

  let append s v =
    let i = s.i in
    Array.unsafe_set s.buffer i v;
    let i = i + 1 in
    s.i <- i;
    if i = block_size then flush_buffer s

  let finish_output s =
    let i = s.i in
    if i > 0 then flush_buffer s;
    Mapped_file.resize s.stream (s.pos + 8 * s.n);
    let a = Mapped_file.output_array s.stream in
    write_int_8 a 16 s.pos;
    for j = 0 to s.n - 1 do
      write_int_8 a (s.pos + 8 * j) s.table.(j)
  (*
  ;Format.eprintf "block %d: %d@." i s.table.(i);
  *)
    done;
    let len = (s.n - 1) * block_size + (if i = 0 then block_size else i) in
    write_int_8 a 8 len;
    for i = 0 to 7 do
      a.{i} <- Char.code magic.[i]
    done

  let close_out s =
    finish_output s;
    Mapped_file.close_out s.stream

  let freeze s =
    finish_output s;
    Materialized
      (Materialized.open_from_mapped_file (Mapped_file.freeze s.stream))

end

include Output_stream_write

(****)

let next_power_of_two k =
  let n = ref 1 in
  while !n < k do n := !n lsl 1 done;
  !n

type input_stream =
  { mutable i : int;
    mutable l : int;
    a : int array;
    mutable eos : bool;
    mutable pos : int;
    limit : int;
    t : t }

let seek st p =
(*Format.eprintf "!%d %d@." p st.limit;*)
  assert (p >= 0 && p <= st.limit);
  let pos' = p / block_size in
  let i' = p mod block_size in
  st.eos <- false;
  if st.pos = pos' + 1 && st.l > 0 then begin
    st.i <- i'
  end else begin
    st.pos <- pos';
    decode st.t st.pos 1 st.a 0;
    st.i <- i';
    let rem = st.limit - st.pos * block_size in
    st.l <- min block_size rem;
    st.pos <- st.pos + 1
  end

let stream ?(first = 0) ?last t =
  let len = length t in
  let last =
    match last with
      Some last -> assert (last <= len); last
    | None      -> len
  in
  assert (0 <= first && first <= last);
  let st =
    { i = 0; l = 0;
      a = Array.make block_size 0;
      eos = false; pos = 0; limit = last;
      t = t }
  in
  if len > 0 then seek st first;
  st

let position st = (st.pos - 1) * block_size + st.i

let refill st =
  let rem = st.limit - st.pos * block_size in
  if rem <= 0 then begin
    st.eos <- true;
    max_int
  end else begin
    decode st.t st.pos 1 st.a 0;
    st.i <- 1;
    st.l <- min block_size rem;
    st.pos <- st.pos + 1;
    st.a.(0)
  end

let read st =
  let i = st.i in
  if i < st.l then begin
(*
Format.eprintf "AAA %d:%d -> %d@." st.pos i st.a.(i);
*)
    st.i <- i + 1;
    Array.unsafe_get st.a i
  end else
    refill st

let at_end_of_stream st = st.i = st.l && st.limit - st.pos * block_size <= 0
let beyond_end_of_stream st = st.eos

(****)

let open_out spec =
  match spec with
    Temp nm  -> open_out ~temp:() (file_in_database nm)
  | Named nm -> open_out (file_in_database nm)

let with_spec f nm = let def = temp nm in fun ?(o=def) -> f (open_out o)

let with_spec_2 f nm1 nm2 =
  let def1 = temp nm1 in let def2 = temp nm2 in
  fun ?(o1=def1) ?(o2=def2) -> f (open_out o1) (open_out o2)

(****)

let is_column nm =
  let ch = Pervasives.open_in (file_in_database nm) in
  let s = String.make 8 ' ' in
  ignore (input ch s 0 8);
  close_in ch;
  s = magic
