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
- sort *two* arrays
- multiway merge
*)

type int_array = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

module Array1 = Bigarray.Array1

let blit (src : int_array) i (dst : int_array) j n =
  for k = 0 to n - 1 do
    Array1.unsafe_set dst (j + k) (Array1.unsafe_get src (i + k))
  done

let cutoff = 10;;
let stable_sort a a' l =
  let merge src1ofs src1len src2 src2' src2ofs src2len dst dst' dstofs =
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
    let rec loop i1 s1 i2 s2 d =
      if s1 <= s2 then begin
        Array1.unsafe_set dst d s1;
        Array1.unsafe_set dst' d (Array1.unsafe_get a' i1);
        let i1 = i1 + 1 in
        if i1 < src1r then
          loop i1 (Array1.unsafe_get a i1) i2 s2 (d + 1)
        else begin
          blit src2 i2 dst (d + 1) (src2r - i2);
          blit src2' i2 dst' (d + 1) (src2r - i2)
        end
      end else begin
        Array1.unsafe_set dst d s2;
        Array1.unsafe_set dst' d (Array1.unsafe_get src2' i2);
        let i2 = i2 + 1 in
        if i2 < src2r then
          loop i1 s1 i2 (Array1.unsafe_get src2 i2) (d + 1)
        else begin
          blit a i1 dst (d + 1) (src1r - i1);
          blit a' i1 dst' (d + 1) (src1r - i1)
        end
      end
    in
    loop src1ofs (Array1.unsafe_get a src1ofs)
         src2ofs (Array1.unsafe_get src2 src2ofs)
         dstofs
  in
  let isortto srcofs (dst : int_array) (dst' : int_array) dstofs len =
    for i = 0 to len - 1 do
      let e = (Array1.unsafe_get a (srcofs + i)) in
      let j = ref (dstofs + i - 1) in
      while (!j >= dstofs && Array1.unsafe_get dst !j > e) do
        Array1.unsafe_set dst (!j + 1) (Array1.unsafe_get dst !j);
        Array1.unsafe_set dst' (!j + 1) (Array1.unsafe_get dst' !j);
        decr j;
      done;
      Array1.unsafe_set dst (!j + 1) e;
      Array1.unsafe_set dst' (!j + 1) (Array1.unsafe_get a' (srcofs + i))
    done;
  in
  let rec sortto srcofs dst dst' dstofs len =
    if len <= cutoff then isortto srcofs dst dst' dstofs len else begin
      let l1 = len / 2 in
      let l2 = len - l1 in
      sortto (srcofs + l1) dst dst' (dstofs + l1) l2;
      sortto srcofs a a' (srcofs + l2) l1;
      merge (srcofs + l2) l1 dst dst' (dstofs + l1) l2 dst dst' dstofs;
    end;
  in
  if l <= cutoff then isortto 0 a a' 0 l else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Array1.create Bigarray.int Bigarray.c_layout l2 in
    let t' = Array1.create Bigarray.int Bigarray.c_layout l2 in
    sortto l1 t t' 0 l2;
    sortto 0 a a' l2 l1;
    merge l2 l1 t t' 0 l2 a a' 0;
  end

external stable_sort : int_array -> int_array -> int -> unit = "sort_bigarrays"

(****)

let next_power_of_two k =
  let n = ref 1 in
  while !n < k do n := !n lsl 1 done;
  !n

type stream =
  { mutable i : int;
    mutable l : int;
    a : int array;
    a' : int array;
    mutable eos : bool;
    mutable sos : bool;
    mutable pos : int;
    limit : int;
    t : Column.t;
    t' : Column.t }

let make_stream t t' pos len =
  { i = 0; l = 0;
    a = Array.make Column.block_size 0;
    a' = Array.make Column.block_size 0;
    eos = false; sos = true; pos = pos; limit = pos * Column.block_size + len;
    t = t; t' = t' }

let end_of_stream st = st.eos
let start_of_stream st = st.sos

let refill st =
  let rem = st.limit - st.pos * Column.block_size in
  if rem <= 0 then begin
    st.eos <- true;
    max_int
  end else begin
    st.sos <- false;
(*Format.eprintf "decoding at %d@." st.pos;*)
    Column.decode st.t st.pos 1 st.a 0;
    Column.decode st.t' st.pos 1 st.a' 0;
    st.i <- 0;
    st.l <- min Column.block_size rem;
    st.pos <- st.pos + 1;
    st.a.(0)
  end

let get st =
  let i = st.i in
  if i < st.l then st.a.(i) else refill st

let merge inputs out out' =
  let inputs =
    Array.map (fun (t, t', pos, len) -> make_stream t t' pos len) inputs in
  let k = Array.length inputs in
  let n = next_power_of_two k in
  let heap_id = Array.make (n - 1) (-1) in
  let heap_v = Array.make (n - 1) (-1) in
  let rec inorder_fill i j =
    if i >= n - 1 then
      j
    else begin
      let j = inorder_fill (2 * i + 1) j in
      heap_id.(i) <- j;
      heap_v.(i) <- if j < k then min_int else max_int;
      inorder_fill (2 * i + 2) (j + 1)
    end
  in
  ignore (inorder_fill 0 1);
(*
for i = 0 to n - 2 do
Format.eprintf "> %d: %d %d@." i heap_id.(i) heap_v.(i)
done;
*)
  let rec push i id v =
(*Format.eprintf "push %d %d %d@." i id v;*)
    if i > 0 then begin
      let i = (i - 1) lsr 1 in
      let v' = heap_v.(i) in
      if v < v' || (v = v' && id < heap_id.(i)) then
        push i id v
      else begin
        let id' = heap_id.(i) in
        heap_v.(i) <- v;
        heap_id.(i) <- id;
        push i id' v'
      end
    end else if
      v <> max_int ||
      not (id >= Array.length inputs || end_of_stream inputs.(id))
    then begin
      if
        v <> min_int ||
        not (id >= Array.length inputs || start_of_stream inputs.(id))
      then begin
        Column.append out v;
        let st = inputs.(id) in
        let i = st.i in
        Column.append out' st.a'.(i);
        st.i <- i + 1
      end;
      push (n - 1 + id) id (get inputs.(id))
    end
  in
  push (n - 1) 0 (get inputs.(0))
(*
;for i = 0 to n - 2 do
Format.eprintf "> %d: %d %d@." i heap_id.(i) heap_v.(i)
done
*)

(****)

let mem = (*2 * *) 1024 * 1024 * 1024 * 3
(*
let mem = 8192 * 8 * 30
*)

let perform output output' input input' =
  let max_size =
    mem * 2 / 3 (* sorting overhead *)
        / 2 (* two tables *)
        / 8 (* int size *) 
        / Column.block_size (* convert to blocks *)
  in
  let l = Column.length input in
  if Column.length input' <> l then
    failwith "sorting column of different sizes";
  let blocks = (l + Column.block_size - 1) / Column.block_size in
Format.eprintf "%d blocks@." blocks;
  let n = (blocks + max_size - 1) / max_size in
  let s = (blocks + n - 1) / n in

Format.eprintf "Need %d steps; %d blocks per step (max %d)@." n s max_size;

  let lst = ref [] in
  let sort_chunks output output' =
    let a =
      Array1.create Bigarray.int Bigarray.c_layout (s * Column.block_size) in
    let a' =
      Array1.create Bigarray.int Bigarray.c_layout (s * Column.block_size) in
    for i = 0 to n - 1 do
  let t = Unix.gettimeofday () in
      Column.decode_to_bigarray input (i * s) (min s (blocks - i * s)) a 0;
      Column.decode_to_bigarray input' (i * s) (min s (blocks - i * s)) a' 0;
  Format.eprintf "reading: %.2fs@." (Unix.gettimeofday () -. t);
  let t = Unix.gettimeofday () in
      let c = min (Array1.dim a) (l - i * Array1.dim a) in
      stable_sort a a' c;
  Format.eprintf "sorting: %.2fs@." (Unix.gettimeofday () -. t);
  let t = Unix.gettimeofday () in
      for i = 0 to c - 1 do
	Column.append output a.{i}
      done;
      for i = 0 to c - 1 do
	Column.append output' a'.{i}
      done;
  Format.eprintf "writing: %.2fs@." (Unix.gettimeofday () -. t);
      lst := (i * s, c) :: !lst
    done;
    (Column.freeze output, Column.freeze output')
  in

  if n = 1 then
    sort_chunks output output'
  else begin
    let temp = Column.open_out (Column.temp "sort") in
    let temp' = Column.open_out (Column.temp "sort") in
    let (temp, temp') = sort_chunks temp temp' in
let t = Unix.gettimeofday () in
    merge
      (Array.map (fun (i, c) -> (temp, temp', i, c))
	 (Array.of_list (List.rev !lst)))
      output output';
Format.eprintf "merging: %.2fs@." (Unix.gettimeofday () -. t);
    (Column.freeze output, Column.freeze output')
  end

let perform = Column.with_spec_2 perform "sorted" "sorted"

(****)

(*
let _ =
  let len = 123457 in

  let i1 = Column.open_out (file "i1") in
  let i2 = Column.open_out (file "i2") in

  let a = Array.init len (fun i -> i) in
  for i = len - 1 downto 1 do
    let j = Random.int i in
    let v = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- v
  done;

  for i = 0 to len - 1 do
    Column.append i1 (len - 1 - a.(i));
    Column.append i2 a.(i)
  done;
  let i1 = Column.freeze i1 in
  let i2 = Column.freeze i2 in

  let o1 = Column.open_out (file "o1") in
  let o2 = Column.open_out (file "o2") in
  perform i1 i2 o1 o2;
  let o1 = Column.freeze o1 in
  let o2 = Column.freeze o2 in
  for i = 0 to len - 1 do
    Format.eprintf "%d %d %b@."
      (Column.get o1 i) (Column.get o2 i)
      (Column.get o1 i = i &&  Column.get o2 i + i = len - 1)
  done
*)
