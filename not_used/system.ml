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
madvise
information regarding available memory, cache size
*)

type 'a char_array =
  ('a, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

type advice =
  | MADV_NORMAL
  | MADV_RANDOM
  | MADV_SEQUENTIAL
  | MADV_WILLNEED
  | MADV_DONTNEED

external stub_madvise :
  'a char_array -> int -> int -> advice -> unit = "ml_madvise"

let madvise buf pos len advice =
  if pos < 0 || len < 0 || pos > Bigarray.Array1.dim buf - len then
    invalid_arg "Lwt_bytes.madvise"
  else
    stub_madvise buf pos len advice
