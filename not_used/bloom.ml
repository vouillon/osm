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

let hash1 s = Hashtbl.seeded_hash 0 s
let hash2 s = Hashtbl.seeded_hash 1 s

type t =
  { m : int;
    k : int;
    v : Bitvect.t }

let make ~m ~k = { m = m; k = k; v = Bitvect.make m }

let add f s =
  let h1 = ref (hash1 s) in
  let h2 = ref (hash2 s) in
  let m = f.m in
  let v = f.v in
  Bitvect.set v (!h1 mod m);
  for i = 1 to f.k - 1 do
    h1 := (!h1 + !h2) mod m;
    h2 := (!h2 + i) mod m;
    Bitvect.set v (!h1 mod m)
  done
  
let mem f s =
  let h1 = ref (hash1 s) in
  let h2 = ref (hash2 s) in
  let m = f.m in
  let v = f.v in
  let res = ref (Bitvect.test v (!h1 mod m)) in
  for i = 1 to f.k - 1 do
    h1 := (!h1 + !h2) mod m;
    h2 := (!h2 + i) mod m;
    res := !res && Bitvect.test v (!h1 mod m);
    (*XXX Should stop as soon as res is false*)
  done;
  !res
