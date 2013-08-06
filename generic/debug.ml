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

type t = { mutable state : bool; name : string; desc : string }

let debugs = ref []
let association = Hashtbl.create 11

let make s desc l =
  let d =
    try
      List.assoc s !debugs
    with Not_found ->
      let d = { state = false; name = s; desc = desc } in
      debugs := (s, d) :: !debugs;
      d
  in
  List.iter (fun s' -> Hashtbl.add association s' s) l;
  fun () -> d.state

let print () =
  Format.eprintf "Debug options:@.";
  List.iter
    (fun (_, d) -> Format.eprintf "    %s: %s@." d.name d.desc) !debugs;
  exit 1

let rec set s =
  if s = "help" || not (List.mem_assoc s !debugs) then
    print ()
  else
    try
      let d = List.assoc s !debugs in
      if not d.state then begin
        d.state <- true;
        List.iter set (Hashtbl.find_all association s)
      end
    with Not_found -> ()
