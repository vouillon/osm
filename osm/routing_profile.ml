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

module type API = sig

  type str
  val s : string -> str
  val nil : str

  module Table : sig
    type 'a t
    val create : (string * 'a) list -> 'a t
    val create_set : string list -> unit t
    val mem : 'a t -> str -> bool
    val find : 'a t -> str -> 'a
  end

  module Tags : sig
    type t
    val mem : t -> str -> str -> bool
    val find : t -> str -> str
  end

  type direction = [`BIDIRECTIONAL | `ONEWAY | `OPPOSITE]
  type params =
    { mutable speed : float;
      mutable backward_speed : float;
      mutable duration : float;   (* < 0 if not defined *)
      mutable direction : direction }

end

module type PROFILE =
  functor (X : API) -> 
  sig
    open X
    val name : string
    val way : Tags.t -> params -> unit
  end

(****)

module Params = struct
  type direction = [`BIDIRECTIONAL | `ONEWAY | `OPPOSITE]
  type params =
    { mutable speed : float;
      mutable backward_speed : float;
      mutable duration : float;
      mutable direction : direction }
end
include Params

module Api (X : sig val dict : Dictionary.t end) = struct
  type str = int

  let string_tbl = Hashtbl.create 17
  let last_str = ref (-2)

  let nil = -1
  let s v =
    try
      Dictionary.find X.dict v
    with Not_found -> try
      Hashtbl.find string_tbl v
    with Not_found ->
      let i = !last_str in
      Hashtbl.add string_tbl v i;
      decr last_str;
      i

  module Table = struct
    type 'a t = (int, 'a) Hashtbl.t

    let create l =
      let h = Hashtbl.create (List.length l * 2) in
      List.iter (fun (k, v) -> Hashtbl.add h (s k) v) l;
      h

    let create_set l =
      let h = Hashtbl.create (List.length l * 2) in
      List.iter (fun k -> Hashtbl.add h (s k) ()) l;
      h

    let mem = Hashtbl.mem
    let find h k =
      Hashtbl.find h k
  end

  module Tags = struct
    type t = (int * int) list
    let mem (tags : t) k v =
      List.exists (fun (k', v') -> k = k' && v = v') tags
    let find tags k = try List.assq k tags with Not_found -> nil
  end

  type direction = [`BIDIRECTIONAL | `ONEWAY | `OPPOSITE]
  type params = Params.params =
    { mutable speed : float;
      mutable backward_speed : float;
      mutable duration : float;
      mutable direction : direction }

end

(****)

let profiles = ref []

module Register (F : PROFILE) = struct
  
  let g dict =
    let module X = Api(struct let dict = dict end) in
    let module M = F(X) in
    M.name,
    fun assoc ->
    let info =
      { speed = -1.; backward_speed = -1.; duration = -1.;
	direction = `BIDIRECTIONAL }
    in
    M.way assoc info;
    info

  let _ = profiles := g :: !profiles
end

let find dict name =
  let rec find_rec l =
    match l with
      [] ->
        failwith (Format.sprintf "profile '%s' not found" name)
    | p :: r ->
        let (n, f) = p dict in
        if n = name then f else find_rec r
  in
  find_rec !profiles
