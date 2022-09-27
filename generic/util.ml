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

let can_enable_msgs = Unix.isatty Unix.stderr

let enable_msgs = ref can_enable_msgs

let enable_messages b = if can_enable_msgs then enable_msgs := b

let cur_msg = ref ""

let hide_msg () =
  if !cur_msg <> "" then begin
    prerr_string "\r";
    prerr_string (String.make (String.length !cur_msg) ' ');
    prerr_string "\r";
    flush stderr;
  end

let show_msg () =
  if !cur_msg <> "" then begin prerr_string !cur_msg; flush stderr end

let set_msg s =
  if !enable_msgs && s <> !cur_msg then begin
    hide_msg (); cur_msg := s; show_msg ()
  end

(****)

let progress_bar f =
  let s = Bytes.of_string "[                                       ]" in
  let p = truncate (f *. 38.99) + 1 in
  for i = 1 to p - 1 do Bytes.set s i '=' done;
  Bytes.set s p '>';
  for i = p + 1 to 39 do Bytes.set s i ' ' done;
  Bytes.to_string s

(****)

let warn_loc = ref None

let set_warning_location s = warn_loc := Some s
let reset_warning_location () = warn_loc := None

let print_warning s =
  hide_msg ();
  begin match !warn_loc with
    None    -> Format.eprintf "Warning: %s@." s
  | Some s' -> Format.eprintf "Warning (%s): %s@." s' s
  end;
  show_msg ()

let fail s =
  hide_msg ();
  Format.eprintf "Failure: %s@." s;
  exit 1

(****)

let title s = Format.printf "%s@.%s@." s (String.make (String.length s) '=')

(****)

module Timer = struct
  type t = float
  let start () = Unix.gettimeofday ()
  let stop t = Unix.gettimeofday () -. t
end

module Utimer = struct
  type t = float
  let start () = (Unix.times ()).Unix.tms_utime
  let stop t = start () -. t
end

module IntSet =
  Set.Make (struct type t = int let compare x (y : int) = compare x y end)
module StringSet = Set.Make (String)

(****)

module ListTbl = struct
  type ('a, 'b) t = ('a, 'b list ref) Hashtbl.t

  let create : int -> ('a, 'b) t = Hashtbl.create

  let add h n p =
    try
      let l = Hashtbl.find h n in
      l := p :: !l
    with Not_found ->
      Hashtbl.add h n (ref [p])

  let find h n = try !(Hashtbl.find h n) with Not_found -> []

  let mem = Hashtbl.mem

  let iter f h = Hashtbl.iter (fun k l -> f k !l) h

  let copy h =
    let h' = Hashtbl.create (2 * Hashtbl.length h) in
    Hashtbl.iter (fun k l -> Hashtbl.add h' k (ref !l)) h;
    h'

  let remove h n f =
    try
      let l = Hashtbl.find h n in
      l := List.filter (fun p -> not (f p)) !l;
      if !l = [] then Hashtbl.remove h n
    with Not_found ->
      ()
end

module StringTbl =
  Hashtbl.Make
    (struct
       type t = string
       let hash = Hashtbl.hash
       let equal (s : string) s' = s = s'
     end)

(****)

let print_list pr sep ch l =
  match l with
    []     -> ()
  | x :: r -> pr ch x; List.iter (fun x -> Format.fprintf ch "%s%a" sep pr x) r

(****)

let rec make_directories f =
  let f = Filename.dirname f in
  if not (Sys.file_exists f) then begin
    try
      Unix.mkdir f (0o755)
    with Unix.Unix_error (Unix.ENOENT, _, _) ->
      make_directories f;
      Unix.mkdir f (0o755)
  end

(****)

let bytes_extend s n c =
  let s' = Bytes.make n c in
  Bytes.blit s 0 s' 0 (Bytes.length s);
  s'

let array_extend a n v =
  let a' = Array.make n v in
  Array.blit a 0 a' 0 (Array.length a);
  a'

(****)

module BitVect = struct
  type t = bytes
  let make n v = Bytes.make n (if v then 'T' else 'F')
  let test vect x = Bytes.get vect x <> 'F'
  let set vect x = Bytes.set vect x 'T'
  let clear vect x = Bytes.set vect x 'F'
  let copy = Bytes.copy
  let extend vect n v = bytes_extend vect n (if v then 'T' else 'F')
  let sub = Bytes.sub
  let implies vect1 vect2 =
    let l = Bytes.length vect1 in
    assert (Bytes.length vect2 = l);
    let rec implies_rec vect1 vect2 i l =
      i = l ||
      ((Bytes.get vect1 i <> 'T' || Bytes.get vect2 i = 'T') &&
       implies_rec vect1 vect2 (i + 1) l)
    in
    implies_rec vect1 vect2 0 l
  let lnot vect =
    let l = Bytes.length vect in
    let vect' = Bytes.make l 'F' in
    for i = 0 to l - 1 do
      Bytes.set vect' i (if Bytes.get vect i = 'F' then 'T' else 'F')
    done;
    vect'
  let (land) vect1 vect2 =
    let l = Bytes.length vect1 in
    assert (Bytes.length vect2 = l);
    let vect = Bytes.make l 'F' in
    for i = 0 to l - 1 do
      Bytes.set vect i (if Bytes.get vect1 i = 'F' || Bytes.get vect2 i = 'F' then 'F' else 'T')
    done;
    vect
  let (lor) vect1 vect2 =
    let l = Bytes.length vect1 in
    assert (Bytes.length vect2 = l);
    let vect = Bytes.make l 'F' in
    for i = 0 to l - 1 do
      Bytes.set vect i (if Bytes.get vect1 i = 'F' && Bytes.get vect2 i = 'F' then 'F' else 'T')
    done;
    vect
end

(****)

let sort_and_uniq compare l =
  let rec uniq v l =
    match l with
      []      -> [v]
    | v' :: r -> if compare v v' = 0 then uniq v r else v :: uniq v' r
  in
  match List.sort compare l with
    []     -> []
  | v :: r -> uniq v r

let compare_pair compare1 compare2 (a1, a2) (b1, b2) =
  let c = compare1 a1 b1 in
  if c = 0 then compare2 a2 b2 else c

let rec compare_list compare l1 l2 =
  match l1, l2 with
    [], [] ->
      0
  | [], _ ->
      -1
  | _, [] ->
      1
  | v1 :: r1, v2 :: r2 ->
      let c = compare v1 v2 in if c = 0 then compare_list compare r1 r2 else c

let group compare l =
  match l with
    [] ->
      []
  | (a, b) :: r ->
      let rec group_rec a bl l =
        match l with
          [] ->
            [(a, List.rev bl)]
        | (a', b) :: r ->
            if compare a a' = 0 then
              group_rec a (b :: bl) r
            else
              (a, List.rev bl) :: group_rec a' [b] r
      in
      group_rec a [b] r

(****)

module Union_find = struct

type 'a link =
    Link of 'a t
  | Value of 'a

and 'a t =
  { mutable state : 'a link }

let rec repr t =
  match t.state with
    Link t' ->
      let r = repr t' in
      t.state <- Link r;
      r
  | Value _ ->
      t

let rec get t =
  match (repr t).state with
    Link _  -> assert false
  | Value v -> v

let merge t t' f =
  let t = repr t in
  let t' = repr t' in
  if t != t' then begin
    t.state <- Value (f (get t) (get t'));
    t'.state <- Link t
  end

let elt v = { state = Value v }

end

(****)

(*
let (>>) v f = f v

let leading_whitespaces_re = Str.regexp "^[ \t\n]+"
let trailing_whitespaces_re = Str.regexp "[ \t\n]+$"

let trim s =
  s >> Str.replace_first leading_whitespaces_re ""
    >> Str.replace_first trailing_whitespaces_re ""
*)
