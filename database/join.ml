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

let perform o1 o2 a1 ?def1 b1 a2 ?def2 b2 =
let t = Unix.gettimeofday () in
  let a1 = Column.stream a1 in
  let a2 = Column.stream a2 in
  let b1 = Column.stream b1 in
  let b2 = Column.stream b2 in
  let rec join_rec v1 v2 =
(*
Format.eprintf "%d %d@." v1 v2;
*)
    if
      v1 = max_int && v2 = max_int &&
      Column.beyond_end_of_stream b1 && Column.beyond_end_of_stream b2
    then
      (*XXX Could stop sooner when def1 and/or def2 is not set. *)
      ()
    else if v1 < v2 then begin
      begin match def2 with
        Some def2 ->
          let w1 = Column.read a1 in
          Column.append o1 w1;
          Column.append o2 def2;
      | None ->
          ignore (Column.read a1)
      end;
      join_rec (Column.read b1) v2
    end else if v1 > v2 then begin
(*
prerr_endline "XXXXXX";
*)
      begin match def1 with
        Some def1 ->
          let w2 = Column.read a2 in
          Column.append o1 def1;
          Column.append o2 w2;
      | None ->
          ignore (Column.read a2)
      end;
      join_rec v1 (Column.read b2)
    end else begin
      let v1' = Column.read b1 in
      let v2' = Column.read b2 in
(*
Format.eprintf "==> %d %d@." v1' v2';
*)
      if v1 < v1' then begin
        let w1 = Column.read a1 in
        Column.append o1 w1;
        Column.append o2 (Column.read a2);
        let v2' = ref v2' in
        while !v2' = v2 do
          Column.append o1 w1;
          Column.append o2 (Column.read a2);
          v2' := Column.read b2
        done;
        join_rec v1' !v2'
      end else if v2 < v2' then begin
        let w2 = Column.read a2 in
        Column.append o1 (Column.read a1);
        Column.append o2 w2;
        let v1' = ref v1' in
        while !v1' = v1 do
          Column.append o1 (Column.read a1);
          Column.append o2 w2;
          v1' := Column.read b1
        done;
        join_rec !v1' v2'
      end else begin
(*
Format.eprintf "%d %d@." v1 v2;
Format.eprintf "==> %d %d@." v1' v2';
*)
        assert (v1' = v1);
        assert (v2' = v2);
        let p = Column.position a2 in
        let inner_loop () =
(*
Format.eprintf "%d@." p;
*)
          Column.seek a2 p;
          Column.seek b2 (p + 1);
          let w1 = Column.read a1 in
          let v2' = ref v2 in
          while !v2' = v2 do
            Column.append o1 w1;
            Column.append o2 (Column.read a2);
            v2' := Column.read b2
(*
;Format.eprintf "==> %d@." !v2'
*)
          done;
          !v2'
        in
        ignore (inner_loop ());
        let v1' = ref v1' in
        let v2' = ref v2' in
        while !v1' = v1 do
          v2' := inner_loop ();
          v1' := Column.read b1
        done;
        join_rec !v1' !v2'
      end
    end
  in
(*
prerr_endline "XXX";
*)
  join_rec (Column.read b1) (Column.read b2);
Format.eprintf "join: %.2fs@." (Unix.gettimeofday () -. t);
  (Column.freeze o1, Column.freeze o2)

let perform = Column.with_spec_2 perform "join" "join"
