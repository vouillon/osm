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
XXX
Indices: node/way ==> assoc
         way ==> refs
*)

let _ = Column.set_database "/tmp/osm"
let source_column = Column.named "source"
let base_column = Column.named "base"

let _ =
  Format.eprintf "Computing index of relation members@.";
  let types = Column.open_in (base_column "relation_members/type") in
  let members = Column.open_in (source_column "relation_members/member") in
  let rename typ col =
    let index = Projection.filter types typ in
    let ids = Projection.project index members in
    let l = Column.length ids in
    let (ids, index) = Sorting.perform ids index in
    let col = Column.open_in (base_column col) in
    let (idx, index) =
      Join.perform
        (Column.identity (Column.length col)) ~def1:(-1) col
        index ids
    in
Format.eprintf "%d %d@." (Column.length idx) l;
    assert (Column.length idx = l);
    (index, idx)
  in
  let l = [rename 0 "node/id"; rename 1 "way/id"; rename 2 "relation/id"] in
  let index = Column_ops.union (List.map fst l) in
  let idx = Column_ops.union (List.map snd l) in
  ignore (Sorting.perform ~o2:(base_column "relation_members/member")
            index idx);

  Format.eprintf "Computing mapping from ways to nodes.@.";
  let way_refs = Column.open_in (source_column "way_refs/node_id") in
  let node_id = Column.open_in (base_column "node/id") in
  let l = Column.length way_refs in
  let (o, o') = Sorting.perform way_refs (Column.identity l) in
  let (node_idx, way_idx) =
    Join.perform (Column.identity (Column.length node_id)) node_id o' o in
  if Column.length node_idx <> l then
    Util.fail "a way refer to a non-existent node"; 
  ignore (Sorting.perform ~o2:(base_column "way_refs/node") way_idx node_idx);

  Format.eprintf "Building string directory@.";
  let renaming =
    Dictionary.build
      (Column.named "source" "string_idx") ("source/strings") "strings"
      (Column.temp "renaming")
  in

  Format.eprintf "Re-indexing strings@.";
  let map input renaming output =
    let l = Column.length input in
    let (o, o') = Sorting.perform input (Column.identity l) in
    let (o, o') =
      Join.perform renaming (Column.identity (Column.length renaming)) o' o in
    assert (Column.length o = l);
    Sorting.perform ~o2:output o' o
  in
  List.iter
    (fun col ->
       ignore
	 (map (Column.open_in (Column.named "source" col)) renaming
	    (Column.named "base" col)))
    ["node_assoc/key"; "node_assoc/val";
     "way_assoc/key"; "way_assoc/val";
     "relation_assoc/key"; "relation_assoc/val";
     "relation_members/role"]
