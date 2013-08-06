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

let unique o i =
  let i = Column.stream i in
  let rec loop last =
    let v = Column.read i in
    if v <> max_int then begin
      if v <> last then Column.append o v;
      loop v
    end
  in
  let v = Column.read i in
  if v <> max_int then begin
    Column.append o v;
    loop v
  end;
  Column.freeze o

let unique = Column.with_spec unique "unique"

let group o1 o2 f i1 i2 =
  let i1 = Column.stream i1 in
  let i2 = Column.stream i2 in
  let rec loop v1 v2 =
    let v1' = Column.read i1 in
    let v2' = Column.read i2 in
    if v1' = v1 then
      loop v1 (f v2 v2')
    else begin
      Column.append o1 v1;
      Column.append o2 v2;
      if v1' <> max_int then loop v1' v2'
    end
  in
  let v1 = Column.read i1 in
  if v1 <> max_int then begin
    let v2 = Column.read i2 in
    loop v1 v2
  end;
  (Column.freeze o1, Column.freeze o2)

let group = Column.with_spec_2 group "key" "val"

let build_index o i =
  let i = Column.stream i in
  let rec loop j j' k =
    if j < j' then begin
      Column.append o k;
      loop (j + 1) j' k
    end else begin
      assert (j = j');
      let j' = Column.read i in
      if j' = max_int then
        Column.append o (k + 1)
      else
        loop j j' (k + 1)
    end
  in
  loop (-1) (-1) (-1);
  Column.freeze o

let build_index = Column.with_spec build_index "index"

let union o l =
  let rec loop i =
    let v = Column.read i in
    if v <> max_int then begin
      Column.append o v;
      loop i
    end
  in
  List.iter (fun i -> loop (Column.stream i)) l;
  Column.freeze o

let union = Column.with_spec union "union"

let map o f i =
  let i = Column.stream i in
  let rec loop () =
    let v = Column.read i in
    if v <> max_int then begin
      Column.append o (f v);
      loop ()
    end
  in
  loop ();
  Column.freeze o

let map = Column.with_spec map "res"

let map_2 o f i1 i2 =
  let i1 = Column.stream i1 in
  let i2 = Column.stream i2 in
  let rec loop () =
    let v1 = Column.read i1 in
    if v1 <> max_int then begin
      let v2 = Column.read i2 in
      Column.append o (f v1 v2);
      loop ()
    end
  in
  loop ();
  Column.freeze o

let map_2 = Column.with_spec map_2 "res"
