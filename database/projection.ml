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

let filter output input v =
  let input = Column.stream input in
  let rec filter_rec i =
    let v' = Column.read input in
    if v' <> max_int then begin
      if v' = v then
        Column.append output i;
      filter_rec (i + 1)
    end
  in
  filter_rec 0;
  Column.freeze output

let filter = Column.with_spec filter "filtered"

let filter_pred output input p =
  let input = Column.stream input in
  let rec filter_rec i =
    let v = Column.read input in
    if v <> max_int then begin
      if p v then
        Column.append output i;
      filter_rec (i + 1)
    end
  in
  filter_rec 0;
  Column.freeze output

let filter_pred = Column.with_spec filter_pred "filtered"

let filter_pred_2 output input1 input2 p =
  let input1 = Column.stream input1 in
  let input2 = Column.stream input2 in
  let rec filter_rec i =
    let v1 = Column.read input1 in
    let v2 = Column.read input2 in
    if v1 <> max_int then begin
      assert (v2 <> max_int);
      if p v1 v2 then
        Column.append output i;
      filter_rec (i + 1)
    end else
      assert (v2 = max_int)
  in
  filter_rec 0;
  Column.freeze output

let filter_pred_2 = Column.with_spec filter_pred_2 "filtered"

let project output index input =
  let index = Column.stream index in
  let input = Column.stream input in
  let rec project_rec i =
    let i' = Column.read index in
    if i' <> max_int then begin
      assert (i <= i');
      if i' - i > 5 then
        Column.seek input i'
      else begin
        let i = ref i in
        while !i < i' do ignore (Column.read input); incr i done
      end;
      Column.append output (Column.read input);
      project_rec (i' + 1)
    end
  in
  project_rec 0;
  Column.freeze output

let project = Column.with_spec project "proj"

let inter output input1 input2 =
  let input1 = Column.stream input1 in
  let input2 = Column.stream input2 in
  let rec inter_rec v1 v2 =
    if v1 < v2 then
      inter_rec (Column.read input1) v2
    else if v2 < v1 then
      inter_rec v1 (Column.read input2)
    else if v1 <> max_int then begin
      Column.append output v1;
      inter_rec (Column.read input1) (Column.read input2)
    end
  in
  inter_rec (Column.read input1) (Column.read input2);
  Column.freeze output

let inter = Column.with_spec inter "inter"

let diff output input1 input2 =
  let input1 = Column.stream input1 in
  let input2 = Column.stream input2 in
  let rec diff_rec v1 v2 =
    if v1 < v2 then begin
      Column.append output v1;
      diff_rec (Column.read input1) v2
    end else if v1 = max_int then
      ()
    else if v2 < v1 then
      diff_rec v1 (Column.read input2)
    else
      diff_rec (Column.read input1) (Column.read input2)
  in
  diff_rec (Column.read input1) (Column.read input2);
  Column.freeze output

let diff = Column.with_spec diff "diff"
