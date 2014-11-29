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

let _ = Printexc.record_backtrace true
let _ = Column.set_database "/tmp/osm"

(****)

(*
let surface =
  Cairo.SVG.create ~fname:"/tmp/bar.svg"  ~width:360. ~height:180.
let ctx = Cairo.create surface
let _ = Cairo.translate ctx 180. 90.

let show = false
*)

let _ =
  if Array.length Sys.argv <> 2 then
    Util.fail (Format.sprintf "no file specified");

  let polys = Osm_coastline.load () in
  Gc.compact ();
  Osm_coastline.build_rtree "small" 50 polys;
  Gc.compact ();
  let build_rtree_simpl level name =
    let (ratio, polys') = Osm_coastline.simplify level polys in
    Osm_coastline.build_rtree name ratio polys';
  in
  build_rtree_simpl 8. "8";
  build_rtree_simpl 6. "6";
  build_rtree_simpl 4. "4";
  build_rtree_simpl 2. "2"
