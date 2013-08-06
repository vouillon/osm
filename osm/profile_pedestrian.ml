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

module F (X : Routing_profile.API) = struct
  open X

  let name = "pedestrian"

  let access_tag_blacklist =
    Table.create_set [(*"no";*) "private"]

  let speed =
    Table.create
      ["primary", 5. *. 0.8;
       "primary_link", 5. *. 0.8;
       "secondary", 5.;
       "secondary_link", 5.;
       "tertiary", 5.;
       "tertiary_link", 5.;
       "road", 5.;
       "residential", 5.;
       "cycleway", 5.;
       "unclassified", 5.;
       "service", 5.;
       "track", 5.;
       "path", 5.;
       "living_street", 5.;
       "pedestrian", 5. *. 1.2;
       "footway", 5. *. 1.2;
       "byway", 5.;
       "services", 5.;
       "bridleway", 5. *. 0.8;
       "steps", 4. *. 1.2]

  let _foot = s"foot"
  let _highway = s"highway"
  let _area = s"area"
  let _yes = s"yes"
  let _no = s"no"
  let _access = s"access"
  let _oneway = s"oneway"
  let _minus1 = s"-1"
  let _motorway = s"motorway"
  let _motorway_link = s"motorway_link"

  let false_table = Table.create_set ["no"; "0"; "false"]
  let true_table = Table.create_set ["yes"; "1"; "true"]

  exception Skip

  let way tags info =
    try
      let highway = Tags.find tags _highway in
      if
        Tags.mem tags _foot _no
          ||
        Table.mem access_tag_blacklist (Tags.find tags _access)
      then
        raise Skip;
      info.speed <- Table.find speed highway;
(*
      let oneway = Tags.find tags _oneway in
      info.direction <-
	if Table.mem false_table oneway then `BIDIRECTIONAL else
	if oneway = _minus1 then `OPPOSITE else
	if
	  Table.mem true_table oneway
	  || highway = _motorway || highway = _motorway_link
	  || (*XXX is round about*) false
	then
	  `ONEWAY
	else
	  `BIDIRECTIONAL
*)
    with Not_found | Skip ->
      ()

end

module M = Routing_profile.Register (F)
