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

  let name = "car"

  let access_tag_blacklist =
    Table.create_set ["no"; "private"; "agricultural"; "forestry"]

  let speed =
    Table.create
    ["motorway",       90.;
     "motorway_link",  75.;
     "trunk",          85.;
     "trunk_link",     70.;
     "primary",        65.;
     "primary_link",   60.;
     "secondary",      55.;
     "secondary_link", 50.;
     "tertiary",       40.;
     "tertiary_link",  30.;
     "unclassified",   25.;
     "residential",    25.;
     "living_street",  10.;
     "service",        15.;
     "ferry",           5.;
     "shuttle_train",  10.;
     "default",        50.]

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
      if Tags.mem tags _area _yes then raise Skip;
      if Table.mem access_tag_blacklist (Tags.find tags _access) then raise Skip;
      info.speed <- Table.find speed highway;
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
    with Not_found | Skip ->
      ()

end

module M = Routing_profile.Register (F)
