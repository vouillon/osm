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

type t = bytes

let make n = Bytes.make ((n + 7) lsr 3) '\000'

let set v i =
  let j = i lsr 3 in
  Bytes.set v j (Char.chr (Char.code (Bytes.get v j) lor (1 lsl (i land 7))))

let test v i =
  let j = i lsr 3 in
  Char.code (Bytes.get v j) land (1 lsl (i land 7)) <> 0
