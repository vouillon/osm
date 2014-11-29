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
* aeroway=runway can be a surface

* When a leaf bounding box is very large, perform clipping of the
  objects it contains, to avoid rendering artifacts (Cairo uses
  integers internally, +/- 8 millions, bug #20091)

* Redesign R-tree of linear features

  mostly constant:
    layer         4 bits
    bridge/tunnel 2 bit

  category    5 bits
  oneway/access 5 bits (car 2, bikes 2, pedestrian 1)
  ==> 2 bytes => 6 bits remaining for the number of ways

* Rendering performance:
  * do not render railway=rail;service=* at low zoom levels
  * multiple surface R-trees:
    R-tree of small surfaces; R-tree of other surfaces;
    R-tree with about the 1% largest surfaces; ...

* Rendering fixes
  ==> Highway with area=yes
      ==> do not draw them as ways as well
  ==> Outline of highway surfaces
  ==> Notre Dame de Paris and Sacré Coeur are missing!
      ==> use same algorithm as osm2pgsql to deal with multipolygon tags

* We could improve the rendering of tunnels, ... by classifying nodes:
  - do not use round linecap at tunnel extremities
  - draw additional circles when a path extremities do not agree
  - detect nodes with level mismatch

* One-way arrows (foot, bicycle, car), accessibility (foot, bicycle, car)
  ===> share between ways!
*)

let _ = Printexc.record_backtrace true
let _ = Column.set_database "/tmp/osm"

(****)

open Osm_display

let _ =
let width = 512 in
let height = 512 in

let st =
  { rect = { x = 0; y = 0; width = width; height = height };
    prev_rect = { x = 0; y = 0; width = width; height = height };
    level = 17.; prev_level = 17.; active = true; timeout = None;
    surface = make_surface ();
    marker1 = None; marker2 = None; path = [] } in

let lat = ref 48.850 in
let lon =  ref 2.350 in
begin
  let (ratio, _, tree) = Lazy.force large_surfaces in
  let bbox = Rtree.bounding_box tree in
  let c x = truncate (x *. 10_000_000. /. float ratio +. 0.5) in
  Format.eprintf "%a %d %d@." Bbox.print bbox (c !lat) (c !lon);
  if not (Bbox.contains_point bbox (c !lat) (c !lon)) then begin
    let c x = float x /. 10_000_000. *. float ratio in
    lat := c ((bbox.Bbox.min_lat + bbox.Bbox.max_lat) / 2);
    lon := c ((bbox.Bbox.min_lon + bbox.Bbox.max_lon) / 2)
  end
end;
let scale = compute_scale st in
st.rect <-
  { st.rect with
    x = truncate (!lon *. scale) - width / 2;
    y = - truncate (Osm_geometry.lat_to_y (!lat *. 10_000_000.) /. 10_000_000. *. scale) - height / 2 };
Format.eprintf "%d %d@." st.rect.x st.rect.y;

ignore (GMain.Main.init ());
let w = GWindow.window () in
ignore (w#connect#destroy GMain.quit);
let b = GPack.hbox ~packing:w#add () in
let table =
 GPack.table ~width ~height ~columns:1 ~rows:1 ~packing:(b#pack ~expand:true) () in
let display =
  GMisc.drawing_area
    ~packing:(table#attach ~left:0 ~top:0 ~fill:`BOTH ~expand:`BOTH) () in
display#misc#set_can_focus true;
(*
display#misc#set_double_buffered false;
*)

let queue_draw () = GtkBase.Widget.queue_draw display#as_widget in
let refresh () =
  invalidate_surface st.surface;
  queue_draw ()
in

let update_size () =
  let a = display#misc#allocation in
  st.rect <-
    { x = st.rect.x + (st.rect.width - a.Gtk.width) / 2;
      y = st.rect.y + (st.rect.height - a.Gtk.height) / 2;
      width = a.Gtk.width;
      height = a.Gtk.height }
in
(*
ignore (display#event#connect#configure
  (fun ev -> false));
ignore (display#event#connect#map
  (fun ev -> false));
  display#event#add [`STRUCTURE];
*)
ignore (display#event#connect#expose
  (fun ev ->
let t = Unix.gettimeofday () in
     let area = GdkEvent.Expose.area ev in
     let x = Gdk.Rectangle.x area in
     let y = Gdk.Rectangle.y area in
     let width = Gdk.Rectangle.width area in
     let height = Gdk.Rectangle.height area in
(*
     Format.eprintf "EXPOSE %d %d %d %d@." x y width height;
*)

     update_size ();
     let a = st.rect in

     let ctx = Cairo_gtk.create (display#misc#window) in
     let pm = st.surface in
     if st.active then begin
       grow_surface st.surface display a.width a.height;

       let dx = pm.valid_rect.x - st.rect.x in
       let dy = pm.valid_rect.y - st.rect.y in

       let p = get_surface pm in
       if
         (dx > 0 && pm.valid_rect.width + dx < a.width) ||
           (dy > 0 && pm.valid_rect.height + dy < a.height)
       then begin
         (* We would have to redraw four rectangles *)
         pm.valid_rect <- empty_rectangle
       end else if not (rectangle_is_empty pm.valid_rect) then begin
         let r = pm.valid_rect in
         if (dx <> 0 || dy <> 0) then begin
           let ctx = Cairo.create p in
           Cairo.set_source_surface ctx p (float dx) (float dy);
           Cairo.rectangle ctx
             (float dx) (float dy) (float r.width) (float r.height);
           Cairo.set_operator ctx Cairo.SOURCE;
           Cairo.fill ctx
         end;
         let offset p l d m = (* 0 <= p; 0 <= l; p + l <= m *)
           if p + d + l <= 0 then
             (0, 0)
           else if p + d < 0 then
             (0, l + p + d)
           else if p + d >= m then
             (m, 0)
           else if p + d + l > m then
             (p + d, m - p - d)
           else
             (p + d, l)
         in
         let (x, width) = offset 0 r.width dx pm.p_width in
         let (y, height) = offset 0 r.height dy pm.p_height in
         if height > 0 then begin
           if x > 0 then begin
             assert (x + width >= a.width);
             draw_map st p 0 y x height
           end else begin
             assert (x = 0);
             if a.width > width then
               draw_map st p width y (a.width - width) height
           end
         end;
         if y > 0 then begin
           assert (y + height >= a.height);
           draw_map st p 0 0 a.width y;
         end else begin
           assert (y = 0);
           if a.height > height then
             draw_map st p 0 height a.width (a.height - height)
         end;
         pm.valid_rect <- st.rect
       end;
       let r = pm.valid_rect in
       if x + width > r.width || y + height > r.height then begin
         draw_map st p 0 0 a.width a.height;
         pm.valid_rect <- st.rect
       end;
       Cairo.set_source_surface ctx p 0. 0.;
     end else begin
       Cairo.set_source_rgb ctx 0.8 0.8 0.8;
       Cairo.rectangle ctx (float x) (float y) (float width) (float height);
       Cairo.fill ctx;
       let p = get_surface pm in
       Cairo.set_source_surface ctx p 0. 0.;
       let coeff = 2. ** (st.prev_level -. st.level) in
       let matrix = Cairo.Matrix.init_identity () in
       Cairo.Matrix.translate matrix
         (-. float st.prev_rect.x) (-. float st.prev_rect.y);
       Cairo.Matrix.scale matrix coeff coeff;
       Cairo.Matrix.translate matrix (float st.rect.x) (float st.rect.y);
       Cairo.Pattern.set_matrix (Cairo.get_source ctx) matrix;
     end;
     Cairo.rectangle ctx (float x) (float y) (float width) (float height);
     Cairo.save ctx;
     (* Workaround for a Cairo bug (in ATI Catalyst drivers?): *)
     if st.active then Cairo.set_operator ctx Cairo.SOURCE;
     Cairo.fill_preserve ctx;
     Cairo.restore ctx;
     Cairo.clip ctx;
     draw_route st ctx;
Format.eprintf "Redraw: %f@." (Unix.gettimeofday () -. t);
     true));

let pos = ref None in
ignore (display#event#connect#button_press
  (fun ev ->
     display#misc#grab_focus ();
     if !pos = None then
       pos := Some (GdkEvent.Button.x ev, GdkEvent.Button.y ev,
                    GdkEvent.Button.button ev, false);
     false));
ignore (display#event#connect#button_release
  (fun ev ->
     let but' = GdkEvent.Button.button ev in
     begin match !pos with
       Some (x, y, but, move) when but = but' ->
         update_size ();
         if not move && but = 1 then begin
           st.marker1 <- find_marker st x y;
           update_route st;
           queue_draw ()
         end else if not move && but = 3 then begin
           st.marker2 <- find_marker st x y;
           update_route st;
           queue_draw ()
         end;
         pos := None
       | _ ->
         ()
     end;
     false));
ignore (display#event#connect#motion_notify
  (fun ev ->
(*Format.eprintf "MOVE@.";*)
     let (x', y') =
       if GdkEvent.Motion.is_hint ev then
         let (x', y') = display#misc#pointer in
         (float x', float y')
       else
         (GdkEvent.Motion.x ev, GdkEvent.Motion.y ev)
     in
     begin match !pos with
       Some (x, y, but, move)
         when but = 1
           && (move || abs_float (x -. x') > 3. || abs_float (y -. y') > 3.) ->
	 st.rect <-
           {st.rect with
            x = st.rect.x + truncate (x -. x');
	    y = st.rect.y + truncate (y -. y') };
         pos := Some (x', y', 1, true);
         queue_draw ()
     | _ ->
         ()
     end;
     false));
display#event#add
  [`BUTTON_PRESS; `BUTTON_RELEASE; `BUTTON1_MOTION; `POINTER_MOTION_HINT];

let perform_zoom ev delta =
  let x = truncate (GdkEvent.Scroll.x ev) in
  let y = truncate (GdkEvent.Scroll.y ev) in
  if (async_zoom || (delta > 0. && async_zoom_in)) && st.active then begin
    st.prev_level <- st.level;
    st.prev_rect <- st.rect;
    st.active <- false
  end;
  st.level <- st.level +. delta;
Format.eprintf "level: %f@." st.level;
  update_size ();
  st.rect <-
    { st.rect with
      x = truncate ((float (st.rect.x + x)) *. 2. ** delta) - x;
      y = truncate ((float (st.rect.y + y)) *. 2. ** delta) - y };
  begin match st.timeout with
    Some id -> Glib.Timeout.remove id
  | None    -> ()
  end;
  if not st.active then
    st.timeout <-
      Some (Glib.Timeout.add async_delay
              (fun () ->
                 st.timeout<- None; st.active <- true; refresh (); false));
  refresh ();
in
ignore (display#event#connect#scroll
          (fun ev ->
             match GdkEvent.Scroll.direction ev with
               `UP ->
                 perform_zoom ev 0.125;
                 true
             | `DOWN ->
                 perform_zoom ev (-0.125);
                 true
             | _ ->
                 false));
display#event#add [`SCROLL];

w#show ();
GMain.main ()
