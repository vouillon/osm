(*

- dashed lines
- underground features: draw them three times
  1. draw in buffer
  2. redraw on stencil
  3. draw to window using stencil and buffer

==================================

Performance tip: no single call to glDrawElements() comprises more
than approximately one fifth of the total number of vertices in the
frame.

Parameters:
- line width
- line color

- cap style
- join style

- dash (???)
- miter_limit

====

TODO
====
* Very sharp angles???
  ==> do as if these were ROUND or SQUARE caps
* Miter limit
* Closed paths

* Dashed lines

* Interleave the vertex information in a single buffer
  (aligned to 4 bytes);
  store small values (bytes) when possible: side, color, ...
  ==> combine side and edge bits
      side = abs(x) - 3;
      edge = sign(x);
* We could save one cycle in the fragment shader by perforing *BLUR+0.5
  in the vertex shader
* Optimize buffer streaming
  http://www.opengl.org/wiki/Buffer_Object_Streaming
* Deal correctly with more than 65536 vertices
  (maybe using the extension is enough...)

* 3D lines?

====

Performance
===========
https://developer.apple.com/library/ios/documentation/3DDrawing/Conceptual/OpenGLES_ProgrammingGuide/TechniquesforWorkingwithVertexData/TechniquesforWorkingwithVertexData.html

http://blogs.unity3d.com/wp-content/uploads/2011/08/FastMobileShaders_siggraph2011.pdf

Map rendering
=============

Rendering Interactive Maps on Mobile Devices Using Graphics Hardware

Efficient Map, Road, Terrain, Text and POI Rendering on OpenGL-ES


Polygon triangulation
=====================
Robust Incremental Polygon Triangulation for Fast Surface Rendering

http://lin-ear-th-inking.blogspot.fr/2011/04/polygon-triangulation-via-ear-clipping.html

==> if the polygon is simple enough, we triangulate;
    render polygons front to back when possible, several polygons at once
    (can we do it all front to back???)
*)

let animate = true

let handle_drag element move stop click =
  let fuzz = 4 in
  element##onmousedown <- Dom_html.handler
    (fun ev ->
       let x0 = ev##clientX and y0 = ev##clientY in
(*
debug_msg (Format.sprintf "Mouse down %d %d" x0 y0);
*)
       let started = ref false in
       let c1 =
         Dom_html.addEventListener Dom_html.document Dom_html.Event.mousemove
           (Dom_html.handler
              (fun ev ->
                 let x = ev##clientX and y = ev##clientY in
(*
debug_msg (Format.sprintf "Mouse move %d %d %d %d" x0 y0 x y);
*)
                 if
                   not !started && (abs (x - x0) > fuzz || abs (y - y0) > fuzz)
                 then begin
                   started := true;
                   element##style##cursor <- Js.string "move"
                 end;
                 if !started then move x0 y0 x y;
                 Dom_html.stopPropagation ev;
                 Js._true))
           Js._true
       in
       let c2 = ref Js.null in
       c2 := Js.some
         (Dom_html.addEventListener Dom_html.document Dom_html.Event.mouseup
            (Dom_html.handler
               (fun ev ->
(*
debug_msg (Format.sprintf "Mouse up %d %d %d %d" x0 y0 ev##clientX ev##clientY);
*)
                  Dom_html.removeEventListener c1;
                  Js.Opt.iter !c2 Dom_html.removeEventListener;
                  if !started then begin
                    element##style##cursor <- Js.string "";
                    stop ev##clientX ev##clientY
                  end else
                    click ev##clientX ev##clientY;
                  Js._true))
            Js._true);
       Js._true)

let handle_touch_events element move stop cancel click =
  let fuzz = 4 in
  ignore (Dom_html.addEventListener element Dom_html.Event.touchstart
    (Dom_html.handler (fun ev ->
       Js.Optdef.iter (ev##changedTouches##item(0)) (fun touch ->
       let id = touch##identifier in
       let x0 = touch##clientX and y0 = touch##clientY in
(*
debug_msg (Format.sprintf "Touch start %d %d" x0 y0);
*)
       let started = ref false in
       let c1 =
         Dom_html.addEventListener Dom_html.document Dom_html.Event.touchmove
           (Dom_html.handler
              (fun ev ->
                 for i = 0 to ev##changedTouches##length - 1 do
                   Js.Optdef.iter (ev##changedTouches##item(i)) (fun touch ->
                   if touch##identifier = id then begin
                     let x = touch##clientX and y = touch##clientY in
(*
  debug_msg (Format.sprintf "Touch move %d %d %d %d" x0 y0 x y);
*)
                     if
                       not !started &&
                       (abs (x - x0) > fuzz || abs (y - y0) > fuzz)
                     then begin
                       started := true;
                       element##style##cursor <- Js.string "move"
                     end;
                     if !started then move x0 y0 x y
                   end)
                 done;
                 Dom_html.stopPropagation ev;
                 Js._false))
           Js._true
       in
       let c2 = ref Js.null in
       let c3 = ref Js.null in
       c2 := Js.some
         (Dom_html.addEventListener Dom_html.document Dom_html.Event.touchend
            (Dom_html.handler
               (fun ev ->
                  for i = 0 to ev##changedTouches##length - 1 do
                    Js.Optdef.iter (ev##changedTouches##item(i)) (fun touch ->
                    if touch##identifier = id then begin
                      let x = touch##clientX and y = touch##clientY in
(*
debug_msg (Format.sprintf "Touch end %d %d %d %d" x0 y0 x y);
*)
                      Dom_html.removeEventListener c1;
                      Js.Opt.iter !c2 Dom_html.removeEventListener;
                      Js.Opt.iter !c3 Dom_html.removeEventListener;
                      if !started then begin
                        element##style##cursor <- Js.string "";
                        stop x y
                      end else
                        click x y
                    end)
                  done;
                  Js._true))
            Js._true);
       c3 := Js.some
         (Dom_html.addEventListener Dom_html.document Dom_html.Event.touchend
            (Dom_html.handler
               (fun ev ->
                  for i = 0 to ev##changedTouches##length - 1 do
                    Js.Optdef.iter (ev##changedTouches##item(i)) (fun touch ->
                    if touch##identifier = id then begin
                      let x = touch##clientX and y = touch##clientY in
(*
debug_msg (Format.sprintf "Touch cancel %d %d %d %d" x0 y0 x y);
*)
                      Dom_html.removeEventListener c1;
                      Js.Opt.iter !c2 Dom_html.removeEventListener;
                      Js.Opt.iter !c3 Dom_html.removeEventListener;
                      if !started then element##style##cursor <- Js.string "";
                      cancel x y
                    end)
                  done;
                  Js._false))
            Js._true));
       Js._false))
    Js._true)

let debug_widget =
  let d = Dom_html.document in
  let w = Dom_html.createDiv d in
  w##style##position <- Js.string "absolute";
  w##style##top <- Js.string "20px";
  w##style##left <- Js.string "20px";
  w##style##lineHeight <- Js.string "0.9em";
  w##style##color <- Js.string "red";
  let p = Dom_html.createP d in
  Dom.appendChild w p;
  (w, p)

let debug_msg s =
  let d = Dom_html.document in
  Dom.appendChild (d##body) (fst debug_widget);
  (snd debug_widget)##innerHTML <- Js.string s

let now () = Js.to_float ((jsnew Js.date_now ())##getTime())

let back_to_front = true

let create_canvas w h =
  let d = Dom_html.window##document in
  let c = Dom_html.createCanvas d in
  c##width <- w;
  c##height <- h;
  c

let makeBuffer (gl : WebGL.renderingContext Js.t) values =
  let vbuffer = gl##createBuffer() in
  gl##bindBuffer(gl##_ARRAY_BUFFER_, vbuffer);
  let values = jsnew Typed_array.float32Array_fromArray(Js.array values) in
  gl##bufferData(gl##_ARRAY_BUFFER_, values, gl##_STATIC_DRAW_);
  vbuffer

let makeUbyteBuffer (gl : WebGL.renderingContext Js.t) values =
  let vbuffer = gl##createBuffer() in
  gl##bindBuffer(gl##_ARRAY_BUFFER_, vbuffer);
  let values = jsnew Typed_array.uint8Array_fromArray(Js.array values) in
  gl##bufferData(gl##_ARRAY_BUFFER_, values, gl##_STATIC_DRAW_);
  vbuffer

let makeByteBuffer (gl : WebGL.renderingContext Js.t) values =
  let vbuffer = gl##createBuffer() in
  gl##bindBuffer(gl##_ARRAY_BUFFER_, vbuffer);
  let values = jsnew Typed_array.int8Array_fromArray(Js.array values) in
  gl##bufferData(gl##_ARRAY_BUFFER_, values, gl##_STATIC_DRAW_);
  vbuffer

let setAttrib (gl:WebGL.renderingContext Js.t) program name size
      ?(offset=0) ?(kind=gl##_FLOAT) ?(normalize=false) buffer =
(*Firebug.console##log(Js.string (Format.sprintf "%s: %d" name (Array.length values)));*)
  let attr = gl##getAttribLocation(program, Js.string name) in
  gl##enableVertexAttribArray(attr);
  gl##bindBuffer(gl##_ARRAY_BUFFER_, buffer);
  gl##vertexAttribPointer(attr, size, kind, Js.bool normalize, 0, offset)

let create_program
      (gl : WebGL.renderingContext Js.t) vertex_shader fragment_shader =
  let vs = gl##createShader(gl##_VERTEX_SHADER_) in
  gl##shaderSource(vs, Js.string vertex_shader);
  gl##compileShader(vs);

  let fs = gl##createShader(gl##_FRAGMENT_SHADER_) in
  gl##shaderSource(fs, Js.string fragment_shader);
  gl##compileShader(fs);

  let program = gl##createProgram() in
  gl##attachShader(program, vs);
  gl##attachShader(program, fs);
  gl##linkProgram(program);

  if not (Js.to_bool gl##getShaderParameter(vs, gl##_COMPILE_STATUS_)) then
    Firebug.console##log(gl##getShaderInfoLog(vs));

  if not (Js.to_bool gl##getShaderParameter(fs, gl##_COMPILE_STATUS_)) then
    Firebug.console##log(gl##getShaderInfoLog(fs));

  if not (Js.to_bool gl##getProgramParameter(program, gl##_LINK_STATUS_)) then
    Firebug.console##log(gl##getProgramInfoLog(program));
  gl##useProgram(program);
  program

let getContext (c : Dom_html.canvasElement Js.t) : WebGL.renderingContext Js.t Js.opt =
  let c = Js.Unsafe.coerce c in
  c##getContext (Js.string "experimental-webgl",
                 Js.Unsafe.(obj [|"antialias", inject Js._true;
                                  "stencil", inject Js._true|]))

let poly_vertex_shader = "
  precision highp float;
  uniform vec2 scale;
  uniform mat3 transform;
  attribute vec2 p;

  void main () {
    gl_Position = vec4((transform * vec3(p, 1)).xy * scale + vec2(-1., -1.), 0., 1.);
  }
"

let poly_fragment_shader = "
  precision mediump float;

  uniform vec4 color;

  void main () {
    gl_FragColor = color;
  }
"

let line_vertex_shader = "
  precision highp float;

  uniform vec2 scale;
  uniform mat3 transform;
  uniform float zoom;

  attribute vec2 p0, p1, p2, p3; // the two extremities of the line
  attribute lowp vec4 color;  // line color
/*
  attribute float w;          // line width
  attribute float style1, style2; // line style
  attribute float edge;   // source or target
  attribute float side;   // which side of the line
*/
  attribute vec4 data1, data2;

  varying mediump vec4 dx;
  varying mediump vec2 dy_hw;
  varying lowp vec4 col;

  void main () {
    float w = data1.x;
    float style1 = data1.y;
    float style2 = data2.y;
    float side = data1.z;
    float edge = data1.w;

    vec2 q1 = (transform * vec3(p1, 1)).xy;
    vec2 q2 = (transform * vec3(p2, 1)).xy;
    float hw = w * zoom * (0.5 / 8.);

    vec2 dq = q2 - q1;
    vec2 t1 = normalize(dq);
    vec2 n1 = vec2(-t1.y, t1.x);

    float border = hw + 1.;

    float dy = (hw + 1.) * side;

    vec4 d1 = vec4(t1, - dot(t1, q1), 0);
    vec4 d2 = vec4(0, 0, length(dq), 0) - d1;
    vec4 d3 = d1;
    vec4 d4 = d2;

    if (style1 > 0.) {
      // Line cap
      q1 = q1 + (border * side) * n1 - border * t1;
      d1.z = (style1 == 3.) ? (d1.z+hw): d1.z;
      d3.z = (style1 == 1.) ? (d3.z-hw): d3.z;
    } else {
      // Line join
      vec2 q0 = (transform * vec3(p0, 1)).xy;
      vec2 t0 = normalize(q1 - q0);
      vec2 n0 = vec2(-t0.y, t0.x);
      vec2 n = (n0 + n1) / (1. + dot(n0, n1));

d3 = (style1 == -3.) ? vec4(-normalize(n), dot(normalize(n), q1), 0)  :  vec4(0.);

      q1 = q1 + (border * side) * n;
      d1 = (style1 == -2.) ? d1 : vec4(0.);
    }

    if (style2 > 0.) {
      // Line cap
      q2 = q2 + (border * side) * n1 + border * t1;
      d2.z = (style2 == 3.) ? (d2.z+hw): d2.z;
      d4.z = (style2 == 1.) ? (d4.z-hw): d4.z;
    } else {
      // Line join
      vec2 q3 = (transform * vec3(p3, 1)).xy;
      vec2 t2 = normalize(q3 - q2);
      vec2 n2 = vec2(-t2.y, t2.x);
      vec2 n = (n1 + n2) / (1. + dot(n1, n2));
  // |n| = sqrt(2/(1. + dot(n1, n2)))
  // miter if miter_limit^2*(1. + dot(n1, n2)) < 2

d4 = (style2 == -3.) ? vec4(-normalize(n), + dot(normalize(n), q2), 0)  :  vec4(0.);

      q2 = q2 + (border * side) * n;
      d2 = (style2 == -2.) ? d2 : vec4(0.);
    }

    vec2 q = edge > 0. ? q1 : q2;

    dy_hw = vec2(dy, hw);
    col = color;

    dx = vec4(q, 1, 0) * mat4(d1,d2,d3,d4);

    gl_Position = vec4(q * scale + vec2(-1., -1.), 0., 1.);
  }
"

let line_fragment_shader = "
  #define FAST 0
  #define BLUR 0.6

  precision mediump float;

  varying mediump vec4 dx;
  varying mediump vec2 dy_hw;
  varying lowp vec4 col;

  void main() {
    float dy = dy_hw.x; float half_width = dy_hw.y;

    vec2 ds = min (dx.xz, dx.yw);

    float dx2 = min (0., ds.x);
    float d = sqrt(dx2 * dx2 + dy * dy);

    d = max(d, -ds.y);

    vec3 dists = vec3(half_width - d,- half_width - d, half_width + ds.y);

#if FAST
    vec3 alphas = clamp(dists * BLUR + 0.5, 0., 1.);
#else
    vec3 alphas = smoothstep(0., 1., dists * BLUR + 0.5);
#endif

    float alpha = /* alphas.z * */ (alphas.x - alphas.y);

    gl_FragColor = col * alpha;
  }
"

let format_lines gl has_uint_indices lines =
  let lines = if back_to_front then List.rev lines else lines in
  let vert_count = List.fold_left (fun n l -> n + List.length l) 0 lines in
  let edge_count = vert_count - List.length lines in
Firebug.console##log(Js.string (Format.sprintf "vertices: %d" vert_count));
Firebug.console##log(Js.string (Format.sprintf "edges: %d" edge_count));

  let coords = Array.make (8 * vert_count + 16) 0. in
  let i = ref 8 in
  List.iter
    (fun l ->
       List.iter
         (fun (x, y, _, _, _, _) ->
            let i' = !i in i := i' + 8;
            coords.(i' + 0) <- x; coords.(i' + 1) <- y;
            coords.(i' + 2) <- x; coords.(i' + 3) <- y;
            coords.(i' + 4) <- x; coords.(i' + 5) <- y;
            coords.(i' + 6) <- x; coords.(i' + 7) <- y)
         l)
    lines;
  let colors = Array.make (16 * vert_count) 0 in
Firebug.console##log_2(Js.string "cols", Array.length colors);
  let i = ref (-2*4) in
  List.iter
    (fun l ->
       List.iter
         (fun (_, _, _, _, c1, c2) ->
            for j = 0 to 1 do
              let i' = !i in i := i' + 8;
              if i' >= 0 then begin
                colors.(i' + 0) <- c1.(0); colors.(i' + 1) <- c1.(1);
                colors.(i' + 2) <- c1.(2); colors.(i' + 3) <- c1.(3);
                colors.(i' + 4) <- c2.(0); colors.(i' + 5) <- c2.(1);
                colors.(i' + 6) <- c2.(2); colors.(i' + 7) <- c2.(3)
              end
            done)
         l)
    lines;
  let data = Array.make (4 * 4 * vert_count) 1 in
(*
  let widths = Array.make (4 * vert_count) 0 in
  let i = ref (-2) in
  List.iter
    (fun l ->
       List.iter
         (fun (_, _, _, w, _, _) ->
            let i' = !i in i := i' + 4;
            let w = truncate (8. *. w +. 0.5) in
            assert (w < 128);
            for j = if i' < 0 then 2 else 0 to 3 do widths.(i' + j) <- w done)
         l)
    lines;
  let styles = Array.make (4 * vert_count) 0 in
  let i = ref 0 in
  List.iter
    (fun l ->
       List.iter
         (fun (_, _, s, _, _, _) ->
            let i' = !i in i := i' + 4;
            let s =
              match s with
                `BUTT -> 1 | `ROUND -> 2 | `SQUARE -> 3
              | `JOIN_MITER -> -1 | `JOIN_ROUND -> -2 | `JOIN_BEVEL -> -3
            in
            for j = 0 to 3 do styles.(i' + j) <- s done)
         l)
    lines;
  let sides = Array.make (4 * (vert_count - 1)) 1 in
  for i = 0 to vert_count - 2 do
    sides.(4 * i + 1) <- -1;
    sides.(4 * i + 3) <- -1
  done;
  let edges = Array.make (4 * (vert_count - 1)) 1 in
  for i = 0 to vert_count - 2 do
    edges.(4 * i + 2) <- -1;
    edges.(4 * i + 3) <- -1
  done;
*)
  let i = ref (-2) in
  List.iter
    (fun l ->
       List.iter
         (fun (_, _, _, w, _, _) ->
            let w = truncate (8. *. w +. 0.5) in
            assert (w < 128);
            let i' = !i in i := i' + 4;
            for j = if i' < 0 then 2 else 0 to 3 do
              data.(4 * (i' + j)) <- w
            done)
         l)
    lines;
  let i = ref 0 in
  List.iter
    (fun l ->
       List.iter
         (fun (_, _, s, _, _, _) ->
            let i' = !i in i := i' + 4;
            let s =
              match s with
                `BUTT -> 1 | `ROUND -> 2 | `SQUARE -> 3
              | `JOIN_MITER -> -1 | `JOIN_ROUND -> -2 | `JOIN_BEVEL -> -3
            in
            for j = 0 to 3 do data.(4 * (i' + j) + 1) <- s done)
         l)
    lines;
  for i = 0 to vert_count - 2 do
    data.(4 * (4 * i + 1) + 2) <- -1;
    data.(4 * (4 * i + 3) + 2) <- -1
  done;
  for i = 0 to vert_count - 2 do
    data.(4 * (4 * i + 2) + 3) <- -1;
    data.(4 * (4 * i + 3) + 3) <- -1
  done;
  let indices = Array.make (6 * edge_count) 0 in
  let i = ref 0 in
  let j = ref 0 in
  List.iter
    (fun l ->
       let n = List.length l - 1 in
       for k = 0 to n - 1 do
         let i' = !i in i := i' + 6;
         let j' = !j in j := j' + 4;
         indices.(i' + 0) <- j' + 0; indices.(i' + 1) <- j' + 1;
         indices.(i' + 2) <- j' + 2; indices.(i' + 3) <- j' + 2;
         indices.(i' + 4) <- j' + 1; indices.(i' + 5) <- j' + 3
       done;
       j := !j + 4)
    lines;
Firebug.console##log(Array.length indices);
    let coords = makeBuffer gl coords in
    let colors = makeUbyteBuffer gl colors in
(*
    let widths = makeByteBuffer gl widths in
    let edges = makeByteBuffer gl edges in
    let sides = makeByteBuffer gl sides in
    let styles = makeByteBuffer gl styles in
*)
    let data = makeByteBuffer gl data in
    let vbuffer = gl##createBuffer() in
  (*XXX FIX: uint! *)
    gl##bindBuffer(gl##_ELEMENT_ARRAY_BUFFER_, vbuffer);
    if has_uint_indices then begin
      let values = jsnew Typed_array.int32Array_fromArray(Js.array indices) in
      gl##bufferData(gl##_ELEMENT_ARRAY_BUFFER_, values, gl##_STATIC_DRAW_)
    end else begin
      let values = jsnew Typed_array.int16Array_fromArray(Js.array indices) in
      gl##bufferData(gl##_ELEMENT_ARRAY_BUFFER_, values, gl##_STATIC_DRAW_)
    end;
    (coords, colors, data, (*widths, edges, sides, styles,*)
     Array.length indices, vbuffer)

let triangulate height a =
(*
  - find point at largest distance from point 0
    ==> split in 2: 0-i / i-n
  - triangulate each subdivisions
*)
  let n = (Array.length a - 4) / 2 in
  let b = Array.make (2 * n + 2) 0. in
  Array.blit a 4 b 0 (2 * n);
  b.(2 * n + 0) <- b.(0);
  b.(2 * n + 1) <- b.(1);
  let c = Array.make (6 * (n - 2)) 0. in
  let p = ref 0 in
  let rec triang i j =
    if i + 1 < j then begin
      let dx = b.(2 * j + 0) -. b.(2 * i + 0) in
      let dy = b.(2 * j + 1) -. b.(2 * i + 1) in
      let s = ref 0. in
      let m = ref (i + 1) in
      for k = i + 1 to j - 1 do
        let dx' = b.(2 * k + 0) -. b.(2 * i + 0) in
        let dy' = b.(2 * k + 1) -. b.(2 * i + 1) in
        let s' = abs_float (dx' *. dy -. dx *. dy') in
        if s' > !s then begin s := s'; m := k end
      done;
(*
Firebug.console##log_3(i, !m, j);
assert (!p < 6 * (n - 2));
*)
      c.(!p + 0) <- b.(2 * i + 0);
      c.(!p + 1) <- height -. b.(2 * i + 1);
      c.(!p + 2) <- b.(2 * !m + 0);
      c.(!p + 3) <- height -. b.(2 * !m + 1);
      c.(!p + 4) <- b.(2 * j + 0);
      c.(!p + 5) <- height -. b.(2 * j + 1);
      p := !p + 6;
      triang i !m; triang !m j
    end
  in
  let m = ref 1 in
  let d = ref 0. in
  for j = 1 to n - 1 do
    let dx = b.(2 * j + 0) -. b.(0) in
    let dy = b.(2 * j + 1) -. b.(1) in
    let d' = dx *. dx +. dy *. dy in
    if d' > !d then begin m := j; d := d' end
  done;
(*
Firebug.console##log_2(Js.string "---", n);
*)
  triang 0 !m;
(*
Firebug.console##log(Js.string "--");
*)
  triang !m n;
(*
Firebug.console##log_2(!p, 6 * (n - 2));
*)
  assert (!p = 6 * (n - 2));
  c

let init _ =
  let canvas = create_canvas 1000 600 in
(*
  let canvas = create_canvas 100 60 in
*)
  Dom.appendChild (Dom_html.document##body) canvas;

  let gl = Js.Opt.get (getContext canvas) (fun _ -> assert false) in
Firebug.console##log(gl##getContextAttributes());

  let has_uint_indices =
    Js.Opt.test
      (Obj.magic (gl##getExtension(Js.string "OES_element_index_uint"))) in

  gl##viewport(0, 0, canvas##width, canvas##height);

  if back_to_front then
    gl##clearColor(1., 1., 1., 1.)
  else
    gl##clearColor(0., 0., 0., 0.);

  let poly_program =
    create_program gl poly_vertex_shader poly_fragment_shader in
  let scale = gl##getUniformLocation(poly_program, Js.string "scale") in
  gl##uniform2f
    (scale, 2. /. float canvas##width, 2. /. float canvas##height);

  let line_program =
    create_program gl line_vertex_shader line_fragment_shader in
  let scale = gl##getUniformLocation(line_program, Js.string "scale") in
  gl##uniform2f
    (scale, 2. /. float canvas##width, 2. /. float canvas##height);
 
  let bbox a =
    let xi = ref a.(0) in
    let xa = ref a.(0) in
    let yi = ref a.(1) in
    let ya = ref a.(1) in
    for i = 0 to Array.length a / 2 - 1 do
      if a.(2 * i) < !xi then xi := a.(2 * i);
      if a.(2 * i) > !xa then xa := a.(2 * i);
      if a.(2 * i + 1) < !yi then yi := a.(2 * i + 1);
      if a.(2 * i + 1) > !ya then ya := a.(2 * i + 1);
    done;
    (!xi, !yi, !xa, !ya)
  in

  let draw_poly transform_matrix (r, g, b, a) bbox len coords =
    gl##useProgram(poly_program);
    let transform =
      gl##getUniformLocation(poly_program, Js.string "transform") in
    gl##uniformMatrix3fv(transform, Js._false, Js.array transform_matrix);
    let color = gl##getUniformLocation(poly_program, Js.string "color") in
(*
let a = 0.5 in
let r = Random.float 1. in
let g = Random.float 1. in
let b = Random.float 1. in
let r = 0.3 in
let g = 0.3 in
let b = 0.3 in
*)
    gl##uniform4f (color, r *. a, g *. a, b *. a, a);
    setAttrib gl poly_program "p" 2 coords;
(*
*)
    gl##enable(gl##_STENCIL_TEST_);

    gl##disable(gl##_CULL_FACE_);
    gl##colorMask(Js._false, Js._false, Js._false, Js._false);
    gl##stencilFunc (gl##_ALWAYS, 0, 0xff);
    gl##stencilOpSeparate
      (gl##_FRONT, gl##_INCR_WRAP_, gl##_INCR_WRAP_, gl##_INCR_WRAP_); 
    gl##stencilOpSeparate
      (gl##_BACK, gl##_DECR_WRAP_, gl##_DECR_WRAP_, gl##_DECR_WRAP_);
    gl##drawArrays(gl##_TRIANGLES, 0, len);

    gl##enable(gl##_CULL_FACE_);
(*
*)
    let perform_depth_test = not back_to_front && a < 1. in
    if perform_depth_test then gl##enable(gl##_DEPTH_TEST_);
    gl##cullFace(gl##_FRONT);
    gl##colorMask(Js._true, Js._true, Js._true, Js._true);
    gl##stencilFunc (gl##_NOTEQUAL, 0, 0xff);
    gl##stencilOp (gl##_REPLACE, gl##_REPLACE, gl##_REPLACE);
(*
for i = 0 to len / 3 - 1 do
let r = Random.float 1. in
let g = Random.float 1. in
let b = Random.float 1. in
gl##uniform4f (color, r *. a, g *. a, b *. a, a);
setAttrib gl poly_program "p" ~offset:(2*4*3*i) 2 coords;
gl##drawArrays(gl##_TRIANGLES, 0, 3)
done;
*)
    gl##drawArrays(gl##_TRIANGLES, 0, len);
    gl##disable(gl##_CULL_FACE_);
    if perform_depth_test then gl##disable(gl##_DEPTH_TEST_);
(*
*)
    gl##disable(gl##_STENCIL_TEST_);
  in

  let draw_lines transform_matrix
        (coords, colors, (*widths, edges, sides, styles,*)
         data, count, indices) =
    gl##useProgram(line_program);
    setAttrib gl line_program "p0" 2 ~offset:0  coords;
    setAttrib gl line_program "p1" 2 ~offset:32 coords;
    setAttrib gl line_program "p2" 2 ~offset:64 coords;
    setAttrib gl line_program "p3" 2 ~offset:96 coords;
    setAttrib gl line_program "color" 4 ~normalize:true
      ~kind:gl##_UNSIGNED_BYTE_DT colors;
    setAttrib gl line_program "data1" 4 ~kind:gl##_BYTE data;
    setAttrib gl line_program "data2" 4 ~offset:(4 * 4) ~kind:gl##_BYTE data;
(*
    setAttrib gl line_program "w" 1 ~kind:gl##_BYTE widths;
    setAttrib gl line_program "edge" 1 ~kind:gl##_BYTE edges;
    setAttrib gl line_program "side" 1 ~kind:gl##_BYTE sides;
    setAttrib gl line_program "style1" 1 ~kind:gl##_BYTE styles;
    setAttrib gl line_program "style2" 1 ~offset:4 ~kind:gl##_BYTE styles;
*)
    let transform =
      gl##getUniformLocation(line_program, Js.string "transform") in
    gl##uniformMatrix3fv(transform, Js._false, Js.array transform_matrix);

    let determinant m = m.(0) *. m.(4) -. m.(1) *. m.(3) in
    let zoom_factor = sqrt(determinant(transform_matrix)) in
    let zoom = gl##getUniformLocation(line_program, Js.string "zoom") in
    gl##uniform1f(zoom, zoom_factor);

    gl##bindBuffer(gl##_ELEMENT_ARRAY_BUFFER_, indices);

    gl##drawElements(gl##_TRIANGLES, count,
                     (if has_uint_indices then gl##_UNSIGNED_INT_ else
                      gl##_UNSIGNED_SHORT_),
                     0)
  in

  if back_to_front then
    gl##blendFunc(gl##_ONE, gl##_ONE_MINUS_SRC_ALPHA_)
  else
    gl##blendFunc(gl##_ONE_MINUS_DST_ALPHA_, gl##_ONE);
  gl##enable(gl##_BLEND);

(*
  let coords =
    [|0.; 0.; 600.; 300.; 0.; 600.;
      150.; 450.; 450.; 300.; 150.; 150. |]
  in
  poly (1., 0.8, 0.8, 0.5) coords;
*)

  let color = ref (-1., -1., -1., -1.) in
  let lst = ref [] in
  let lines = ref [] in
  let prims = ref [] in
let sz = ref 0 in
let pcount = ref 0 in
  let push_lines () =
    if !lines <> [] then begin
      prims := `Lines (format_lines gl has_uint_indices !lines) :: !prims;
      lines := []
    end
  in
  let push_poly () =
    if !lst != [] then begin
      let len = List.fold_left (fun n l -> n + Array.length l) 0 !lst in
      let l' = Array.make len 0. in
      let i = ref 0 in
      List.iter
        (fun l ->
incr pcount;
           let i' = !i in i := i' + Array.length l;
           for j = 0 to Array.length l - 1 do
             l'.(i' + j) <- l.(j)
           done)
        !lst;
sz := !sz + len;
Firebug.console##log(len);
      prims := `Poly (!color, bbox l', Array.length l' / 2, makeBuffer gl l') :: !prims
    end;
    lst := []
  in

let t = now () in
  let scene : float array array = Js.Unsafe.variable "scene" in
  let height = float canvas##height in
Firebug.console##log_2 (Js.string "scene:", Array.length scene);
  for i = 0 to Array.length scene - 1 do
    let a = scene.(i) in
    if a.(0) = 1. then begin
      let n = (Array.length a - 4) / 2 in
      let r = a.(1) in
      let g = a.(2) in
      let b = a.(3) in
(* TRIANGLE STRIP
      let l = Array.make (2 * (n + 2)) 0. in
      let i = ref 0 in
      let j = ref (n - 1) in
      let k = ref 2 in
      while !i <= !j do
        l.(!k + 0) <- a.(2 * !i + 0 + 4);
        l.(!k + 1) <- height -. a.(2 * !i + 1 + 4);
        incr i; k := !k + 2;
        if !i <= !j then begin
          l.(!k + 0) <- a.(2 * !j + 0 + 4);
          l.(!k + 1) <- height -. a.(2 * !j + 1 + 4);
          decr j; k := !k + 2
        end
      done;
      assert (!k = 2 * n + 2);
      l.(0) <- l.(2);
      l.(1) <- l.(3);
      l.(2 * n + 2) <- l.(2 * n + 0);
      l.(2 * n + 3) <- l.(2 * n + 1);
*)
(* TRIANGLES *)
(*
      let l = triangulate height a in
*)
      let l = Array.make (6 * (n - 2)) 0. in
      for i = 0 to n - 3 do
        l.(6 * i + 0) <- a.(4);
        l.(6 * i + 1) <- height -. a.(5);
        l.(6 * i + 2) <- a.(2 * i + 6);
        l.(6 * i + 3) <- height -. a.(2 * i + 7);
        l.(6 * i + 4) <- a.(2 * i + 8);
        l.(6 * i + 5) <- height -. a.(2 * i + 9);
      done;
      let c = (r, g, b, 1.) in
      push_lines ();
      if c <> !color then begin push_poly (); color := c end;
(*
if n < 10 then
*)
      lst := l :: !lst
    end else begin
      push_poly ();
      let r = a.(1) in
      let g = a.(2) in
      let b = a.(3) in
      let w = a.(4) in
      let col x = truncate (x *. 255.99) in
      let color = [|col r; col g; col b; col 1.|] in
      let closed = a.(5) = 1. in
      let join =
        match a.(6) with 1. -> `JOIN_MITER | 2. -> `JOIN_ROUND | _ -> `JOIN_BEVEL
      in
      let cap = match a.(7) with 1. -> `BUTT | 2. -> `ROUND | _ -> `SQUARE in
      let p0 = (a.(8), height -. a.(9), cap, w, color, color) in
      let l = ref [p0] in
      for i = 0 to (Array.length a - 10) / 2 - if closed then 1 else 2 do
        l :=
          (a.(2 * i + 10), height -. a.(2 * i + 11), join, w, color, color)
          :: !l
      done;
      let len = Array.length a in
      if closed then
        l := p0 :: !l
      else
        l := (a.(len - 2), height -. a.(len - 1), cap, w, color, color) :: !l;
      lines := !l :: !lines
    end
  done;
  push_poly (); push_lines ();
Firebug.console##log_3(Js.string "polygons:", !sz, !pcount);
Firebug.console##log_2(Js.string "preparation:", now () -. t);

  let prims = if back_to_front then List.rev !prims else !prims in

  let transform_matrix = ref [|1.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 1.|] in
  let perform_redraw () =
(*Firebug.console##log(Js.string "START");*)
    let t = now () in
    Random.init 0;

    gl##clear
      (gl##_COLOR_BUFFER_BIT_ lor gl##_STENCIL_BUFFER_BIT_ lor
         gl##_DEPTH_BUFFER_BIT_);
    List.iter
      (fun p ->
        match p with
          `Poly (c, bbox, len, l) -> draw_poly !transform_matrix c bbox len l
        | `Lines l                -> draw_lines !transform_matrix l)
      prims;
(*Firebug.console##log_2(Js.string "DONE", now () -. t)*)
  in

  let angle = ref 0. in
  let t = ref (now ()) in
  let dt = ref 15. in
  let dt' = ref 15. in
  let n = ref 0 in
  let rec loop () =
    let t' = now () in
    dt := 0.9 *. !dt +. 0.1 *. (t' -. !t);
    incr n;
    if !n mod 60 = 0 then debug_msg (Format.sprintf "%.1f fps - %.f ms" (1000. /. !dt) !dt');
    t := t';
    if animate then Dom_html._requestAnimationFrame(Js.wrap_callback loop);

    angle := !angle +. 1.;
    let a = !angle *. 3.14 /. 180. in
    let s = 0.5 *. (1.8 +. 0.8 *. cos a) in
    let s = if animate then s else 0.5 in
    transform_matrix := [|s; 0.; 0.; 0.; s; 0.; 500. *. (1. -. s); 300. *. (1. -. s); 1.|];
    perform_redraw ();
    gl##finish ();
    let t'' = now () in
    dt' := 0.9 *. !dt' +. 0.1 *. (t'' -. t')
  in
  loop ();
(*
  let redraw_scheduled = ref false in
  let schedule_redraw () =
    if not !redraw_scheduled then begin
      redraw_scheduled := true;
      Dom_html._requestAnimationFrame
        (Js.wrap_callback
           (fun () -> redraw_scheduled := false; perform_redraw ()))
    end
  in
  schedule_redraw ();

  let old_transform = ref (Array.copy !transform_matrix) in 
  handle_drag canvas
    (fun x0 y0 x1 y1 ->
Firebug.console##log_2(x1 - x0, y1 - y0);
       !transform_matrix.(6) <- !old_transform.(6) +. float (x1 - x0);
       !transform_matrix.(7) <- !old_transform.(7) -. float (y1 - y0);
       schedule_redraw ())
    (fun _ _ -> old_transform := Array.copy !transform_matrix)
    (fun _ _ -> ());

  handle_touch_events canvas
    (fun x0 y0 x1 y1 ->
       !transform_matrix.(6) <- !old_transform.(6) +. float (x1 - x0);
       !transform_matrix.(7) <- !old_transform.(7) -. float (y1 - y0);
       schedule_redraw ())
    (fun _ _ -> old_transform := Array.copy !transform_matrix)
    (fun _ _ ->
       transform_matrix := Array.copy !old_transform; schedule_redraw ())
    (fun _ _ -> ());
*)
  Js._false

let _ = Dom_html.window##onload <- Dom_html.handler init
