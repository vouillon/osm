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

(****)

let get_lines height =
let scene : float array array = Js.Unsafe.variable "scene" in
let lst = ref [] in
Firebug.console##log
  (Js.string (Printf.sprintf "elements: %d" (Array.length scene)));
for i = 0 to Array.length scene - 1 do
  let a = scene.(i) in
  if a.(0) = 0. then begin
    let r = a.(1) in
    let g = a.(2) in
    let b = a.(3) in
    let w = a.(4) in
    let color = [|r; g; b; 1.|] in
    let closed = a.(5) = 1. in
    let join =
      match a.(6) with 1. -> `JOIN_MITER | 2. -> `JOIN_ROUND | _ -> `JOIN_BEVEL
    in
    let cap = match a.(7) with 1. -> `BUTT | 2. -> `ROUND | _ -> `SQUARE in
    let p0 = (a.(8), height -. a.(9), cap, w, color, color) in
    let l = ref [p0] in
    for i = 0 to (Array.length a - 10) / 2 - if closed then 1 else 2 do
      l :=
        (a.(2 * i + 10), height -. a.(2 * i + 11), join, w, color, color) :: !l
    done;
    let len = Array.length a in
    if closed then
      l := p0 :: !l
    else
      l := (a.(len - 2), height -. a.(len - 1), cap, w, color, color) :: !l;
    lst := !l :: !lst
  end;
done;
List.rev !lst

(****)

let vertex_shader = "
  precision highp float;

  uniform vec2 scale;
  uniform mat3 transform;

  attribute vec2 p0, p1, p2, p3; // the two extremities of the line
  attribute vec2 n;
  attribute float edge;   // source or target
  attribute float side;   // which side of the line

  attribute float w;          // line width
  attribute lowp vec4 color;  // line color
  attribute float style1, style2; // line style

  varying mediump vec4 dx;
  varying mediump vec2 dy_hw;
  varying lowp vec4 col;

  float determinant (mat3 m) {
    return m[0][0] * m[1][1] - m[0][1] * m[1][0];
  }

  void main () {
    vec2 q1 = (transform * vec3(p1, 1)).xy;
    vec2 q2 = (transform * vec3(p2, 1)).xy;
    float hw = w * sqrt(determinant(transform)) * 0.5;

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
      d1.z = (style1 == 3.) ? (d1.z+border): d1.z;
      d3.z = (style1 == 1.) ? (d3.z-border): d3.z;
    } else {
      // Line join
      vec2 q0 = (transform * vec3(p0, 1)).xy;
      vec2 t0 = normalize(q1 - q0);
      vec2 n0 = vec2(-t0.y, t0.x);
      vec2 n = (n0 + n1) / (1. + dot(n0, n1));

d3 = (style1 == -3.) ? vec4(-normalize(n), dot(normalize(n), q1), 0)  :  vec4(0.);

      q1 = q1 + (border * side) * n;
      d1 = (style1 == -2.) ? d1 : vec4(0.);
//      d3 = vec4(0.);
    }

    if (style2 > 0.) {
      // Line cap
      q2 = q2 + (border * side) * n1 + border * t1;
      d2.z = (style2 == 3.) ? (d2.z+border): d2.z;
      d4.z = (style2 == 1.) ? (d4.z-border): d4.z;
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

let fragment_shader = "
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

let back_to_front = true

let create_canvas w h =
  let d = Dom_html.window##document in
  let c = Dom_html.createCanvas d in
  c##width <- w;
  c##height <- h;
  c

let getContext (c : Dom_html.canvasElement Js.t) : WebGL.renderingContext Js.t Js.opt =
  let c = Js.Unsafe.coerce c in
  c##getContext (Js.string "experimental-webgl",
                 Js.Unsafe.(obj [|"antialias", inject Js._false|]))

let setAttrib
      (gl:WebGL.renderingContext Js.t) program name size ?(offset=0) values =
(*Firebug.console##log(Js.string (Format.sprintf "%s: %d" name (Array.length values)));*)
  let attr = gl##getAttribLocation(program, Js.string name) in
  let vbuffer = gl##createBuffer() in
  let values = jsnew Typed_array.float32Array_fromArray(Js.array values) in
  gl##bindBuffer(gl##_ARRAY_BUFFER_, vbuffer);
  gl##bufferData(gl##_ARRAY_BUFFER_, values, gl##_STATIC_DRAW_);
  gl##enableVertexAttribArray(attr);
  gl##vertexAttribPointer(attr, size, gl##_FLOAT, Js._false, 0, offset)

let (|>) x f = f x

let init _ =
  let canvas = create_canvas 1000 600 in
  Dom.appendChild (Dom_html.document##body) canvas;

  let gl = Js.Opt.get (getContext canvas) (fun _ -> assert false) in

  let has_uint_indices =
    Js.Opt.test
      (Obj.magic (gl##getExtension(Js.string "OES_element_index_uint"))) in
  gl##viewport(0, 0, canvas##width, canvas##height);
  if back_to_front then
    gl##clearColor(1., 1., 1., 1.)
  else
    gl##clearColor(0., 0., 0., 0.);

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

  let scale = gl##getUniformLocation(program, Js.string "scale") in
  gl##uniform2f
    (scale, 2. /. float canvas##width, 2. /. float canvas##height);
  let transform = gl##getUniformLocation(program, Js.string "transform") in
  let transform_matrix = [|0.5; 0.; 0.; 0.; 0.5; 0.; 0.; 0.; 1.|] in
  let transform_matrix = [|1; 0; 0; 0; 1; 0; 0; 0; 1|] in
  let transform_matrix = [|2.; 0.; 0.; 0.; 2.; 0.; -50.; 0.; 1.|] in

  gl##uniformMatrix3fv(transform, Js._false, Js.array transform_matrix);

  if back_to_front then
    gl##blendFunc(gl##_ONE, gl##_ONE_MINUS_SRC_ALPHA_)
  else
    gl##blendFunc(gl##_ONE_MINUS_DST_ALPHA_, gl##_ONE);
  gl##enable(gl##_BLEND);
(*
  gl##enable(gl##_DEPTH_TEST_);
  gl##depthFunc(gl##_LEQUAL);
*)

(*
  let black = [|0.; 0.; 0.; 1.|] in
  let red = [|1.; 0.; 0.; 1.|] in
  let blue = [|0.; 0.; 1.; 1.|] in

  let p0 = (100., 100., `SQUARE, 6., black, black) in
  let p1 = (500., 200., `ROUND,  6., black, black) in
  
  let p2 =
    (100., 100., `ROUND, 4.,
     [|0.6; 0.4; 0.6; 1.|], [|0.6; 0.4; 0.6; 1.|]) in
  let p3 =
    (500., 200., `BUTT, 4.,
     [|0.2; 0.4; 0.6; 1.|], [|0.2; 1.0; 0.6; 1.|]) in

  let q0 = (100., 200., `ROUND, 8., blue, blue) in
  let q1 = (300., 250., `JOIN_ROUND, 8., black, black) in
  let q2 = (300., 200., `JOIN_MITER, 8., red, red) in
  let q3 = (500., 250., `JOIN_BEVEL, 8., black, black) in
  let q4 = (450., 220., `ROUND, 8., black, black) in
(*
  let q3 = (200., 200., `ROUND, 8., red, red) in
*)

  let lines = [[p0; p1]; [p2; p3]; [q0; q1; q2; q3; q4]] in
*)

  let lines = get_lines (float canvas##height) in
  let lines = if back_to_front then lines else List.rev lines in

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
  let colors = Array.make (16 * vert_count) 0. in
  let i = ref 0 in
  List.iter
    (fun l ->
       List.iter
         (fun (_, _, _, _, c1, c2) ->
            for j = 0 to 1 do
              let i' = !i in i := i' + 8;
              colors.(i' + 0) <- c1.(0); colors.(i' + 1) <- c1.(1);
              colors.(i' + 2) <- c1.(2); colors.(i' + 3) <- c1.(3);
              colors.(i' + 4) <- c2.(0); colors.(i' + 5) <- c2.(1);
              colors.(i' + 6) <- c2.(2); colors.(i' + 7) <- c2.(3)
            done)
         l)
    lines;
  let widths = Array.make (4 * vert_count) 0. in
  let i = ref 0 in
  List.iter
    (fun l ->
       List.iter
         (fun (_, _, _, w, _, _) ->
            let i' = !i in i := i' + 4;
            for j = 0 to 3 do widths.(i' + j) <- w done)
         l)
    lines;
  let styles = Array.make (4 * vert_count) 0. in
  let i = ref 0 in
  List.iter
    (fun l ->
       List.iter
         (fun (_, _, s, _, _, _) ->
            let i' = !i in i := i' + 4;
            let s =
              match s with
                `BUTT -> 1. | `ROUND -> 2. | `SQUARE -> 3.
              | `JOIN_MITER -> -1. | `JOIN_ROUND -> -2. | `JOIN_BEVEL -> -3.
            in
            for j = 0 to 3 do styles.(i' + j) <- s done)
         l)
    lines;
  let sides = Array.make (4 * (vert_count - 1)) 1. in
  for i = 0 to vert_count - 2 do
    sides.(4 * i + 1) <- -1.;
    sides.(4 * i + 3) <- -1.
  done;
  let edges = Array.make (4 * (vert_count - 1)) 1. in
  for i = 0 to vert_count - 2 do
    edges.(4 * i + 2) <- -1.;
    edges.(4 * i + 3) <- -1.
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

  (*XXX We should share the buffer for p1 and p2... *)
  setAttrib gl program "p0" 2 ~offset:0  coords;
  setAttrib gl program "p1" 2 ~offset:32 coords;
  setAttrib gl program "p2" 2 ~offset:64 coords;
  setAttrib gl program "p3" 2 ~offset:96 coords;
  setAttrib gl program "w" 1 ~offset:(2*4) widths;
  setAttrib gl program "edge" 1 edges; (* use integers and normalize... *)
  setAttrib gl program "side" 1 sides;
  setAttrib gl program "color" 4 ~offset:(4*2*4) colors;
  setAttrib gl program "style1" 1 styles;
  setAttrib gl program "style2" 1 ~offset:16 styles;

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

Firebug.console##log(gl##getContextAttributes());
Firebug.console##log(gl##getParameter(gl##_SAMPLES_));

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
    Dom_html._requestAnimationFrame(Js.wrap_callback loop);
(*
*)
    angle := !angle +. 1.;
    let a = !angle *. 3.14 /. 180. in
    let s = 0.5 *. (1.8 +. 0.8 *. cos a) in
(*
let s = 1. in
*)
    let transform_matrix = [|s; 0.; 0.; 0.; s; 0.; 0.; 0.; 1.|] in
    gl##uniformMatrix3fv(transform, Js._false, Js.array transform_matrix);

    gl##clear(gl##_COLOR_BUFFER_BIT_);
    gl##drawElements(gl##_TRIANGLES, Array.length indices,
                     (if has_uint_indices then gl##_UNSIGNED_INT_ else
                      gl##_UNSIGNED_SHORT_),
                     0);

    gl##finish ();
    let t'' = now () in
    dt' := 0.9 *. !dt' +. 0.1 *. (t'' -. t')
  in
  loop ();

  Js._false

let init2 _ =
  let canvas = create_canvas 1000 600 in
  Dom.appendChild (Dom_html.document##body) canvas;
  let ctx = canvas##getContext (Dom_html._2d_) in
let scene = Js.Unsafe.variable "scene" in
(*ctx##scale (0.5, 0.5);*)
for i = 0 to Array.length scene - 1 do
  let a = scene.(i) in
  if a.(0) = 0. then begin
  Firebug.console##log_2(a.(8), a.(9));
    let r = a.(1) in
    let g = a.(2) in
    let b = a.(3) in
    let w = a.(4) in
    ctx##lineWidth <- w;
    let color =
      Printf.sprintf "#%02x%02x%02x"
        (truncate (r *. 255.)) (truncate (g *. 255.)) (truncate (b *. 255.)) in
    ctx##strokeStyle <- Js.string color;
    let closed = a.(5) in
    ctx##lineJoin <-
      Js.string
        (match a.(6) with 1. -> "miter" | 2. -> "round" | _ -> "bevel");
    ctx##lineCap <-
      Js.string
        (match a.(7) with 1. -> "butt" | 2. -> "round" | _ -> "square");
    ctx##beginPath ();
    ctx##moveTo (a.(8), a.(9));
    for i = 0 to (Array.length a - 10) / 2 - 1 do
      ctx##lineTo (a.(2 * i + 10), a.(2 * i + 11))
    done;
    if closed = 1. then ctx##closePath ();
    ctx##stroke ();
  end
done;
Js._false

let _ = Dom_html.window##onload <- Dom_html.handler init
