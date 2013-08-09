
(*
Smooth (C^2) Bezier splines
http://www.particleincell.com/blog/2012/bezier-splines/

Tends to oscillate a lot
*)
let interpolate p =
  let n = Array.length p - 1 in
  let a = Array.make n 0. in
  let b = Array.make n 0. in
  let c = Array.make n 0. in
  let q = Array.make n 0. in
  b.(0) <- 2.;
  c.(0) <- 1.;
  q.(0) <- p.(0) +. 2. *. p.(1);
  for i = 1 to n - 2 do
    a.(i) <- 1.;
    b.(i) <- 4.;
    c.(i) <- 1.;
    q.(i) <- 4. *. p.(i) +. 2. *. p.(i + 1)
  done;
  a.(n - 1) <- 2.;
  b.(n - 1) <- 7.;
  q.(n - 1) <- 8. *. p.(n - 1) +. p.(n);

  c.(0) <- c.(0) /. b.(0);
  q.(0) <- q.(0) /. b.(0);
  for i = 1 to n - 1 do
    let m = 1.0 /. (b.(i) -. a.(i) *. c.(i - 1)) in
    c.(i) <- c.(i) *. m;
    q.(i) <- (q.(i) -. a.(i) *. q.(i - 1)) *. m
  done;
  for i = n - 2 downto 0 do
    q.(i) <- q.(i) -. c.(i) *. q.(i + 1)
  done;

  let r = Array.make n 0. in
  for i = 0 to n - 2 do
    r.(i) <- 2. *. p.(i + 1) -. q.(i + 1)
  done;
  r.(n - 1) <- (p.(n) +. q.(n - 1)) /. 2.;

  (q, r)


(* Cardinal splines with uniform parametrization; not so good either... *)

let t = 0.5

let interpolate p =
  let n = Array.length p - 1 in
  let p' = Array.make (n + 1) 0. in
  p'.(0) <- t *. (p.(1) -. p.(0)) *. 2.;
  for i = 1 to n - 1 do
    p'.(i) <- t *. (p.(i + 1) -. p.(i - 1))
  done;
  p'.(n) <- t *. (p.(n) -. p.(n - 1)) *. 2.;
  let q = Array.make n 0. in
  let r = Array.make n 0. in
  for i = 0 to n - 1 do
    q.(i) <- p.(i) +. p'.(i) /. 3.;
    r.(i) <- p.(i + 1) -. p'.(i + 1) /. 3.;
  done;
  (q, r)

(****)

(*
   Catmull-Rom Curves with centripedal parametrization (a = 0.25)
   See: "On the Parameterization of Catmull-Rom Curves"
*)

let eps = 1e-2

let interpolate_1d d p q r j j' =
  for i = j to j' - 2 do
    q.(i + 1) <-
      (d.(i) ** 2. *. p.(i + 2)
       -. d.(i + 1) ** 2. *. p.(i)
       +. (2. *. d.(i) ** 2.
           +. 3. *. d.(i) *. d.(i + 1)
           +. d.(i + 1) ** 2.) *. p.(i + 1))
      /. (3. *. d.(i) *. (d.(i) +. d.(i + 1)));
    r.(i) <-
      (d.(i + 1) ** 2. *. p.(i)
       -. d.(i) ** 2. *. p.(i + 2)
       +. (2. *. d.(i + 1) ** 2.
           +. 3. *. d.(i + 1) *. d.(i)
           +. d.(i) ** 2.) *. p.(i + 1))
      /. (3. *. d.(i + 1) *. (d.(i + 1) +. d.(i)))
  done;
(*
  (* Tangent at point p.(0) go through p.(1); looks bad *)
  q.(j) <- (2. *. p.(j) +. p.(j + 1)) /. 3.;
  r.(j' - 1) <- (2. *. p.(j') +. p.(j' - 1)) /. 3.;
  (* Take p.(i -1) = p.(i) *)
  q.(j) <- p.(j);
  r.(j' - 1) <- p.(j');
*)
  (* Second derivative is 0 at point p.(0); result in less deviations *)
  q.(j) <- (p.(j) +. r.(j)) /. 2.;
  r.(j' - 1) <- (p.(j') +. q.(j' - 1)) /. 2.

let interpolate_1d d p q r j j' =
  if j' = j + 1 then begin
    q.(j) <- p.(j);
    r.(j) <- q.(j);
  end else
    interpolate_1d d p q r j j'

let min_cos = 0.8
let max_dist = 100.
let alpha = 0.5

let interpolate2 d x y =
  let n = Array.length x - 1 in
  let x1 = Array.make n 0. in
  let y1 = Array.make n 0. in
  let x2 = Array.make n 0. in
  let y2 = Array.make n 0. in
  let d' = Array.make n 0. in
  for i = 0 to n - 1 do
    d'.(i) <- d.(i) ** alpha
  done;
  let j = ref 0 in
  for k = 1 to n - 1 do
    let cos =
      ((x.(k) -. x.(k - 1)) *. (x.(k + 1) -. x.(k)) +.
       (y.(k) -. y.(k - 1)) *. (y.(k + 1) -. y.(k)))
        /. d.(k - 1) /. d.(k)
    in
    let sin =
      ((x.(k) -. x.(k - 1)) *. (y.(k + 1) -. y.(k)) -.
       (y.(k) -. y.(k - 1)) *. (x.(k + 1) -. x.(k)))
        /. d.(k - 1) /. d.(k)
    in
    let h d1 d2 =
      let r = d1 /. d2 in
      d2 *. (r ** (1. -. alpha) /. 4. /. (1. +. r ** alpha)) *. abs_float sin
    in
    let d1 = d.(k -1) in
    let d2 = d.(k) in
(*
Format.eprintf "%b %f %f %f@." (cos >= min_cos) cos (h d1 d2) (h d2 d1);
Format.eprintf "%f@." cos;
*)
    (* We do not smooth sharp edged; no smoothing if it makes the path
       deviate too much *)
    (* When the path deviates too much, we could add intermediate points
       too make the deviation more localized *)
    if cos < min_cos || h d1 d2 > max_dist || h d2 d1 > max_dist then begin
      interpolate_1d d' x x1 x2 !j k;
      interpolate_1d d' y y1 y2 !j k;
      j := k
    end
  done;
  interpolate_1d d' x x1 x2 !j n;
  interpolate_1d d' y y1 y2 !j n;
  ((x, y), (x1, y1), (x2, y2))

let remove_duplicates n d x y =
  let l = Array.length x in
  let x' = Array.make (l - n) 0. in
  let y' = Array.make (l - n) 0. in
  let d' = Array.make (l - n - 1) 0. in
  x'.(0) <- x.(0);
  y'.(0) <- y.(0);
  let j = ref 1 in
  for i = 1 to l - 1 do
    if d.(i - 1) > eps then begin
      x'.(!j) <- x.(i);
      y'.(!j) <- y.(i);
      d'.(!j - 1) <- d.(i - 1);
      incr j
    end
  done;
  (d', x', y')

let perform x y =
  let n = Array.length x - 1 in
  let d = Array.make n 0. in
  let z = ref 0 in
  for i = 0 to n - 1 do
    d.(i) <- sqrt ((x.(i + 1) -. x.(i)) ** 2. +. (y.(i + 1) -. y.(i)) ** 2.);
    if d.(i) < eps then incr z
  done;
  if !z > 0 then
    let (d, x, y) = remove_duplicates !z d x y in
    interpolate2 d x y
  else
    interpolate2 d x y
