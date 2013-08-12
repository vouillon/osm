type node =
  { key : int;
    remove : unit -> unit;
    mutable prev : node;
    mutable next : node }

type t =
  { mutable lst : node option;
    max_size : int;
    mutable size : int;
    mutable hits : int;
    mutable misses : int }

let front cache =
  match cache.lst with
    Some n' -> n'
  | None    -> assert false

let remove n =
  n.prev.next <- n.next;
  n.next.prev <- n.prev

let insert_before n n' =
  n.prev <- n'.prev;
  n.next <- n';
  n'.prev.next <- n;
  n'.prev <- n

let move_to_front cache n =
  let n' = front cache in
  if n' != n then begin
    remove n;
    insert_before n n';
    cache.lst <- Some n
  end

let insert_value cache i f =
  match cache.lst with
    Some n' ->
      let n = { key = i; remove = f; prev = n'; next = n' } in
      insert_before n n';
      cache.lst <- Some n
  | None ->
      let rec n = { key = i; remove = f; prev = n; next = n } in
      cache.lst <- Some n

let find cache tbl f i =
  try
    let (v, n) = Hashtbl.find tbl i in
    cache.hits <- cache.hits + 1;
    move_to_front cache n;
    v
  with Not_found ->
    cache.misses <- cache.misses + 1;
    let v = f i in
    if cache.size = cache.max_size then begin
      let n' = (front cache).prev in
      n'.remove ();
      remove n'
    end else
      cache.size <- cache.size + 1;
    insert_value cache i (fun () -> Hashtbl.remove tbl i);
    Hashtbl.add tbl i (v, front cache);
    v

let make n =
  { lst = None;
    max_size = n; size = 0;
    hits = 0; misses = 0 }

let funct cache f =
  let tbl = Hashtbl.create 256 in
  fun i -> find cache tbl f i
