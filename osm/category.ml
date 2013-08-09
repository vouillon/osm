

let rec find index key v imin imax =
  (* Invariant:
     i < imin ==> key.(index.(i)) < v
     i >= imax ==> v <= key.(index.(i)) *)
  if imin < imax then begin
    let i = (imin + imax) lsr 1 in
    if key.(index.(i)) < v then
      find index key v (i + 1) imax
    else
      find index key v imin i
  end else
    imin

let sort (orders : int array array) =
  let l = Array.length orders.(0) in
  let index = Array.make l 0 in
  for i = 0 to l - 1 do index.(i) <- i done;
  let n = Array.length orders in
  let rec compare_rec k i i' =
    if k = n then 0 else
    let c = compare orders.(k).(i) orders.(k).(i') in
    if c <> 0 then c else compare_rec (k + 1) i i'
  in
  Array.sort (fun i i' -> compare_rec 0 i i') index;
  index

(****)

module type S = sig
  type t
  val list : t list
end

module Make (X : S) = struct
  let list = Array.of_list (List.sort compare X.list)
  let table =
    let h = Hashtbl.create 128 in
    Array.iteri (fun i c -> Hashtbl.add h c i) list;
    h

  type id = int

  let none = Array.length list + 1000

  let of_id id =
    assert (id != none);
    list.(id)

  let to_id c = try Hashtbl.find table c with Not_found -> none

  type filter = [`Any of string list | `Not of string list]
  type classifier = (string * (filter * X.t) list) list

  let classify dict c =
    let s v = try Dictionary.find dict v with Not_found -> -1 in
    let h = Hashtbl.create 32 in
    List.iter
      (fun (key, l) ->
         let key = s key in
         let h' =
           try
             fst (Hashtbl.find h key)
           with Not_found ->
             let h' = Hashtbl.create 16 in
             Hashtbl.add h key (h', none);
             h'
         in
         List.iter
           (fun (filter, cat) ->
              let cat = to_id cat in
              match filter with
                `Any l ->
                  List.iter
                    (fun value -> Hashtbl.replace h' (s value) cat) l
              | `Not l ->
                  List.iter
                    (fun value -> Hashtbl.replace h' (s value) none) l;
                  Hashtbl.replace h key (h', cat))
           l)
      c;
    fun key value ->
      try
        let (h', def) = Hashtbl.find h key in
        try Hashtbl.find h' value with Not_found -> def
      with Not_found ->
        none

  let filter dict c =
    let f = classify dict c in
    fun key value -> f key value != none


  module Partition = struct
    type group = int

    type t =
      { mutable next_group : int;
        mutable mapping : group array;
        mutable next_cat : int;
        mutable total_order : int array }

    let make () =
      let len = Array.length list in
      { next_group = 0; mapping = Array.make len (-1);
        next_cat = 0; total_order = Array.make len (-1) }
        
    let add_group p g =
      let i = p.next_group in
      p.next_group <- p.next_group + 1;
      List.iter
        (fun id ->
           assert (p.mapping.(id) = -1);
           p.mapping.(id) <- i;
           let j = p.next_cat in
           p.next_cat <- p.next_cat + 1;
           p.total_order.(id) <- j)
        (List.map to_id g);
      i

    type ('a, 'b) collection =
      { partition : t;
        data : 'a array;
        categorize : 'a -> id;
        keys : int array list;
        groups : int array option;
        mutable index : int array option }

    let apply partition data categorize =
      for i = 0 to Array.length partition.mapping - 1 do
        assert (partition.mapping.(i) <> -1)
      done;
      { partition; data; categorize; keys = []; groups = None; index = None }

    let order_by k c =
      assert (Array.length k = Array.length c.data);
      {c with keys = k :: c.keys; index = None }

    let order_by_group c =
      let g =
        match c.groups with
          Some g ->
            g
        | None ->
            let l = Array.length c.data in
            let g = Array.make l 0 in
            let mapping = c.partition.mapping in
            let data = c.data in
            let categorize = c.categorize in
            for i = 0 to l - 1 do
              let j = categorize data.(i) in
              assert (j <> none);
              g.(i) <- mapping.(j)
            done;
            g
      in
      { c with keys = g :: c.keys; index = None }

    let order_totally c =
      let l = Array.length c.data in
      let g = Array.make l 0 in
      let o = Array.make l 0 in
      let mapping = c.partition.mapping in
      let total_order = c.partition.total_order in
      let data = c.data in
      let categorize = c.categorize in
      for i = 0 to l - 1 do
        let j = categorize data.(i) in
        assert (j <> none);
        o.(i) <- total_order.(j);
        g.(i) <- mapping.(j)
      done;
      { c with keys = o :: c.keys; groups = Some g; index = None }

    type ('a, 'b) iterator =
      { i_data : 'a array;
        i_index : int array;
        i_keys : int array list;
        i_start : int;
        i_end : int }

    let select c =
      let index =
        match c.index with
          Some index ->
            index
        | None ->
            let index = sort (Array.of_list c.keys) in
            c.index <- Some index;
            index
      in
      { i_data = c.data;
        i_index = index;
        i_keys = c.keys;
        i_start = 0;
        i_end = Array.length index }

    let with_key k i =
      match i.i_keys with
        key :: rem ->
          let s = find i.i_index key k i.i_start i.i_end in
          let e = find i.i_index key (k + 1) i.i_start i.i_end in
          { i with i_keys = rem; i_start = s; i_end = e }
      | _ ->
          assert false

    let with_group = with_key

    let iter f i =
      for j = i.i_start to i.i_end - 1 do
        f i.i_data.(i.i_index.(j))
      done

(*
linear features:
- is tunnel
- group (waterway, others)
- layer
- position in group
then select tunnel,group and iterate on layers
*)
  end
end
