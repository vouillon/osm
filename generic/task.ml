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
TODO:
- error handling
- clear marshalled when large
- should keep track of the state of each process (idle or not)
  ==> deal with several function invocation (either failure or queueing?)
*)

let debug_task = Debug.make "tasks" "debug client/server communication" []

module Utimer = Util.Utimer

type stats =
  { mutable marshal_time : float;
    mutable unmarshal_time : float }

let stats = { marshal_time = 0.; unmarshal_time = 0. }
let side = ref "SRV"

type indirect =
  { pipe_in : Unix.file_descr;
    pipe_out : Unix.file_descr;
    mem : Bytearray.t;
    pid : int }

type 'a t = Local of 'a | Remote of indirect

type 'a future_state = Running of indirect | Finished of 'a

type 'a future = 'a future_state ref

let mem_size = 1 lsl 24

external processor_count : unit -> int = "task_processor_count"

let proc_count = ref (processor_count ())

let get_processor_count () = !proc_count
let set_processor_count n = proc_count := if n < 1 then 1 else n

let function_count = ref 0
let functions = Hashtbl.create 17

let send pipe i l =
  let s = Printf.sprintf "%d %d\n" i l in
  ignore (Unix.write pipe s 0 (String.length s))

let receive pipe =
  let s = String.create 50 in
  let len = Unix.read pipe s 0 (String.length s) in
  if len = 0 then exit 1;
  Scanf.sscanf s "%d %d" (fun i l -> (i, l))

let read mem l =
  let t = Utimer.start () in
  let res = Bytearray.unmarshal mem 0 in (*XXX Clear the data if large*)
  let dt = Utimer.stop t in
  stats.unmarshal_time <- stats.unmarshal_time +. dt;
  if debug_task () then Format.eprintf "Unmarshal: %s %.3f (%d)@." !side dt l;
  res

let write mem v =
  let t = Utimer.start () in
  let res = Bytearray.marshal_to_buffer mem 0 v [] in
  let dt = Utimer.stop t in
  stats.marshal_time <- stats.marshal_time +. dt;
  if debug_task () then Format.eprintf "Marshal:   %s %.3f (%d)@." !side dt res;
  res

let funct f =
  let i = !function_count in
  incr function_count;
  Hashtbl.add functions i
    (fun st mem l -> write mem (f (Obj.obj st) (read mem l)));
  fun st x ->
    match st with
      Local st ->
        ref (Finished (f st x))
    | Remote st ->
        send st.pipe_out i (write st.mem x);
        ref (Running st)

let _ =
at_exit (fun _ ->
if debug_task () then
  Format.eprintf "===>> marshal: %.3f / unmarshal: %.3f / user: %.3f@."
    stats.marshal_time stats.unmarshal_time (Unix.times ()).Unix.tms_utime)

let spawn f =
  if !proc_count <= 1 then
    Local (f ())
  else begin
    let (cr, sw) = Unix.pipe () in
    let (sr, cw) = Unix.pipe () in
    let fd = Unix.openfile "/dev/zero" [Unix.O_RDWR] 0 in
    let mem =
      Bigarray.Array1.map_file
        fd Bigarray.char Bigarray.c_layout true mem_size
    in
    Unix.close fd;
    match Unix.fork () with
      0 ->
        Unix.close sr; Unix.close sw;
        stats.marshal_time <- 0.; stats.unmarshal_time <- 0.;
        side := "CLI";
        let st = Obj.repr (f ()) in
        let rec loop () =
          let (i, l) = receive cr in
          if i < 0 then
            exit 0
          else begin
            let g = Hashtbl.find functions i in
            let l = g st mem l in
            send cw 0 l;
            loop ()
          end
        in
        loop ()
    | pid ->
        Unix.close cr; Unix.close cw;
        Remote { pipe_in = sr; pipe_out = sw; mem = mem; pid = pid }
  end

let kill st =
  match st with
    Local _   ->
      ()
  | Remote st ->
      send st.pipe_out (-1) 0;
      Unix.close st.pipe_in; Unix.close st.pipe_out;
      (*XXX Clear mmapped memory *)
      ignore (Unix.waitpid [] st.pid)

let wait fut =
  match !fut with
    Finished v ->
      v
  | Running st ->
      let t = Unix.gettimeofday () in
      let (i, l) = receive st.pipe_in in
      if debug_task () then
        Format.eprintf "Wait:         %.3f@." (Unix.gettimeofday () -. t);
      let v = read st.mem l in
      fut := Finished v;
      v

type scheduler =
  { mutable fds : Unix.file_descr list;
    conts : (Unix.file_descr, int -> unit) Hashtbl.t }

let scheduler () = { fds = []; conts = Hashtbl.create 11 }

let async sched fut f =
  match !fut with
    Finished v ->
      f v
  | Running st ->
      let g l =
        let v = read st.mem l in
        fut := Finished v;
        f v
      in
      sched.fds <- st.pipe_in :: sched.fds;
      Hashtbl.add sched.conts st.pipe_in g

let run sched =
  while sched.fds <> [] do
    let t = Unix.gettimeofday () in
    let (avail, _, _) = Unix.select sched.fds [] [] (-1.) in
    if debug_task () then
      Format.eprintf "Wait:         %.3f@." (Unix.gettimeofday () -. t);
    sched.fds <- List.filter (fun fd -> not (List.mem fd avail)) sched.fds;
    List.iter
      (fun fd ->
         let cont = Hashtbl.find sched.conts fd in
         Hashtbl.remove sched.conts fd;
         let (i, l) = receive fd in
         cont l)
      avail
  done

let map l pre post =
  List.map (fun x -> post (wait x)) (List.map pre l)

let iter_ordered l pre post =
  List.iter (fun x -> post (wait x)) (List.map pre l)

let iteri_ordered l pre post =
  List.iter (fun (x, y) -> post x (wait y)) (List.map pre l)

let iter l pre post =
  let s = scheduler () in
  List.iter (fun v -> async s (pre v) post) l;
  run s

let iteri l pre post =
  let s = scheduler () in
  List.iter (fun x -> let (y, t) = pre x in async s t (fun z -> post y z)) l;
  run s

(*

#ifdef MADV_REMOVE
    if (madvise(ptr, size, MADV_REMOVE) >= 0)
        return;
#endif

#ifdef MADV_FREE
    if (madvise(ptr, size, MADV_FREE) >= 0)
        return;
#endif

#ifdef MADV_DONTNEED
    madvise(ptr, size, MADV_DONTNEED);
#endif
}

*)
