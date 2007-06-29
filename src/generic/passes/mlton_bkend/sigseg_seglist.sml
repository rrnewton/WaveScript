
(*
  [2007.03.26] Switching to bigarray.
*)

open Bigarray

type sample = int  (* Should be int64 *)

(*   Doesn't have timebase:  *)
type ('a,'b) sigseg = 
    SS of (('a, 'b, c_layout) Array1.t list *   (* List of data segments*)
	     sample *                           (* Start sample number *)
	     int )                              (* Total width of window *)
(*	     ('a,'b) Bigarray.kind)             (* kind for the bigarray *) *)

let nullseg  t = SS([],0,0)
let timebase ss          = 0
let width    (SS(_,_,w)) = w
let ss_start (SS(_,s,_)) = s
let ss_end   (SS(_,s,w)) = s + w - 1

let toSigseg arr st tb   = SS([arr], st, Array1.dim arr)

let ss_get (SS(ls,_,_)) i = 
  let rec loop ls i =
    match ls with 
      |   [] -> raise (Failure "ss_get out of bounds ref")
      | h::t -> 
	  if i < Array1.dim h
	  then   Array1.get h i
	  else loop t (i - Array1.dim h)
  in loop ls i


let concatArray1 ls w kind = 
  let newarr = Array1.create kind c_layout w in
  let rec loop ls ind = 
    (*Printf.printf "concat loop len ls %d ind %d recorded width %d\n" (List.length ls) ind w;*)
    match ls with 
      | [] -> newarr
      | h::t -> 
	  let len = Array1.dim h in
	    (*Printf.printf "  length of this chunk: %d\n" len;*)
          for i = 0 to len-1 do 
	    Array1.set newarr (ind+i) (Array1.get h i)
	  done;
	  loop t (ind + len)
  in 
    loop ls 0

(* Doesn't cache result yet. *)
let toArray (SS(arr,st,w)) = 
  match arr with 
    | [] -> raise (Failure "can't toArray a null sigseg")
    | h::t -> concatArray1 (h::t) w (Array1.kind h)

let joinsegs (SS(a,t1,w1)) (SS(b,t2,w2)) = 
  assert (t2 == t1 + w1);
  SS(List.append a b, t1, w1+w2)

let subseg (SS(ls,st,w)) pos len = 
(* TEMP TEMP TEMP: *)
(*  assert (pos - st + len < w); *)
  let rec loop ls i =
    match ls with 
      |   [] -> raise (Failure ("subseg out of bounds: pos " ^ string_of_int pos))
      | h::t -> 
	  let hlen = Array1.dim h in
	  if i < hlen
	  then (if i+len <= hlen
		then [Array1.sub h i len]
		else (Array1.sub h i (hlen - i) ::
		      loop2 t (i + len - hlen)))
	  else loop t (i - hlen)
  and loop2 ls j =
    match ls with 
      |   [] -> raise (Failure "subseg out of bounds")
      | h::t -> 
	  let hlen = Array1.dim h in
	  if j <= hlen
	  then [Array1.sub h 0 j]
	  else loop2 t (j - hlen)
  in 
    SS(loop ls (pos-st), pos, len)

;;  
