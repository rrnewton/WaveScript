
(*
  [2007.03.26] Switching to bigarray.

  [2007.06.29] It looks like I never got around to duplicating things
  to make this work with BOTH array representations.
*)

open Bigarray

type sample = int  (* Should be int64 *)

(*   Doesn't have timebase:  *)
type ('a,'b) sigseg =  {
  data : ('a, 'b, c_layout) Array1.t list;
  start : sample;
  size : int;	     (* Total width of window *)
  offset : int;      (* Offset into the first segment. Allows more sharing. *)
}
(*	     ('a,'b) Bigarray.kind)             (* kind for the bigarray *) *)

let nullseg  t           = { data=[]; start=0; size=0; offset=0 }
let timebase ss          = 0
let width    ss          = ss.size
let ss_start ss          = ss.start
let ss_end   ss          = ss.start + ss.size - 1

let toSigseg arr st tb   = { data=[arr]; start=st; size=Array1.dim arr; offset=0 }

let ss_get ss i = 
  let rec loop ls i =
    match ls with 
      |   [] -> raise (Failure "ss_get out of bounds ref")
      | h::t -> 
	  if i < Array1.dim h
	  then   Array1.get h i
	  else loop t (i - Array1.dim h)
  in loop ss.data (i + ss.offset)

(*
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
let toArray ss = 
  match ss.data with 
    | [] -> raise (Failure "can't toArray a null sigseg")
    | h::t -> concatArray1 (h::t) ss.size (Array1.kind h)
*)

let joinsegs a b = 
  assert (b.start == a.start + a.size);
  assert (a.offset == 0 && b.offset == 0);
  { data= List.append a.data b.data;
    start = a.start; size= a.size+b.size;
    }

let subseg ss pos len = 
  assert (pos - ss.start + len < ss.size);
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
  in loop ss.data (pos - ss.start)
