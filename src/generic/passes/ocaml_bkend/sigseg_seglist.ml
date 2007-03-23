
type sample = int  (* Should be int64 *)
(* Doesn't have timebase *)
type 't sigseg = SS of ('t array list * sample * int)

open Array
let nullseg = SS([],0,0)
let timebase ss          = 0
let width (SS(_,_,w))    = w
let ss_start (SS(_,s,_)) = s
let ss_end (SS(_,s,w))   = s + w - 1
let toSigseg arr st tb = SS([arr], st, length arr)

let ss_get (SS(ls,_,_)) i = 
  let rec loop ls i =
    match ls with 
      |   [] -> raise (Failure "ss_get out of bounds ref")
      | h::t -> 
	  if i < length h
	  then h.(i) 
	  else loop t (i - length h)
  in loop ls i

(* Doesn't cache result yet. *)
let toArray (SS(ls,w,a)) = concat ls

let joinsegs (SS(a,t1,w1)) (SS(b,t2,w2)) = 
  assert (t2 == t1 + w1);
  SS(List.append a b, t1, w1+w2)

let subseg (SS(ls,st,w)) pos len = 
  let rec loop ls i =
    match ls with 
      |   [] -> raise (Failure "subseg out of bounds")
      | h::t -> 
	  if i < length h
	  then (if i+len <= length h
		then [sub h i len]
		else (sub h i (length h - i) ::
		      loop2 t (i + len - length h)))
	  else loop t (i - length h)
  and loop2 ls j = 
    match ls with 
      |   [] -> raise (Failure "subseg out of bounds")
      | h::t -> 
	  if j < length h
	  then [sub h 0 j]
	  else loop t (j - length h)
  in 
    SS(loop ls (pos-st), pos, len)
