





type sample = int  (* Should be int64 *)
(* Doesn't have timebase *)
type 'a sigseg = SS of ('a array * sample * int )  

open Array

let joinsegs (SS(a,t1,w1)) (SS(b,t2,w2)) = 
  assert (t2 == t1+w1);
  SS(append a b, t1, w1+w2)

let subseg (SS(a,st,w)) pos len = 
  SS(sub a (pos - st) len, pos, len)

let nullseg = SS([||],0,0)

let timebase ss = 0

let toSigseg arr st tb = SS(arr, st, length arr)

let width (SS(_,_,w)) = w
let ss_start (SS(_,s,_)) = s
let ss_end (SS(_,s,_)) = s

let ss_get (SS(a,_,_)) i = a.(i)

