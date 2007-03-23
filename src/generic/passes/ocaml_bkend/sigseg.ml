



type sample = int  (* Should be int64 *)
(* Doesn't have timebase *)
type 'a sigseg = SS of ('a array * sample )  

open Array
let joinsegs (SS(a,t1)) (SS(b,t2)) = 
  assert (t2 == t1 + length a);
  SS(append a b, t1)
let subseg (SS(a,st)) pos len = SS(sub a (pos - st) len, pos)
let nullseg                   = SS([||],0)
let timebase ss               = 0
let toSigseg arr st tb        = SS(arr, st)
let toArray (SS(a,_))         = a
let width (SS(a,_))           = length a
let ss_start (SS(_,s))        = s
let ss_end (SS(a,s))          = s + length a - 1
let ss_get (SS(a,_)) i        = a.(i)

