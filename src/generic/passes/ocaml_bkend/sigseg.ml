


type sample = int  (* Should be int64 *)
(* Doesn't have timebase *)
type ('a,'b)sigseg = SS of (('a,'b) wsarray * sample )  

let joinsegs (SS(a,t1)) (SS(b,t2)) = 
  assert (t2 == t1 + wslen a);
  SS(wsappend a b, t1)
let subseg (SS(a,st)) pos len = SS(wssub a (pos - st) len, pos)
let nullseg t                 = SS(wsnull t,0)
let timebase ss               = 0
let toSigseg arr st tb        = SS(arr, st)
let toArray (SS(a,_))         = a
let width (SS(a,_))           = wslen a
let ss_start (SS(_,s))        = s
let ss_end (SS(a,s))          = s + wslen a - 1
let ss_get (SS(a,_)) i        = wsget a i

