


type sample = int  (* Should be int64 *)
(* Doesn't have timebase *)
(*type ('a,'b)sigseg = SS of (('a,'b) wsarray * sample )  *)
type ('a,'b)sigseg = SS of (('a, 'b, Bigarray.c_layout) Bigarray.Array1.t * sample )

let joinsegs (SS(a,t1)) (SS(b,t2)) = 
  assert (t2 == t1 + Bigarray.Array1.dim a);
  SS(wsappend a b, t1)
let subseg (SS(a,st)) pos len = SS(Bigarray.Array1.sub a (pos - st) len, pos)
let nullseg t                 = SS(Bigarray.Array1.create t Bigarray.c_layout 0, 0)
let timebase ss               = 0
let toSigseg arr st tb        = SS(arr, st)
let toArray (SS(a,_))         = a
let width (SS(a,_))           = Bigarray.Array1.dim a
let ss_start (SS(_,s))        = s
let ss_end (SS(a,s))          = s + Bigarray.Array1.dim a - 1
let ss_get (SS(a,_)) i        = Bigarray.Array1.get a i

