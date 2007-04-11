


type sample = int  (* Should be int64 *)
(* Doesn't have timebase *)
(*type ('a,'b)sigseg = SSF of (('a,'b) wsarray * sample )  *)
type ('a,'b)sigseg_flat = SSF of (('a, 'b, Bigarray.c_layout) Bigarray.Array1.t * sample )

let joinsegs_flat (SSF(a,t1)) (SSF(b,t2)) = 
  assert (t2 == t1 + Bigarray.Array1.dim a);
  SSF(wsappend a b, t1)
let subseg_flat (SSF(a,st)) pos len = SSF(Bigarray.Array1.sub a (pos - st) len, pos)
let nullseg_flat t                 = SSF(Bigarray.Array1.create t Bigarray.c_layout 0, 0)
let timebase_flat ss               = 0
let toSigseg_flat arr st tb        = SSF(arr, st)
let toArray_flat (SSF(a,_))         = a
let width_flat (SSF(a,_))           = Bigarray.Array1.dim a
let ss_start_flat (SSF(_,s))        = s
let ss_end_flat (SSF(a,s))          = s + Bigarray.Array1.dim a - 1
let ss_get_flat (SSF(a,_)) i        = Bigarray.Array1.get a i

(* Annoyingly, we're duplicating everything for the two possible Array repsresentations *)

(*
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
  *)
