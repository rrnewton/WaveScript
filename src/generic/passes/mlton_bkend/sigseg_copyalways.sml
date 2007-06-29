

structure SigSeg =
struct

type sample = Int64.int
(* Doesn't have timebase *)
datatype 'a sigseg = SS of ('a wsarray * sample )


fun joinsegs (SS(a,t1), SS(b,t2)) = 
  (assert (t2 = t1 + Int64.fromInt (Array.length a));
   SS(wsappend a b, t1))
fun width   (SS(a,_))            = Int32.fromInt (wslen a)
fun subseg  (SS(a,st), pos, len) = SS(wssub a (Int64.toInt (Int64.- (Int64.fromLarge (Int32.toLarge pos), st))) len,
                                      Int64.fromInt pos)
fun nullseg ()                   = SS(wsnull(), 0)
fun timebase ss                  = 0
fun toSigseg (arr, st, tb)       = SS(arr, Int64.fromInt st)
fun toArray  (SS(a,_))           = a
(* This should really return a WS Int64!! *)
fun ss_start (SS(_,s))           = Int32.fromInt (Int64.toInt s)
fun ss_end   (SS(a,s))           = Int32.fromInt (Int64.toInt (s + Int64.fromInt (wslen a - 1)))
fun ss_get   (SS(a,_), i)        = wsget a i

end

open SigSeg
