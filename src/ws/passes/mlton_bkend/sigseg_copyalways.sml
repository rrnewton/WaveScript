

(* Would using vectors help? *)

structure SigSeg : SIGSEG =
struct

type sample = Int64.int
type timebase = Int.int

(* Doesn't have timebase!! *)
datatype 'a sigseg = SS of ('a wsarray * sample )

fun nullTimebase () = 0
fun newTimebase n   = n

fun joinsegs (SS(a,t1), SS(b,t2)) = 
  (assert (t2 = t1 + Int64.fromInt (Array.length a));
   SS(wsappend a b, t1))
fun width   (SS(a,_))            = Int32.fromInt (wslen a)
fun nullseg ()                   = SS(wsnull(), 0)
(* val nullseg                      = SS(Array.fromList [], 0) *)
fun timebase ss                  = 0
fun toSigseg (arr, st, tb)       = SS(arr, st)
fun toArray  (SS(a,_))           = a
(* This should really return a WS Int64!! *)
fun ss_start (SS(_,s))           = s
fun ss_end   (SS(a,s))           = (s + Int64.fromInt (wslen a - 1))
fun ss_get   (SS(a,_), i)        = wsget a i

(* NOTE! This only works if the contents is an equality type! *)
fun eq subeq (SS(a1,s1), SS(a2,s2)) = 
    (
(*
     print ("Comparing "^ (Int64.toString s1) ^" and "^ (Int64.toString s2) ^"\n");
     print ("widths "^ (Int.toString (Array.length a1)) ^" and "^ (Int.toString (Array.length a2)) ^"\n");
     print ("array comp: "^ Bool.toString (a1 = a2) ^"\n");
*)
     s1 = s2 andalso 
     arrayEqual subeq (a1, a2)
     )

(* Can return nullseg... but that loses sample information! *)
fun subseg  (SS(a,st), pos, len) = 
  if len = 0 then nullseg() else
  let
    open Int64
    val start = toInt (Int64.- (pos, st))
(*    val _ = print ("  subseg arrlen "^(Int.toString (Array.length a))^" st "^(toString st)^" pos "^(toString pos2)^" \n")
    val _ = print ("  requested len "^(Int.toString len)^" seg width: "^(Int.toString (width (SS(a,st))))^"\n")*)
  in
    SS(wssub a start len, pos)
  end

end

open SigSeg
