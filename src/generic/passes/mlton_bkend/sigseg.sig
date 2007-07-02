
(* 

NOTE ON NULLSEG:

Currently the semantics of "null" segments (width = 0) is that they
lose their sample number information.  Any two null segments are equal
to eachother.  Any null segment can be joined on the left or right
with any other null segment.  Subseg can return a null segment.

*)


signature SIGSEG =
sig
  type sample
  type 'a sigseg
  type timebase

(*  datatype 'a sigseg = SS of ('a wsarray * sample )*)

  val width     : 'a sigseg                         -> Int32.int
  val timebase  : 'a sigseg                         -> timebase 
  val joinsegs  : 'a sigseg * 'a sigseg             -> 'a sigseg

  (* This should at least take a double, if not Int64! *)
  val subseg    : 'a sigseg * Int32.int * Int32.int -> 'a sigseg
  val toSigseg  : 'a array  * Int32.int * timebase  -> 'a sigseg
  val toArray   : 'a sigseg                         -> 'a array
  val nullseg   : unit                              -> 'a sigseg

(*  val ss_start  : 'a sigseg                         -> sample
  val ss_end    : 'a sigseg                         -> sample*)
  val ss_start  : 'a sigseg                         -> Int32.int
  val ss_end    : 'a sigseg                         -> Int32.int
  val ss_get    : 'a sigseg * Int32.int             -> 'a

  val eq        : ('a * 'a -> bool) -> 'a sigseg * 'a sigseg -> bool

end
