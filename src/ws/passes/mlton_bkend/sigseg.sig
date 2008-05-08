
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
  val joinsegs  : 'a sigseg * 'a sigseg             -> 'a sigseg

  val timebase      : 'a sigseg                     -> timebase 
  val nullTimebase  : unit                          -> timebase 
  val newTimebase   : int                           -> timebase 

  (* This should at least take a double, if not Int64! *)
  val subseg    : 'a sigseg * sample * Int32.int -> 'a sigseg
  val toSigseg  : 'a array  * sample * timebase  -> 'a sigseg
  val toArray   : 'a sigseg                         -> 'a array
  val nullseg   : unit                              -> 'a sigseg

(*  val ss_start  : 'a sigseg                         -> sample
  val ss_end    : 'a sigseg                         -> sample*)
  val ss_start  : 'a sigseg                         -> sample
  val ss_end    : 'a sigseg                         -> sample
  val ss_get    : 'a sigseg * Int32.int             -> 'a

  val eq        : ('a * 'a -> bool) -> 'a sigseg * 'a sigseg -> bool

end
