
(* In this version sigsegs are lists of individual segments. 

   In this variant the sigsegs are "tight", there's no extra storage
   at the beginning or end of the seglist.  This means that we still
   have to reallocate the segments at the start and end.
   (The end is really unnecessary.)

   This is just an intermediate step.  It probably won't offer much performance benefit.
   In the "_wsharing" version we are more tolerant of garbage and reallocate less.
      
   We go ahead and use SML's vectors... since these are immutable afterall.
   I don't see any performance difference either way.

   DEBUG CHECKS: there are many invariant checks below, both "asserts" and "checkseg" calls.
   Comment all of them out for best performance.
*)

exception SigSegFailure of string

structure SigSeg : SIGSEG =
struct

type sample = int  (* Should be int64 *)
type timebase = int

(* Doesn't have timebase *)
(* seglist, start sample, element count *)
datatype 't sigseg = SS of ('t vector list * sample * int)

open Vector
(*open Array*)

fun slice arr offset len = 
  tabulate (len, fn i => sub(arr, offset + i))

(* This checks the invariants on a sigseg*)
fun sum [] = 0 | sum (h::t) = h + sum t
fun checkseg (SS(ls,st,w)) = 
  let val sm = sum (List.map length ls) in
    if sm = w
    then ()
    else raise (SigSegFailure ("checkseg failed: expected width "^(Int.toString w)^
                              " but found "^(Int.toString sm)))
  end 

fun nullseg()             = SS([],0,0)
fun timebase ss           = 0
fun width    (SS(_,_,w))  = w
fun ss_start (SS(_,s,_))  = s (* Should perhaps return error for nullseg? *)
fun ss_end   (SS(_,s,w))  = s + w - 1
fun toSigseg (arr, st, tb) = SS([Array.vector arr], st, Array.length arr)
(* fun toSigseg (arr, st, tb) = SS([arr], st, Array.length arr) *)

(* Doesn't cache result yet. *)
fun toArray (SS(ls,s,w)) = 
(*
    (* array version *)
    if w=0 then fromList [] else
    let val newarr = array(w, sub (hd ls,0))
        fun loop ls i = 
	  if List.null ls
          then newarr
          else (copy {di= i, dst= newarr, src= hd ls};
	        loop (tl ls) (i + length (hd ls)))
    in loop ls 0 end
*)
    let val vec = concat ls
    in Array.tabulate (w, fn i => sub (vec,i)) end

(* Doesn't do full bounds check *)
fun ss_get (SS(ls,_,w), i) = 
  let 
(*    val _ = assert (i < w) *)
   fun loop ls i =
    case ls 
     of   [] => raise (SigSegFailure "ss_get out of bounds ref")
      | h::t => 
	  if i < length h
	  then sub (h,i)
	  else loop t (i - length h)
  in loop ls i end

fun joinsegs (SS(a,t1,w1), SS(b,t2,w2)) = 
  if w1 = 0 then SS(b,t2,w2) else
  if w2 = 0 then SS(a,t1,w1) else
  let 
      val _ = assert (t2 = t1 + w1)
(*       val _ = checkseg (SS(a,t1,w1)) *)
(*       val _ = checkseg (SS(b,t2,w2)) *)
      val result = SS(a @ b, t1, w1+w2)
(*       val _ = checkseg result *)
  in 
    result
  end

(* Can return nullseg... but that loses sample number! *)
fun subseg (SS(ls,st,w), pos, len) = 
  if len = 0 then nullseg() else
  let 
(*    val _ = checkseg(SS(ls,st,w)) *)
(*    val _ = print (" subseg: Ok on the way in, want len "^(Int.toString len)^" pos "^(Int.toString pos)^"\n") *)

    (* Second, chop off the tail that we need *)
   fun loop2 ls cnt = 
    case ls  
     of   [] => raise (SigSegFailure "subseg.loop12 subseg out of bounds")
      | h::t => 
          let val hlen = length h in
	  if cnt < hlen      then [slice h 0 cnt]
	  (* We will fairly frequently cut off precisely to the end: *)
	  else if cnt = hlen then [h]
	  else h :: loop2 t (cnt - hlen)
          end

   (* First, scroll forward till the start of our subseg *)
   fun loop ls i =
    case ls 
     of   [] => raise (SigSegFailure "subseg.loop1  subseg out of bounds")
      | h::t => 
          let val hlen = length h in
	  if i < hlen
	  then (if i+len <= hlen
		then [slice h i len]
		else (slice h i (hlen - i) ::
		      loop2 t (len - (hlen - i))))
	  else loop t (i - hlen) end

   val result = SS(loop ls (pos-st), pos, len)
(*    val _ = checkseg result *)
  in 
    result
  end 

fun eq f (SS(ls1,st1,w1), SS(ls2,st2,w2)) = 
 let 
(*    val _ = checkseg (SS(ls1,st1,w1)) *)
(*    val _ = checkseg (SS(ls2,st2,w2)) *)
 in 
     w1 = w2  andalso 
    (w1 = 0 orelse      (* nullsegs are always equal *)
     st1 = st2 andalso     
     if w1 = 0 then true else 
     let fun loop (v1,v2) (ls1,ls2) (i1,i2) cnt = 
         if cnt = 0 then true         
	 else if i1 = length v1
         then loop (hd ls1, v2) (tl ls1, ls2) (0,i2) cnt 
	 else if i2 = length v2
         then loop (v1, hd ls2) (ls1, tl ls2) (i1,0) cnt 
	 else f(sub (v1,i1), sub (v2,i2)) andalso 
	      loop (v1,v2) (ls1,ls2) (i1+1, i2+1) (cnt-1)
     in loop (hd ls1, hd ls2) (tl ls1, tl ls2) (0,0) w1 end)
  end

end

open SigSeg
