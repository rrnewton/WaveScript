
(* In this version sigsegs are lists of individual segments. 
   This is better for rewindowing (joinsegs and subseg). 
   
   We go ahead and use SML's vectors... since these are immutable afterall.
   I don't see any performance difference either way.
*)

structure SigSeg : SIGSEG =
struct

type sample = int  (* Should be int64 *)
type timebase = int

exception SigSegFailure of string

(* Doesn't have timebase *)
(* seglist, start sample, element count *)
datatype 't sigseg = SS of ('t vector list * sample * int)

open Vector
(*open Array*)

fun mysub arr offset len = 
  tabulate (len, fn i => sub(arr, offset + i))


fun nullseg()             = SS([],0,0)
fun timebase ss           = 0
fun width    (SS(_,_,w))  = w
fun ss_start (SS(_,s,_))  = s
fun ss_end   (SS(_,s,w))  = s + w - 1
fun toSigseg (arr, st, tb) = SS([Array.vector arr], st, Array.length arr)
(* fun toSigseg (arr, st, tb) = SS([arr], st, Array.length arr) *)

fun ss_get (SS(ls,_,_), i) = 
  let fun loop ls i =
    case ls 
     of   [] => raise (SigSegFailure "ss_get out of bounds ref")
      | h::t => 
	  if i < length h
	  then sub (h,i)
	  else loop t (i - length h)
  in loop ls i end

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

fun joinsegs (SS(a,t1,w1), SS(b,t2,w2)) = 
  (assert (t2 = t1 + w1);
   SS(a @ b, t1, w1+w2))


fun subseg (SS(ls,st,w), pos, len) = 
  let 
  fun loop ls i =
    case ls 
     of   [] => raise (SigSegFailure "subseg out of bounds")
      | h::t => 
	  if i < length h
	  then (if i+len <= length h
		then [mysub h i len]
		else (mysub h i (length h - i) ::
		      loop2 t (i + len - length h)))
	  else loop t (i - length h)
  and loop2 ls j = 
    case ls  
     of   [] => raise (SigSegFailure "subseg out of bounds")
      | h::t => 
	  if j < length h
	  then [mysub h 0 j]
	  else loop t (j - length h)
  in 
    SS(loop ls (pos-st), pos, len)
  end 

end

open SigSeg
