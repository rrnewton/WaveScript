
(* This version includes an offset so that a sigseg needn't start at the first element of its first seg. 

   Similarly, it allows extra garbage at the end, so that a sigseg
   needn't end precisely at the end of its last seg.

   DEBUG CHECKS: there are many invariant checks below, both "asserts" and "checkseg" calls.
   Comment all of them out for best performance.

*)

structure SigSeg : SIGSEG =
struct

type sample = int  (* Should be int64 *)
type timebase = int

type 'a sigseg =  {
  dat : 'a array list,
  st  : sample,
  sz  : int,	     (* Total width of window *)
  off : int          (* Offset into the first segment. Allows more sharing. *)
}

open Array

fun slice arr offset len = 
  tabulate (len, fn i => sub(arr, offset + i))

fun sum [] = 0 | sum (h::t) = h + sum t
fun checkseg {dat,st,sz,off} = 
  (* Make sure nullsegs are in canonical form. *)
  if sz = 0 then assert (st=0) else    
  let val sm = sum (List.map length dat) 
  in
    if sm >= sz andalso length(hd dat) > off
    then ()
    else raise (SigSegFailure ("checkseg failed: expected width "^(Int.toString sz)^
                              " but found "^(Int.toString sm)))
  end 

fun nullseg  t                  = { dat=[], st=0, sz=0, off=0 }
fun timebase ss                 = 0
fun width    {dat,st,sz,off}    = sz
fun ss_start {dat,st,sz,off}    = st
fun ss_end   {dat,st,sz,off}    = st + sz - 1
(* fun toSigseg (arr, st, tb)      = { dat=[Array.vector arr], st=st, sz=Array.length arr, off=0 } *)
fun toSigseg (arr, st, tb)      = { dat=[arr], st=st, sz=length arr, off=0 }

(* Doesn't cache result yet. *)
fun toArray {dat,st,sz,off} = 
    (* array version *)
    if sz = 0 then Array.fromList [] else
(*    if null (tl dat) then hd dat else*)

    (* Here's a potential optimization: *)
    (* We don't necessarily copy.  But you'd better not mutate!! *)    
    if off=0 andalso null(tl dat) andalso sz = Array.length (hd dat) then hd dat else  
    let  
(*         val _ = checkseg {dat=dat,off=off,sz=sz,st=st} *)
	(* Should use unsafe make array here *)
        val newarr = Array.array(sz, sub (hd dat,0))
        fun loop ls i j = 
	 if i = sz       then newarr else
         case ls 
          of  [] => raise (SigSegFailure "toArray not enough data in sigseg")
           | h::t => 	  
	    if j = length h then loop t i 0 else
            (Array.update (newarr, i, sub(h,j));
	     loop (h::t) (i+1) (j+1))
    in loop dat 0 off end
(*(Array.copyVec {di= i, dst= newarr, src= hd ls};
	        loop (tl ls) (i + length (hd ls)))*)
(*
    let val vec = concat ls
    in Array.tabulate (w, fn i => sub (vec,i)) end
*)

(* Improper bounds checking currently!! *)
(* Should check agaainst 'sz' *)
fun ss_get ({dat,off,sz,st}, ind) = 
  let 
(*     val _ = checkseg {dat=dat,off=off,sz=sz,st=st} *)
(*     val _ = assert (i < sz) *)
    fun loop ls i =
      case ls 
       of  [] => raise (SigSegFailure ("ss_get out of bounds ref " ^ Int.toString ind))
        | h::t => 
            let val hlen = length h in
	    if i < hlen
	    then sub (h, i)
	    else loop t (i - hlen) end
  in loop dat (ind + off) end

(* This makes sure that offset=0 *)
(* Returns just the data list *)
fun reallocFirstChunk {dat, st, sz, off} =
  let 
(*       val _ = checkseg {dat=dat, st=st, sz=sz, off=off} *)
  in 
    if off = 0 orelse sz = 0
    then dat
    else slice (hd dat) off (length (hd dat) - off) :: tl dat
  end

(* Here's where we may need to reallocate the first segment of B if it's offset is not zero *)
fun joinsegs (a, b) =  
  let
     val {dat=d1, st=s1, sz=z1, off=off1}  = a
     val {dat=_,  st=s2, sz=z2, off=off2} = b
(*      val _ = checkseg a *)
(*      val _ = checkseg b *)
  in
  if z1 = 0 then b else
  if z2 = 0 then a else
  let 
      val _ = if s2 = s1 + z1 then ()
             else raise (SigSegFailure ("segments do not line up: start1= "
	                      ^Int.toString s1^ " size1= "^Int.toString z1^
 		             " and start2= "^Int.toString s2))
      val d2 = reallocFirstChunk b
      val result = { dat = d1@d2,
                     st  = s1,
                     sz  = z1+z2,
                     off = off1 }
(*       val _ = checkseg result *)
  in
     result
  end 
  end

(* We need to rebuild the list of segments to avoid accumulating arbitrary amounts of garbage. *)
fun subseg (ss, pos, len) = 
  if len = 0 then nullseg() else
  let 
(*     val _ = checkseg ss *)
    val {dat, st, sz, off} = ss 

(*     val _ = print ("  shaving off from size: "^Int.toString sz^"\n") *)

    val _ = assert (pos - st + len <= sz);

    (* Second, chop off the tail that we need *)
    fun loop2 ls cnt = 
     case ls  
      of   [] => raise (SigSegFailure "subseg.loop2 out of bounds")
       | h::t => 
          let val hlen = length h in
	  if cnt <= hlen      
          then [h]
	  else h :: loop2 t (cnt - hlen)
          end

    (* First forward to the start *)
    fun loop ls i =
     case ls of
         [] => raise (SigSegFailure ("subseg.loop1 out of bounds: requested pos " 
	                             ^Int.toString pos^ " len "^Int.toString len^
				     " from sigseg of len "^Int.toString sz^" pos "^Int.toString st))
      | h::t => 
	  let val hlen = length h 
(* 	      val _ = print (" head segment length: "^Int.toString hlen^"\n") *)
	  in
	    if i < hlen
	    then (if i+len <= hlen
	          then [h]
		  else h :: loop2 t (len - (hlen - i)),
		  i) (* new offset *)
            else loop t (i - hlen)
          end

    val (dat2,off2) = loop dat (off + pos - st)
    
(*     val _ = print ("New offset! "^Int.toString off2^" old was "^Int.toString off^"\n") *)

    val result = { dat= dat2, st=pos, sz=len, off=off2 }
(*     val _ = checkseg result *)
  in result end


(* For now we're trying to keep the CANONICAL representation for nullsegs. *)
(* However, this definition doesn't enforce canonical nullsegs. *)
fun eq f (a,b) = 
 let
(*      val _ = checkseg a   *)
(*      val _ = checkseg b  *)
     val {dat=d1, st=st1, sz=w1, off=off1} = a
     val {dat=d2, st=st2, sz=w2, off=off2} = b
 in
     w1 = w2  andalso 
    (w1 = 0 orelse      (* nullsegs are always equal *)
     st1 = st2 andalso     
(*      if w1 = 0 then true else  *)
     let fun loop (v1,v2) (ls1,ls2) (i1,i2) cnt = 
         if cnt = 0 then true         
	 else if i1 = length v1
         then loop (hd ls1, v2) (tl ls1, ls2) (0,i2) cnt 
	 else if i2 = length v2
         then loop (v1, hd ls2) (ls1, tl ls2) (i1,0) cnt 
	 else f(sub (v1,i1), sub (v2,i2)) andalso 
	      loop (v1,v2) (ls1,ls2) (i1+1, i2+1) (cnt-1)
     in loop (hd d1, hd d2) (tl d1, tl d2) (off1,off2) w1 end)
 end


end (* End structure *)

open SigSeg


(*
let concatArray1 ls w kind = 
  let newarr = Array1.create kind c_layout w in
  let rec loop ls ind = 
    (*Printf.printf "concat loop len ls %d ind %d recorded width %d\n" (List.length ls) ind w;*)
    match ls with 
      | [] -> newarr
      | h::t -> 
	  let len = Array1.dim h in
	    (*Printf.printf "  length of this chunk: %d\n" len;*)
          for i = 0 to len-1 do 
	    Array1.set newarr (ind+i) (Array1.get h i)
	  done;
	  loop t (ind + len)
  in 
    loop ls 0

(* Doesn't cache result yet. *)
let toArray ss = 
  match ss.data with 
    | [] -> raise (Failure "can't toArray a null sigseg")
    | h::t -> concatArray1 (h::t) ss.size (Array1.kind h)
*)
