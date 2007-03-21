








type sample = int  (* Should be int64 *)
(* Doesn't have timebase *)
type 'a sigseg = SS of ('a array * sample * int )  


(* TODO: FINISH: *)

let joinsegs (SS(a,t1,w1)) (SS(b,t2,w2)) = 
  SS(a ,
     t1, w1+w2)

let subseg ss pos len = ss

let nullseg = SS([||],0,0)

let toSigseg arr st tb = SS(arr,st, Array.length arr)

let width (SS(_,_,w)) = w
let ss_start (SS(_,s,_)) = s
let ss_end (SS(_,s,_)) = s

(* Does nothing currently *)
let fft (SS(flarr, st, w)) = 
  let complexarr = Array.make (Array.length flarr) Complex.zero in
  for i = 0 to Array.length flarr do
    (*complexarr.(i) <- { Complex.re= flarr.(i); Complex.im = 0.0 };*)
    ()
  done;
  (SS(complexarr, st, w))
    
  
