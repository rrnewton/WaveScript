








type sample = int  (* Should be int64 *)
(* Doesn't have timebase *)
type 'a sigseg = SS of ('a array * sample * int )  


let joinseg (SS(a,t1,w1)) (SS(b,t2,w2)) = 
  SS(a ,
     t1, w1+w2)
;;


