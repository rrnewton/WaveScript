
(* These are things that MLton is lacking that SML/NJ has... *)

(* Look into using unsafe array ops: *)
fun arrayExtract (a, offset, len) = 
  Array.tabulate (len, fn i => Array.sub(a, offset+i))
(*
  if Array.length a = 0
  then raise "can't take subarray of empty array" else
  let (*val new = Array.make (len, Array.sub (a,0))*) 
  in     
  end 
*)

(* GREAT... copy doesn't work either. *)
(*    (Array.copy (a,i, new,0, len); new)*)
(*  Unsafe.Array.make (100,)*)
