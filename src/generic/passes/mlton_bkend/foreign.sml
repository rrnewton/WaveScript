
(* These are primitives implemented in C *)
(* This is not standard SML. It's MLton specific.  *)

val raw_fftR2C = _import "raw_fftR2C" : (Real32.real array * Word64.word array * int) -> unit;

(* This uses FFTW "measurement" to get a better FFT.  It caches the plan. *)
(* val memoized_fftR2C = _import "memoized_fftR2C" : (Real32.real array * Word64.word array * int) -> unit *)
val memoized_fftR2C = _import "memoized_fftR2C" : (Real32.real array * int) -> MLton.Pointer.t;

(*
val memoized_fftR2C = _import "memoized_fftR2C" : (Real32.real array * int) -> unit; 
val set_cached_plan = _import "set_cached_plan" : (Word64.word array * int) -> unit; 
*)
