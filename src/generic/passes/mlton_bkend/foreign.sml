
(* These are primitives implemented in C *)
(* This is not standard SML. It's MLton specific.  *)

val raw_fftR2C = _import "raw_fftR2C" : (Real32.real array * Word64.word array * int) -> unit;
