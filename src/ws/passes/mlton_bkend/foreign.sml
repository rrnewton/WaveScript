
(* These are primitives implemented in C *)
(* This is not standard SML. It's MLton specific.  *)

val raw_fftR2C  = _import "raw_fftR2C"  : (Real32.real array * Word64.word array * int) -> unit;
val raw_ifftC2R = _import "raw_ifftC2R" : (Word64.word array * Real32.real array * int) -> unit;

(* This uses FFTW "measurement" to get a better FFT.  It caches the plan. *)
(* This uses the same output buffer every time. Returns a machine
pointer to that output buffer. *)
(* val memoized_fftR2C = _import "memoized_fftR2C" : (Real32.real array * Word64.word array * int) -> unit *)
val memoized_fftR2C = _import "memoized_fftR2C" : (Real32.real array * int) -> MLton.Pointer.t;

(*
val memoized_fftR2C = _import "memoized_fftR2C" : (Real32.real array * int) -> unit; 
val set_cached_plan = _import "set_cached_plan" : (Word64.word array * int) -> unit; 
*)

(* Calls what we need to start up the ensbox. 
   After this we just wait for the callback. *)
val init_ensbox = _import "init_ensbox" : unit -> unit;

(* This is the entrypoint for binary data from the ensbox hardware. *)
val ensbox_entry = _export "wsmlton_entry" : (MLton.Pointer.t * int -> unit) -> unit;
(*val _ = entry (fn (p,n) => ())*)
