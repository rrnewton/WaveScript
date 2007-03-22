




(*
(* Does nothing currently *)
let fft (SS(flarr, st, w)) = 
  let complexarr = make (length flarr / 2 + 1) Complex.zero in
  for i = 0 to length flarr do
    (*complexarr.(i) <- { Complex.re= flarr.(i); Complex.im = 0.0 };*)
    ()
  done;
  (SS(complexarr, st, w))
    *)
    
open Bigarray;;

let autofft = 
  let size = ref 1024 in
  let plan = ref (Fftw2.create Fftw2.forward !size) in 
    fun (arr : (Complex.t, complex64_elt, c_layout) Array1.t) ->
      if Array1.dim arr == !size
      then !plan arr
      else (size := Array1.dim arr;
	    plan := Fftw2.create Fftw2.forward !size;
	    !plan arr)
;;	      

(* Need to halve the size of the output!! *)
let fft (SS(flarr, st, w)) = 
  let bigarr = Array1.create complex64 c_layout (length flarr) in
  for i = 0 to length flarr do
    Array1.set bigarr i { Complex.re= flarr.(i); Complex.im = 0.0 }
  done;
    let result = autofft bigarr in
    (* Terrible conversions! *)
    (*let complexarr = make (length flarr / 2 + 1) Complex.zero in*)
    let complexarr = make (length flarr) Complex.zero in
      for i = 0 to length flarr do
	complexarr.(i) <- Array1.get result i;
      done;
      (SS(complexarr, st, w))
