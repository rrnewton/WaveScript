
(* Trying to figure out how to invoke fftw2.cma *)

open Fftw2;;
open Bigarray;;

Printf.printf "Trying fftw...\n";;

let myfft (*:   
    (Complex.t, complex64_elt, 'f) Array1.t ->
    (Complex.t, complex64_elt, 'f) Array1.t*)
  = create forward 1024;;

let arr = Array1.create complex64 c_layout 1024;;

let x = myfft arr;;

Printf.printf "Well, got an fft fun it looks like\n";;

let y = Array1.get x 37;;

open Complex;;
Printf.printf "Here's a complex num: %f + %f i\n" y.re y.im;;


let autofft = 
  let size = ref 1024 in
  let plan = ref (create forward !size) in 
    fun (arr : (Complex.t, complex64_elt, c_layout) Array1.t) ->
      if Array1.dim arr == !size
      then !plan arr
      else (size := Array1.dim arr;
	    plan := create forward !size;
	    !plan arr)
;;	      
