



    
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
let fft (sigseg : float sigseg) : Complex.t sigseg  = 
  let flarr = toArray sigseg in
  let newsize = (length flarr / 2) + 1 in 
  let bigarr = Array1.create complex64 c_layout (length flarr) in
  for i = 0 to length flarr - 1 do
    Array1.set bigarr i { Complex.re= flarr.(i); Complex.im = 0.0 };
  done;
    let result = autofft bigarr in
    (* Terrible conversions! *)
    (*let complexarr = make (length flarr / 2 + 1) Complex.zero in*)
    let complexarr = make newsize Complex.zero in
      for i = 0 to newsize-1 do
	complexarr.(i) <- Array1.get result i;

      done;
      (* Start is zero for this frequency domain array: *)
      toSigseg complexarr 0 3333
