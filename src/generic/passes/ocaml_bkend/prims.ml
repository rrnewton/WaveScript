



    
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
let fft (sigseg : (float, Bigarray.float64_elt) sigseg) : (Complex.t, Bigarray.complex64_elt) sigseg = 
  let flarr = toArray sigseg in
  let newsize = (wslen flarr / 2) + 1 in 
  let bigarr = Array1.create complex64 c_layout (wslen flarr) in
  for i = 0 to wslen flarr - 1 do
    Array1.set bigarr i { Complex.re= wsget flarr i; Complex.im = 0.0 };
  done;
    let result = autofft bigarr in
    (* Terrible conversions! *)
    let complexarr = wsmakearr complex64  newsize Complex.zero in
      for i = 0 to newsize-1 do
	wsset complexarr i  (Array1.get result i);

      done;
      (* Start is zero for this frequency domain array: *)
      toSigseg complexarr 0 3333


(* If our arrays were already bigarrays... well the we'd still need to
   copy to get the half of the output that we want. 
   Wouldn't have to copy on input...*)


