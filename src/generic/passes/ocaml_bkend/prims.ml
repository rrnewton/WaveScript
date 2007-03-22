


(* Does nothing currently *)
let fft (SS(flarr, st, w)) = 
  let complexarr = make (length flarr / 2 + 1) Complex.zero in
  for i = 0 to length flarr do
    (*complexarr.(i) <- { Complex.re= flarr.(i); Complex.im = 0.0 };*)
    ()
  done;
  (SS(complexarr, st, w))
    
