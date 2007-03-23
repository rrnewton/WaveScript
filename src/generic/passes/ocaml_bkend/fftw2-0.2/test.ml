(* Testing the FFTW interface *)

open Bigarray
open Format
module FFT = Fftw2
module C = Complex

(* Printing *)

let print_complex z =
  if z.C.im = 0. then printf "%g" z.C.re
  else if z.C.re = 0. then printf "%gI" z.C.im
  else printf "%g%+gI" z.C.re z.C.im

let print_float x = printf "%g" x (* override Pervasive def *)

let print_matrix_2d print_elt (m: (_,_,fortran_layout) Array2.t) =
  printf "[| @[";
  for i = 1 to Array2.dim1 m do
    printf "@[<2>";
    for j = 1 to Array2.dim2 m do
      print_elt m.{i,j};
      printf " "
    done;
    printf "@]";
    if i < Array2.dim1 m then printf "@\n";
  done;
  printf "|]@]@\n"


(* One dimensional transform *)

let n = 16
let fft = FFT.create FFT.forward n
and ifft = FFT.create FFT.backward ~normalize:true n

let () =
  let a = Array1.create complex64 c_layout n in
  for i = 0 to n - 1 do
    a.{i} <- { C.re = 1.; im = 0. }
  done;
  let fa = fft a in
  let ifa = ifft fa in
  printf "a^ = @[[| ";
  for i = 0 to n - 1 do print_complex fa.{i}; printf "@ " done;
  printf "|]@]@\na^^ = @[[| ";
  for i = 0 to n - 1 do print_complex ifa.{i}; printf "@ " done;
  printf "|]@]@\n"

let () =
  let l = FFT.wisdom_export_fold (fun c (i,l) -> (i+1, l @ [c])) (0,[]) in
  printf "Wisdom = %s = " (FFT.wisdom_to_string());
  Gc.full_major();
  List.iter (fun c -> printf "%c" c) (snd l); printf "@\n";
  FFT.wisdom_from_string "(FFTW-2.1.3 (23 4 garbage))"


(* Multidimensional complex transforms *)

let n = 5
let fft = FFT.nd_create FFT.forward fortran_layout [|n; n|]
and ifft = FFT.nd_create FFT.backward ~normalize:true fortran_layout [|n; n|]

(* Pack the matrices -- using the last coordinate to index them. *)
let fft a = array3_of_genarray(fft ~index_dims:1 (genarray_of_array3 a))
let ifft a = array3_of_genarray(ifft ~index_dims:1 (genarray_of_array3 a))

let () =
  printf "--- Multidimensional complex transforms ---@\n";
  let m = Array3.create complex64 fortran_layout n n 2 in
  for no = 1 to 2 do
    for i1 = 1 to n do
      for i2 = 1 to n do
	m.{i1,i2,no} <- { C.re=float(no+1); im=0.}
      done
    done
  done;
  for no = 1 to 2 do
    printf "m.{_,_,%i} = " no;
    print_matrix_2d  print_complex(Array3.slice_right_2 m no)
  done;
  let fm = fft m in
  for no = 1 to 2 do
    printf "m^.{_,_,%i} = " no;
    print_matrix_2d print_complex (Array3.slice_right_2 fm no)
  done;
  let ffm = ifft fm in
  for no = 1 to 2 do
    printf "m^^.{_,_,%i} = " no;
    print_matrix_2d print_complex (Array3.slice_right_2 ffm no)
  done


(* Multidimensional real transforms *)

let rfft = FFT.rnd_create FFT.forward fortran_layout [|n; n|]
and irfft = FFT.rnd_create FFT.backward ~normalize:true fortran_layout [|n; n|]

let () =
  (* Pack the matrices -- using the last coordinate to index them. *)
  let rfft a = array3_of_genarray(rfft ~index_dims:1 (genarray_of_array3 a))
  and irfft a = array3_of_genarray(irfft ~index_dims:1 (genarray_of_array3 a))
  in
  printf "--- Multidimensional real transforms ---@\n";
  let m = Array3.create float64 fortran_layout n n 2 in
  for no = 1 to 2 do
    for i1 = 1 to n do
      for i2 = 1 to n do
	m.{i1,i2,no} <- float(no + 1)
      done
    done
  done;
  for no = 1 to 2 do
    printf "m.{_,_,%i} = " no;
    print_matrix_2d print_float (Array3.slice_right_2 m no)
  done;
  let fm = rfft m in
  for no = 1 to 2 do
    printf "m^.{_,_,%i} = " no;
    print_matrix_2d print_complex (Array3.slice_right_2 fm no)
  done;
  (* BEWARE that this inverse transform will destroy [fm]. *)
  let ffm = irfft fm in
  for no = 1 to 2 do
    printf "m^^.{_,_,%i} = " no;
    print_matrix_2d print_float (Array3.slice_right_2 ffm no)
  done
