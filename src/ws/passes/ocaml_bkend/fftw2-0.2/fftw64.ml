(* File: fftw64.ml

   Copyright (C) 2002 Christophe Troestler
   email: Christophe.Troestler@umh.ac.be
   WWW: http://www.umh.ac.be/math/an/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* 	$Id: fftw64.ml,v 1.7 2002/05/21 18:00:17 trch Exp $	 *)

(* FFTW version 2 (http://www.fftw.org/) interface for OCaml. *)


open Bigarray


let invalid_arg s = invalid_arg ("Fftw." ^ s)


(* Normalization of a matrix (division of all elements by n) *)

external normalize_1 :
  int -> (* n *)
  int -> (* howmany *)
  (Complex.t, complex64_elt, 'a) Array1.t -> (* m *)
  int -> (* mstride *)
  int -> (* mdist *)
  unit = "fftw_normalize_1"

external normalize_nd :
  int -> (* product of dims = total size *)
  int -> (* howmany *)
  (Complex.t, complex64_elt, 'a) Genarray.t -> (* m *)
  unit = "fftw_normalize_nd"

external normalize_r :
  int -> (* n *)
  int -> (* howmany *)
  (float, float64_elt, 'a) Array1.t -> (* m *)
  int -> (* mstride *)
  int -> (* mdist *)
  unit = "fftw_normalize_r"

external normalize_rnd :
  int -> (* size as fftw_real array *)
  int -> (* prodn *)
  int -> (* howmany matrices in the following bigarray *)
  ('a, 'b, 'c) Genarray.t -> (* bigarray, real or complex *)
  unit = "fftw_normalize_rnd"




(* HACK!  Bigarrays returning NULL as the data pointer.  Useful for
   work arrays for in-place transforms---instead of allocating
   anything with [Array1.create complex64 layout n]. *)

external alloc_null_bigarray : unit -> unit = "fftw_alloc_null_bigarray"

let () = alloc_null_bigarray()

external null_bigarray_1 :
  'c Bigarray.layout -> ('a, 'b, 'c) Bigarray.Array1.t =
   "fftw_get_null_bigarray"

external null_bigarray_nd :
  'c Bigarray.layout -> ('a, 'b, 'c) Bigarray.Genarray.t =
   "fftw_get_null_bigarray"

(* Direction: Forward & backward
 **********************************************************************)

(* The precise type definition is only useful for the multidimensional
   real transform.  For the other transforms, only the test is_forward
   is useful. *)

type ('a, 'b, 'c, 'd) dir = ('a, 'b) kind * ('c, 'd) kind

let forward = (float64, complex64)
and backward = (complex64, float64)

let is_backward (kind_in, kind_out) =
  (kind_in  = (Obj.magic complex64 : ('a, 'b) Bigarray.kind)) &&
  (kind_out = (Obj.magic float64 : ('c, 'd) Bigarray.kind))



(*
 * FFTW -- one dimensional complex transforms
 **********************************************************************)


type fftw_plan

external fftw_create_plan_wrapper : int -> bool -> bool -> bool -> bool ->
  fftw_plan = "fftw_create_plan_wrapper"

external fftw_create_plan_specific_wrapper :
  int -> bool -> bool -> bool -> bool ->
  (Complex.t, complex64_elt, 'a) Array1.t -> int ->
  (Complex.t, complex64_elt, 'a) Array1.t -> int -> fftw_plan
  = "fftw_create_plan_specific_wrapper_bc" "fftw_create_plan_specific_wrapper"

external fftw_wrapper : fftw_plan -> int ->
  (Complex.t, complex64_elt, 'a) Array1.t -> int -> int ->
  (Complex.t, complex64_elt, 'a) Array1.t -> int -> int -> unit
    = "fftw_wrapper_bc" "fftw_wrapper"


(* Wrappers of fftw *)

let fftw plan n  ?(howmany=1) ?out ?(ostride=1) ?(odist=n)
    ?(istride=1) ?(idist=n) i =
  let o, os, od = match out with
    | None -> Array1.create complex64 (Array1.layout i) (n * howmany), 1, n
    | Some(m) -> m, ostride, odist  in
  fftw_wrapper plan  howmany  i istride idist  o os od;
  o

let fftw_normalized plan n  ?(howmany=1) ?out ?(ostride=1) ?(odist=n)
    ?(istride=1) ?(idist=n) i =
  let o, os, od = match out with
    | None -> Array1.create complex64 (Array1.layout i) (n * howmany), 1, n
    | Some(m) -> m, ostride, odist  in
  fftw_wrapper plan  howmany  i istride idist  o os od;
  normalize_1 n howmany o os od;
  o

let fftw_in_place plan n  ?(howmany=1) ?work ?(istride=1) ?(idist=n) i =
  let work = match work with
    | None -> null_bigarray_1 (Array1.layout i)
    | Some(m) -> m
  in fftw_wrapper plan  howmany  i istride idist  work 0 0

let fftw_normalized_in_place plan n  ?(howmany=1) ?work
    ?(istride=1) ?(idist=n) i =
  let work = match work with
    | None -> null_bigarray_1 (Array1.layout i)
    | Some(m) -> m  in
  fftw_wrapper plan  howmany  i istride idist  work 0 0;
  normalize_1 n howmany i istride idist


(* Safe versions *)

let fftw_check_in_params fname n howmany  istride idist dim_i =
  (* check args and that the 'in' array is big enough*)
  if howmany < 1 then invalid_arg (fname ^ ": ~howmany < 1");
  if istride < 1 then invalid_arg (fname ^ ": ~istride < 1");
  if idist < 1 then invalid_arg (fname ^ ": ~idist < 1");
  if (n - 1) * istride + (howmany - 1) * idist >= dim_i then
    invalid_arg (fname ^ ": input array too small")



let get_out_array n howmany out ostride odist layout =
  match out with
  | None ->
      (Array1.create complex64 layout (n * howmany), 1, n)
  | Some(m) ->
      (* Check out params & that the 'out' array is big enough *)
      if ostride < 1 then invalid_arg "create: ~ostride < 1";
      if odist < 1 then invalid_arg "create: ~odist < 1";
      if (n - 1) * ostride + (howmany - 1) * odist >= Array1.dim m then
	invalid_arg "create: output array ~out too small";
      (m, ostride, odist)

let fftw_safe plan n  ?(howmany=1) ?out ?(ostride=1) ?(odist=1)
  ?(istride=1) ?(idist=1) i =
  fftw_check_in_params "create" n howmany istride idist (Array1.dim i);
  let o, os, od = get_out_array n howmany out ostride odist (Array1.layout i) in
  fftw_wrapper plan  howmany  i istride idist  o os od;
  o

let fftw_safe_normalized plan n  ?(howmany=1) ?out ?(ostride=1) ?(odist=1)
    ?(istride=1) ?(idist=1) i =
  fftw_check_in_params "create" n howmany istride idist (Array1.dim i);
  let o, os, od = get_out_array n howmany out ostride odist (Array1.layout i) in
  fftw_wrapper plan  howmany  i istride idist  o os od;
  normalize_1 n howmany o os od;
  o


let get_work_array n layout work =
  match work with
  | None -> null_bigarray_1 layout
  | Some(m) -> (* Check the 'work' array is big enough *)
      if n <= Array1.dim m then m
      else invalid_arg "create_in_place: work array too small"

let fftw_safe_in_place plan n  ?(howmany=1) ?work
    ?(istride=1) ?(idist=1) i =
  fftw_check_in_params "create_in_place" n howmany istride idist (Array1.dim i);
  let work = get_work_array n (Array1.layout i) work in
  fftw_wrapper plan  howmany  i istride idist  work 1 1

let fftw_safe_normalized_in_place plan n  ?(howmany=1) ?work
    ?(istride=1) ?(idist=1) i =
  fftw_check_in_params "create_in_place" n howmany istride idist (Array1.dim i);
  let work = get_work_array n (Array1.layout i) work in
  fftw_wrapper plan  howmany  i istride idist  work 1 1;
  normalize_1 n howmany i istride idist


(* Create 1D FFT transforms *)

let check_matrix fname inout  m mstride n =
  if mstride < 1 then invalid_arg (fname ^ ": ~" ^ inout ^ "stride < 1");
  if (n-1) * mstride >= Array1.dim m then
    invalid_arg (fname ^ ": ~" ^ inout ^ " array too small")

let get_plan fname  n backward measure in_place use_wisdom
    i istride  o ostride =
  if n < 1 then invalid_arg (fname ^ ": input size < 1");
  match i, o with
  | None, None ->
      fftw_create_plan_wrapper n backward measure in_place use_wisdom
  | Some(im), Some(om) ->
      check_matrix fname "i" im istride n;
      check_matrix fname "o" om ostride n;
      fftw_create_plan_specific_wrapper n backward measure
        in_place use_wisdom  im istride  om ostride
  | _, _ ->
      invalid_arg (fname)


let create dir ?(measure=false) ?(use_wisdom=false) ?(unsafe=false)
    ?(normalize=false)  ?i ?(istride=1) ?o ?(ostride=1) n =
  let plan = get_plan "create" n (is_backward dir) measure
    (* in_place = *) false use_wisdom i istride  o ostride in
  match unsafe, normalize with
  | true, false -> fftw plan n
  | false, false -> fftw_safe plan n
  | true, true -> fftw_normalized plan n
  | false, true -> fftw_safe_normalized plan n


let create_in_place dir ?(measure=false) ?(use_wisdom=false)
    ?(unsafe=false) ?(normalize=false)  ?i ?(istride=1) ?o ?(ostride=1) n =
  let plan = get_plan "create_in_place" n (is_backward dir) measure
    (* in_place = *) true use_wisdom i istride  o ostride in
  match unsafe, normalize with
  | true, false -> fftw_in_place plan n
  | false, false -> fftw_safe_in_place plan n
  | true, true -> fftw_normalized_in_place plan n
  | false, true -> fftw_safe_normalized_in_place plan n



(*
 * FFTWND -- multidimensionnal complex transforms
 **********************************************************************)

type fftwnd_plan

external fftwnd_create_plan_wrapper :
  int array -> (* array of dimensions of the transform *)
  bool -> (* true -> forward | false -> backward *)
  bool -> (* measure *)
  bool -> (* in place *)
  bool -> (* use wisdom *)
  bool    (* fortran layout *)
  -> fftwnd_plan = "fftwnd_create_plan_wrapper_bc" "fftwnd_create_plan_wrapper"

external fftwnd_create_plan_specific_wrapper :
  int array -> bool -> bool -> bool -> bool -> bool ->
  (Complex.t, complex64_elt, 'a) Genarray.t -> int ->
  (Complex.t, complex64_elt, 'a) Genarray.t -> int -> fftwnd_plan
  = "fftwnd_create_plan_specific_wrapper_bc"
      "fftwnd_create_plan_specific_wrapper"

external fftwnd_wrapper : fftwnd_plan ->
  int -> (* howmany = the number of transforms rfftw will compute *)
  (Complex.t, complex64_elt, 'a) Genarray.t -> int -> int ->
  (Complex.t, complex64_elt, 'a) Genarray.t -> int -> int -> unit
  = "fftwnd_wrapper_bc" "fftwnd_wrapper"


(* Wrappers of fftwnd *)

(* Howmany transforms to compute -- several matrices packed in a
   single array. *)
let get_howmany_c index_dims i =
  let howmany = ref 1 in
  for d = 0 to index_dims - 1 do
    howmany := !howmany * (Genarray.nth_dim i d)
  done;
  !howmany

let get_howmany_fortran index_dims i =
  let howmany = ref 1
  and ndims = Genarray.num_dims i in
  for d = ndims - index_dims to ndims - 1 do
    howmany := !howmany * (Genarray.nth_dim i d)
  done;
  !howmany


let fftwnd plan prodn create get_howmany check_mat
    ?(index_dims=0) ?out i =
  let howmany = get_howmany index_dims i
  and o = match out with
    | None -> create (Genarray.dims i)
    | Some(m) -> m  in
  fftwnd_wrapper plan  howmany  i 1 prodn  o 1 prodn;
  o

let fftwnd_normalized plan prodn create get_howmany check_mat
    ?(index_dims=0) ?out i =
  let howmany = get_howmany index_dims i
  and o = match out with
    | None -> create (Genarray.dims i)
    | Some(m) -> m  in
  fftwnd_wrapper plan  howmany  i 1 prodn  o 1 prodn;
  normalize_nd prodn howmany o;
  o

let fftwnd_in_place plan prodn null_array get_howmany check_mat
    ?(index_dims=0) i =
  let howmany = get_howmany index_dims i in
  fftwnd_wrapper plan  howmany  i 1 prodn  null_array 1 1

let fftwnd_normalized_in_place plan prodn null_array get_howmany check_mat
    ?(index_dims=0) i =
  let howmany = get_howmany index_dims i in
  fftwnd_wrapper plan  howmany  i 1 prodn  null_array 1 1;
  normalize_nd prodn howmany i


(* Safe versions *)

let check_matrix_c fname n =
  let ln = Array.length n in
  fun inout index_dims m ->
    let ndims_m = Genarray.num_dims m in
    if index_dims + ln <> ndims_m then
      invalid_arg (fname ^ ": incorrect number of dims of " ^ inout ^ " array");
    for i = 0 to ln - 1 do
      if n.(i) <> Genarray.nth_dim m (index_dims + i) then
	invalid_arg (fname ^ ": incorrect " ^ string_of_int i
		      ^ "th dim of " ^ inout ^ " array")
    done

let check_matrix_fortran fname n =
  let ln = Array.length n in
  fun inout index_dims m ->
    let ndims_m = Genarray.num_dims m in
    if ln + index_dims <> ndims_m then
      invalid_arg (fname ^ ": incorrect number of dims of " ^ inout ^ " array");
    for i = 0 to ln - 1 do
      if n.(i) <> Genarray.nth_dim m i then
	invalid_arg (fname ^ ": incorrect " ^ string_of_int i
		      ^ "th dim of " ^ inout ^ " array")
    done



let fftwnd_safe plan prodn create get_howmany check_mat
    ?(index_dims=0) ?out i =
  if index_dims < 0 then invalid_arg("nd_create: ~index_dims < 0");
  check_mat "input" index_dims i;
  let howmany = get_howmany index_dims i
  and o = match out with
    | None -> create (Genarray.dims i)
    | Some(m) -> check_mat "~out" index_dims m;  m  in
  fftwnd_wrapper plan  howmany  i 1 prodn  o 1 prodn;
  o

let fftwnd_safe_normalized plan prodn create get_howmany check_mat
    ?(index_dims=0) ?out i =
  if index_dims < 0 then invalid_arg("nd_create: ~index_dims < 0");
  check_mat "input" index_dims i;
  let howmany = get_howmany index_dims i
  and o = match out with
    | None -> create (Genarray.dims i)
    | Some(m) -> check_mat "~out" index_dims m;  m  in
  fftwnd_wrapper plan  howmany  i 1 prodn  o 1 prodn;
  normalize_nd prodn howmany o;
  o

let fftwnd_safe_in_place plan prodn null_array get_howmany check_mat
    ?(index_dims=0) i =
  if index_dims < 0 then invalid_arg("nd_create_in_place: ~index_dims < 0");
  check_mat "input" index_dims i;
  let howmany = get_howmany index_dims i in
  fftwnd_wrapper plan  howmany  i 1 prodn  null_array 1 1

let fftwnd_safe_normalized_in_place plan prodn null_array get_howmany check_mat
    ?(index_dims=0) i =
  if index_dims < 0 then invalid_arg("nd_create_in_place: ~index_dims < 0");
  check_mat "input" index_dims i;
  let howmany = get_howmany index_dims i in
  fftwnd_wrapper plan  howmany  i 1 prodn  null_array 1 1;
  normalize_nd prodn howmany i


(* Create ND FFTW transforms *)

let get_prodn fname n =
  let p = ref 1 in
  for i = 0 to Array.length n - 1 do
    if n.(i) < 1 then invalid_arg (fname ^ ": invalid dimension "
                                    ^ string_of_int i);
    p := !p * n.(i)
  done;
  !p

let get_plan_nd fname n backward measure in_place use_wisdom
    want_f_layout check_mat  i o =
  match i, o with
  | None, None ->
      fftwnd_create_plan_wrapper n backward measure in_place
	use_wisdom want_f_layout
  | Some(im), Some(om) ->
      check_mat "i" 0 im;
      check_mat "o" 0 om;
      fftwnd_create_plan_specific_wrapper n backward measure in_place
	use_wisdom want_f_layout im (* istride = *) 1  om (* ostride = *) 1
  | _, _ ->
      invalid_arg (fname)


let nd_create  dir ?(measure=false) ?(use_wisdom=false) ?(unsafe=false)
    ?(normalize=false)    ?i ?o layout n =
  let prodn = get_prodn "nd_create" n
  and want_f_layout = (layout = (Obj.magic fortran_layout : _ layout)) in
  let create = Genarray.create complex64 layout
  and get_howmany =
    if want_f_layout then get_howmany_fortran else get_howmany_c
  and check_mat =
    (if want_f_layout then check_matrix_fortran else check_matrix_c)
      "nd_create" n in
  let plan = get_plan_nd "nd_create" n (is_backward dir) measure
    (* in_place = *) false use_wisdom want_f_layout check_mat i o
  in match unsafe, normalize with
  | true, false ->
      fftwnd plan prodn create get_howmany check_mat
  | false, false ->
      fftwnd_safe plan prodn create get_howmany check_mat
  | true, true ->
      fftwnd_normalized plan prodn create get_howmany check_mat
  | false, true ->
      fftwnd_safe_normalized plan prodn create get_howmany check_mat


let nd_create_in_place  dir ?(measure=false) ?(use_wisdom=false)
    ?(unsafe=false) ?(normalize=false)   ?i ?o layout n =
  let prodn = get_prodn "nd_create_in_place" n
  and want_f_layout = (layout = (Obj.magic fortran_layout : _ layout)) in
  let null_array = null_bigarray_nd layout
  and get_howmany =
    if want_f_layout then get_howmany_fortran else get_howmany_c
  and check_mat =
    (if want_f_layout then check_matrix_fortran else check_matrix_c)
      "nd_create_in_place" n in
  let plan = get_plan_nd "nd_create_in_place" n (is_backward dir) measure
    (* in_place = *) true use_wisdom want_f_layout check_mat i o
  in match unsafe, normalize with
  | true, false ->
      fftwnd_in_place plan prodn null_array get_howmany check_mat
  | false, false ->
      fftwnd_safe_in_place plan prodn null_array get_howmany check_mat
  | true, true ->
      fftwnd_normalized_in_place plan prodn null_array get_howmany check_mat
  | false, true ->
      fftwnd_safe_normalized_in_place
        plan prodn null_array get_howmany check_mat


(*
 * RFFTW -- real FFTW
 ***********************************************************************)

type rfftw_plan

external rfftw_create_plan_wrapper : int -> bool -> bool -> bool -> bool ->
  rfftw_plan = "rfftw_create_plan_wrapper"

external rfftw_create_plan_specific_wrapper :
  int -> bool -> bool -> bool -> bool ->
  (float, float64_elt, 'a) Array1.t -> int ->
  (float, float64_elt, 'a) Array1.t -> int -> rfftw_plan
  = "rfftw_create_plan_specific_wrapper_bc"
      "rfftw_create_plan_specific_wrapper"

external rfftw_wrapper : rfftw_plan -> int ->
  (float, float64_elt, 'a) Array1.t -> int -> int ->
  (float, float64_elt, 'a) Array1.t -> int -> int -> unit
    = "rfftw_wrapper_bc" "rfftw_wrapper"


(* Wrappers of rfftw *)

let rfftw plan n  ?(howmany=1) ?out ?(ostride=1) ?(odist=n)
    ?(istride=1) ?(idist=n) i =
  let o, os, od = match out with
    | None -> Array1.create float64 (Array1.layout i) (n * howmany), 1, n
    | Some(m) -> m, ostride, odist  in
  rfftw_wrapper plan  howmany  i istride idist  o os od;
  o

let rfftw_normalized plan n  ?(howmany=1) ?out ?(ostride=1) ?(odist=n)
    ?(istride=1) ?(idist=n) i =
  let o, os, od = match out with
    | None -> Array1.create float64 (Array1.layout i) (n * howmany), 1, n
    | Some(m) -> m, ostride, odist  in
  rfftw_wrapper plan  howmany  i istride idist  o os od;
  normalize_r n howmany o os od;
  o

let rfftw_in_place plan n  ?(howmany=1) ?work
    ?(istride=1) ?(idist=n) i =
  let work = match work with
    | None -> null_bigarray_1 (Array1.layout i)
    | Some(m) -> m  in
  rfftw_wrapper plan  howmany  i istride idist  work 0 0

let rfftw_normalized_in_place plan n  ?(howmany=1) ?work
    ?(istride=1) ?(idist=n) i =
  let work = match work with
    | None -> null_bigarray_1 (Array1.layout i)
    | Some(m) -> m  in
  rfftw_wrapper plan  howmany  i istride idist  work 0 0;
  normalize_r n howmany i istride idist


(* Safe versions *)

let r_get_out_array n howmany out ostride odist layout =
  match out with
  | None ->
      (Array1.create float64 layout (n * howmany), 1, n)
  | Some(m) ->
      (* Check out params & that the 'out' array is big enough *)
      if ostride < 1 then invalid_arg "create: ~ostride < 1";
      if odist < 1 then invalid_arg "create: ~odist < 1";
      if (n - 1) * ostride + (howmany - 1) * odist >= Array1.dim m then
	invalid_arg "create: output array ~out too small";
      (m, ostride, odist)

let rfftw_safe plan n  ?(howmany=1) ?out ?(ostride=1) ?(odist=1)
    ?(istride=1) ?(idist=1) i =
  fftw_check_in_params "r_create" n howmany istride idist (Array1.dim i);
  let o, os, od =
    r_get_out_array n howmany out ostride odist (Array1.layout i) in
  rfftw_wrapper plan  howmany  i istride idist  o os od;
  o

let rfftw_safe_normalized plan n  ?(howmany=1) ?out ?(ostride=1) ?(odist=1)
    ?(istride=1) ?(idist=1) i =
  fftw_check_in_params "r_create" n howmany istride idist (Array1.dim i);
  let o, os, od =
    r_get_out_array n howmany out ostride odist (Array1.layout i) in
  rfftw_wrapper plan  howmany  i istride idist  o os od;
  normalize_r n howmany o os od;
  o


let rfftw_safe_in_place plan n  ?(howmany=1) ?work
    ?(istride=1) ?(idist=1) i =
  fftw_check_in_params "r_create_in_place" n howmany
    istride idist (Array1.dim i);
  let work = get_work_array n (Array1.layout i) work in
  rfftw_wrapper plan  howmany  i istride idist  work 1 1

let rfftw_safe_normalized_in_place plan n  ?(howmany=1) ?work
    ?(istride=1) ?(idist=1) i =
  fftw_check_in_params "r_create_in_place" n howmany
    istride idist (Array1.dim i);
  let work = get_work_array n (Array1.layout i) work in
  rfftw_wrapper plan  howmany  i istride idist  work 1 1;
  normalize_r n howmany i istride idist




(* Create 1D real FFT transforms *)

let r_get_plan fname  n backward measure in_place use_wisdom
    i istride  o ostride =
  if n < 1 then invalid_arg (fname ^ ": input size < 1");
  match i, o with
  | None, None ->
      rfftw_create_plan_wrapper n backward measure in_place use_wisdom
  | Some(im), Some(om) ->
      check_matrix fname "i" im istride n;
      check_matrix fname "o" om ostride n;
      rfftw_create_plan_specific_wrapper n backward measure
	in_place use_wisdom  im istride  om ostride
  | _, _ ->
      invalid_arg (fname)


let r_create dir ?(measure=false) ?(use_wisdom=false) ?(unsafe=false)
    ?(normalize=false)  ?i ?(istride=1) ?o ?(ostride=1) n =
  let plan = r_get_plan "r_create" n (is_backward dir) measure
    (* in_place = *) false use_wisdom i istride  o ostride in
  match unsafe, normalize with
  | true, false -> rfftw plan n
  | false, false -> rfftw_safe plan n
  | true, true -> rfftw_normalized plan n
  | false, true -> rfftw_safe_normalized plan n


let r_create_in_place dir ?(measure=false) ?(use_wisdom=false)
    ?(unsafe=false) ?(normalize=false)  ?i ?(istride=1) ?o ?(ostride=1) n =
  let plan = r_get_plan "r_create_in_place" n (is_backward dir) measure
    (* in_place = *) true use_wisdom i istride  o ostride in
  match unsafe, normalize with
  | true, false -> rfftw_in_place plan n
  | false, false -> rfftw_safe_in_place plan n
  | true, true -> rfftw_normalized_in_place plan n
  | false, true -> rfftw_safe_normalized_in_place plan n


(*
 * RFFTWND -- multidimensionnal real transforms
 ***********************************************************************)

type ('a, 'b, 'c, 'd) rfftwnd_plan

external rfftwnd_create_plan_wrapper : int array ->
  bool -> bool -> bool -> bool -> bool ->
  ('a, 'b, 'c, 'd) rfftwnd_plan =
    "rfftwnd_create_plan_wrapper_bc" "rfftwnd_create_plan_wrapper"

external rfftwnd_real_to_complex_wrapper :
  ('a, 'b, 'c, 'd) rfftwnd_plan -> int ->
  ('a, 'b, 'e) Genarray.t -> int -> int ->
  ('c, 'd, 'e) Genarray.t -> int -> int -> unit =
    "rfftwnd_real_to_complex_wrapper_bc" "rfftwnd_real_to_complex_wrapper"
(* Can only be used with ('a, 'b, 'c, 'd) = (float, float64_elt,
   Complex.t, complex64_elt). *)

external rfftwnd_complex_to_real_wrapper :
  ('a, 'b, 'c, 'd) rfftwnd_plan -> int ->
  ('a, 'b, 'e) Genarray.t -> int -> int ->
  ('c, 'd, 'e) Genarray.t -> int -> int -> unit =
   "rfftwnd_complex_to_real_wrapper_bc" "rfftwnd_complex_to_real_wrapper"
(* Can only be used with ('a, 'b, 'c, 'd) = (Complex.t, complex64_elt,
   float, float64_elt). *)



let rnd_create dir ?(measure=false)
    ?(use_wisdom=false) ?(unsafe=false) ?(normalize=false) layout n =
  let _, kind_out = dir in
  let want_f_layout = (layout = (Obj.magic fortran_layout : _ layout))
  and complex_to_real = is_backward dir in
  let prodn = get_prodn "rnd_create" n
  and n_hermit = Array.copy n in
  let get_howmany =
    if want_f_layout then begin
      n_hermit.(0) <- n_hermit.(0)/2 + 1;
      get_howmany_fortran
    end
    else begin
      let l = Array.length n_hermit - 1 in
      n_hermit.(l) <- n_hermit.(l)/2 + 1;
      get_howmany_c
    end in
  let size_complex = get_prodn "rnd_create" n_hermit in
  let wrapper =
    if complex_to_real then rfftwnd_complex_to_real_wrapper
    else rfftwnd_real_to_complex_wrapper
  and plan =
    rfftwnd_create_plan_wrapper n complex_to_real measure
      (* in_place = *) false use_wisdom want_f_layout
  and dist_i, dist_o =
    if complex_to_real then (size_complex, prodn) else (prodn, size_complex)
  and create_out =
    (* create output matrix if needed.  We have to use the input array
       dimensions because several matrices may be packed together
       using additional dimensions. *)
    match want_f_layout, complex_to_real with
    | true, false ->
	let n0 = n_hermit.(0) in
	(fun dims -> dims.(0) <- n0; (* [dims]: real input array dimensions *)
          Genarray.create kind_out layout dims)
    | true, true ->
	let n0 = n.(0) in
	(fun dims -> dims.(0) <- n0; (* [dims]: complex input array dimensions *)
	  Genarray.create kind_out layout dims)
    | false, false ->
	let nd = n_hermit.(Array.length n_hermit - 1) in
	(fun dims -> dims.(Array.length dims - 1) <- nd;
	  Genarray.create kind_out layout dims)
    | false, true ->
	let nd = n.(Array.length n_hermit - 1) in
	(fun dims -> dims.(Array.length dims - 1) <- nd;
	  Genarray.create kind_out layout dims)
  and check_mat_in, check_mat_out =
    if unsafe then (fun _ _ _ -> ()), (fun _ _ _ -> ())
    else match want_f_layout, complex_to_real with
    | true, false ->
        check_matrix_fortran "rnd_create" n,
	check_matrix_fortran "rnd_create" n_hermit
    | true, true ->
        check_matrix_fortran "rnd_create" n_hermit,
	check_matrix_fortran "rnd_create" n
    | false, false ->
        check_matrix_c "rnd_create" n,
	check_matrix_c "rnd_create" n_hermit
    | false, true ->
        check_matrix_c "rnd_create" n_hermit,
	check_matrix_c "rnd_create" n
  and do_normalize =
    if normalize then
      normalize_rnd (if complex_to_real then prodn else 2*size_complex) prodn
    else
      fun howmany o -> ()
  in
  fun ?(index_dims=0) ?out i ->
    if index_dims < 0 then invalid_arg("rnd_create: ~index_dims < 0");
    check_mat_in "input" index_dims i;
    let howmany = get_howmany index_dims i
    and o =
      match out with
      | None -> create_out (Genarray.dims i)
      | Some(m) -> (check_mat_out "~out" index_dims m; m) in
    wrapper plan howmany  i 1 dist_i  o 1 dist_o;
    do_normalize howmany o;
    o




(*
 * Wisdom management
 ***********************************************************************)

(* Export *)

external fftw_export_wisdom_fold : 'a -> 'a = "fftw_export_wisdom_fold"

let wisdom_export_fold (f : char -> 'a -> 'a) (init : 'a) =
  Callback.register "Fftw wisdom emitter fold" f;
  fftw_export_wisdom_fold init


external fftw_export_wisdom_iter : unit -> unit = "fftw_export_wisdom_iter"

let wisdom_export_iter (f : char -> unit) =
  Callback.register "Fftw wisdom emitter iter" f;
  fftw_export_wisdom_fold ()

external wisdom_to_file : string -> unit
  = "fftw_export_wisdom_to_file_wrapper"

external wisdom_to_string : unit -> string
  = "fftw_export_wisdom_to_string_wrapper"

(* Import *)

external fftw_import_wisdom_fold : 'a -> 'a = "fftw_import_wisdom_fold"

let wisdom_import_fold (f : 'a -> 'a * char) (init :'a) =
  let () = Callback.register "Fftw wisdom get_input fold" f in
    fftw_import_wisdom_fold init


external fftw_import_wisdom_iter : unit -> unit = "fftw_import_wisdom_iter"

let wisdom_import_iter (f : unit -> char) =
  Callback.register "Fftw wisdom get_input iter" f;
  fftw_import_wisdom_iter ()

external wisdom_from_file : string -> unit
  = "fftw_import_wisdom_from_file_wrapper"

external wisdom_from_string : string -> unit
  = "fftw_import_wisdom_from_string_wrapper"

(* Delete *)

external wisdom_forget : unit -> unit = "fftw_forget_wisdom_wrapper"


(*
 * Precision
 ***********************************************************************)

let float = float64
and complex = complex64

type float_elt = float64_elt
type complex_elt = complex64_elt

