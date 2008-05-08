(** FFTW (version 2) interface for OCaml.

  FFTW (http://www.fftw.org/) is one of the fastest library to compute
  Discrete Fourier Transforms (DFT).  This module allows to use these
  routines to compute DFT of bigarrays.

  Typical usage:

{[let fft = Fftw2.create Fftw2.forward n
and ifft = Fftw2.create Fftw2.backward ~normalize:true n]}

  [let a = Array1.create Fftw2.complex c_layout n]

  Fill the matrix [a].

{[let fa = fft a
let ffa = ifft fa]}

  The explanations below assume you have read the manual for FFTW.
*)

open Bigarray


(** {6 Directions} *)

type ('a, 'b, 'c, 'd) dir

val forward : (float, float64_elt, Complex.t, complex64_elt) dir
val backward : (Complex.t, complex64_elt, float, float64_elt) dir
(**
  Allows to specify whether one wants a forward or backward transform.

  Note that the precise types of [forward] and [backward] are only
  relevant for {!Fftw2.rnd_create} for which the direction influence
  the kind of arrays for input and output.
*)



(** {6 One-dimensional complex transform} *)

val create :
  ('a, 'b, 'c, 'd) dir ->
  ?measure:bool -> ?use_wisdom:bool -> ?unsafe:bool -> ?normalize:bool ->
  ?i:(Complex.t, complex64_elt, 'e) Array1.t -> ?istride:int ->
  ?o:(Complex.t, complex64_elt, 'e) Array1.t -> ?ostride:int ->
  int ->
  (* [fft] *)
  ?howmany:int ->
  ?out:(Complex.t, complex64_elt, 'f) Array1.t -> ?ostride:int -> ?odist:int ->
  ?istride:int -> ?idist:int ->
  (Complex.t, complex64_elt, 'f) Array1.t ->
  (Complex.t, complex64_elt, 'f) Array1.t
(**
  [create dir <options> n] returns a function, say [fft], that computes
  the Discrete Fourier Transform (DFT) for one-dimensional bigarrays
  of size [n] in the direction specified by [dir] which can either be
  [forward] or [backward].  [<options>] represent the following
  optional arguments:

  [~measure] by default, the plan is created with FFTW_ESTIMATE. If
  this option is set [true], FFT_MEASURE is used --- which gives a
  faster [fft] function but takes some time to generate.

  [~use_wisdom]: if [true] use FFTW_USE_WISDOM (default: [false]).

  [~unsafe] by default, the [fft] function checks that the array
  passed is big enough.  Set this option to [true] if you want to
  disable the check (which results in faster [fft] but that can give
  segfaults).  Default is [false].

  [~normalize] by default, FFTW computes an unnormalized transform.
  If this option is set to [true], the output returned by FFTW is
  divided by [n].  Therefore [let fft = create forward n and ifft =
  create backward ~normalize:true n] satisfy [ifft(fft x) = x].

  [~i] and [~o] give arrays (of elements spaced by [istride] and
  [ostride] resp.) for which the plan is optimized.  The contents of
  these arrays are destroyed.

  The function [fft] returned takes one complex bigarray representing
  [howmany] one-dimensional arrays (according to [~istride] and
  [~idist]) and returns its DFT.  If one wishes to specify the output
  array, one can do so with the [~out] optional argument. The meanings
  of [~istride], [~idist], [~ostride], [~odist] are explained in the
  FFTW manual.  If an output array is not given, one is allocated
  automatically with [ostride = 1] and [odist = n].
*)


val create_in_place :
  ('a, 'b, 'c, 'd) dir ->
  ?measure:bool -> ?use_wisdom:bool -> ?unsafe:bool -> ?normalize:bool ->
  ?i:(Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Array1.t ->
  ?istride:int ->
  ?o:(Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Array1.t ->
  ?ostride:int ->
  int ->
  (* [fft] *)
  ?howmany:int ->
  ?work:(Complex.t, Bigarray.complex64_elt, 'f) Bigarray.Array1.t ->
  ?istride:int ->
  ?idist:int ->
  (Complex.t, Bigarray.complex64_elt, 'f) Bigarray.Array1.t -> unit
(** Same as {!Fftw2.create} but returns an in-place transform.  Note
  that, instead of prescribing an output array, one can give a work
  array to be used as a scratch space.  Its contents are destroyed.
*)


(** {6 N-dimensional complex transform} *)

val nd_create :
  ('a, 'b, 'c, 'd) dir ->
  ?measure:bool -> ?use_wisdom:bool -> ?unsafe:bool -> ?normalize:bool ->
  ?i:(Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Genarray.t ->
  ?o:(Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Genarray.t ->
  'e Bigarray.layout ->
  int array ->
  (* [fft] *)
  ?index_dims:int ->
  ?out:(Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Genarray.t ->
  (Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Genarray.t ->
  (Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Genarray.t
(**
  [nd_create dir <options> layout n] returns a function, say [fft],
  that computes the Discrete Fourier Transform (DFT) for
  multidimensional arrays of size given by the integer array [n].
  Because the memory representation is different, it is compulsory to
  supply the layout of the multidimensional arrays.  It can be either
  [Bigarray.c_layout] or [!Bigarray.fortran_layout].

  [dir] and the optional arguments [<options>] are the same as for
  {!Fftw2.create} except that there is no possible striding for [~i]
  and [~o].

  The [fft] function returned expects a bigarray of the given layout.
  The bigarray outputed by [fft] can be prescribed with the [~out]
  parameter---otherwise a fresh array is created.  If you want [fft]
  to compute the DFT of several arrays at once, you need to pack them
  into a single bigarray, say [i].  The way it is done depend on the
  layout chosen.  With [c_layout], you need to index your arrays with
  the {i first} [~index_dims] dimensions of [i].  More precisely, the
  DFT of all the arrays

  [Genarray.slice_left i [|j1; ...; jp|]]

  where p is [~index_dims] (by default = 0) and [j1],...,[jp] vary in
  their range.  With [fortran_layout], the indexing dimensions are the
  {i rightmost} ones---so [Genarray.slice_right] can be used to
  set/access the arrays you care about.
*)

val nd_create_in_place :
  ('a, 'b, 'c, 'd) dir ->
  ?measure:bool -> ?use_wisdom:bool -> ?unsafe:bool -> ?normalize:bool ->
  ?i:(Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Genarray.t ->
  ?o:(Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Genarray.t ->
  'e Bigarray.layout ->
  int array ->
  (* [fft] *)
  ?index_dims:int ->
  (Complex.t, Bigarray.complex64_elt, 'e) Bigarray.Genarray.t -> unit
(** Same as {!Fftw2.nd_create} but returns an in-place transform. *)




(** {6 One-dimensional real transform} *)

val r_create :
  ('a, 'b, 'c, 'd) dir ->
  ?measure:bool -> ?use_wisdom:bool -> ?unsafe:bool -> ?normalize:bool ->
  ?i:(float, Bigarray.float64_elt, 'e) Bigarray.Array1.t ->
  ?istride:int ->
  ?o:(float, Bigarray.float64_elt, 'e) Bigarray.Array1.t ->
  ?ostride:int ->
  int ->
  (* fft *)
  ?howmany:int ->
  ?out:(float, Bigarray.float64_elt, 'f) Bigarray.Array1.t ->
  ?ostride:int ->
  ?odist:int ->
  ?istride:int ->
  ?idist:int ->
  (float, Bigarray.float64_elt, 'f) Bigarray.Array1.t ->
  (float, Bigarray.float64_elt, 'f) Bigarray.Array1.t
(**

  [r_create dir <options> n] returns a function, say [fft], that
  computes the Discrete Fourier transform of {i real} ([dir =
  forward]) or {i hermitian} ([dir = backward]) one-dimensional arrays
  of size [n].  To understand how hermitian arrays are stored in
  one-dimensional bigarrays, please consult the FFTW manual.

  [dir] and the optional arguments [<options>] are the same as the
  ones of {!Fftw2.create}.

  BEWARE that the backward transform destroys the contents of the
  input array.

  The resulting [fft] function accepts the same options as the one
  returned by {!Fftw2.create}.
*)


val r_create_in_place :
  ('a, 'b, 'c, 'd) dir ->
  ?measure:bool -> ?use_wisdom:bool -> ?unsafe:bool -> ?normalize:bool ->
  ?i:(float, Bigarray.float64_elt, 'e) Bigarray.Array1.t ->
  ?istride:int ->
  ?o:(float, Bigarray.float64_elt, 'e) Bigarray.Array1.t ->
  ?ostride:int ->
  int ->
  (* fft *)
  ?howmany:int ->
  ?work:(float, Bigarray.float64_elt, 'f) Bigarray.Array1.t ->
  ?istride:int ->
  ?idist:int -> (float, Bigarray.float64_elt, 'f) Bigarray.Array1.t -> unit
(** Same as {!Fftw2.r_create} but returns an in-place transform. *)


(** {6 N-dimensional real transform} *)

val rnd_create :
  ('a, 'b, 'c, 'd) dir ->
  ?measure:bool -> ?use_wisdom:bool -> ?unsafe:bool -> ?normalize:bool ->
  'e Bigarray.layout ->
  int array ->
  (* [fft] *)
  ?index_dims:int ->
  ?out:('c, 'd, 'e) Bigarray.Genarray.t ->
  ('a, 'b, 'e) Bigarray.Genarray.t -> ('c, 'd, 'e) Bigarray.Genarray.t
(** [rnd_create dir <options> layout n] returns a function, say [fft],
  that computes the Discrete Fourier Transform (DFT) for {i real} or
  {i hermitian} multidimensional arrays of size given by the integer
  array [n].

  [dir] and the optional arguments [<options>] are the same as for
  {!Fftw2.create} except that there is no way to optimize the transform
  for specific arrays.

  BEWARE that the [backward] transform destroys the contents of the
  input array.

  Notice that if [dir = forward], [fft] transforms a real array into
  an hermitian array, if [dir = backward], it is the other way around.
  To understand how hermitian arrays are stored , please consult the
  FFTW manual.  Suffices here to say that, if the size of the real array
  is [n = [|n1; ...; np|]], the size of the complex hermitian array
  is [[|n1; ...: n(p-1); np/2+1|]] when [layout] is [c_layout] and
  [[|n1/2+1; n2; ...; np|]] if [layout] is [fortran_layout].

  To pack several arrays into one bigarray to compute at once the DFT
  of these arrays, see the explanations for {!Fftw2.nd_create}.
*)





(** {6 Wisdom} *)

val wisdom_export_fold : (char -> 'a -> 'a) -> 'a -> 'a
(** [wisdom_export_fold f] folds the function [f] over the "list" of
  all chars describing the currently accumulated wisdom. *)

val wisdom_export_iter : (char -> unit) -> unit
(** [wisdom_export_iter f] iterates the function [f] on all chars
  describing the currently accumulated wisdom. *)


external wisdom_to_file : string -> unit =
    "fftw_export_wisdom_to_file_wrapper"
(** [wisdom_to_file fname] writes the currently accumulated wisdom to
  the file whose name is [fname]. *)

external wisdom_to_string : unit -> string =
   "fftw_export_wisdom_to_string_wrapper"
(** [wisdom_to_string()] returns a string containing the currently
  accumulated wisdom. *)


val wisdom_import_fold : ('a -> 'a * char) -> 'a -> 'a
(** [wisdom_import_fold f a] imports the wisdom by folding [f] as many
  times as needed on the initial data [a] to have all the [char]'s
  describing the wisdom. *)

val wisdom_import_iter : (unit -> char) -> unit
(** [wisdom_import_iter f] imports the wisdom by calling [f] as many
  times as needed to have all the [char]'s describing the wisdom. *)

external wisdom_from_file : string -> unit =
   "fftw_import_wisdom_from_file_wrapper"
(** [wisdom_from_file fname] reads the wisdom from the file of name
  [fname]. *)

external wisdom_from_string : string -> unit =
   "fftw_import_wisdom_from_string_wrapper"
(** [wisdom_from_string s] reads the wisdom from the string [s]. *)

external wisdom_forget : unit -> unit = "fftw_forget_wisdom_wrapper"
(** [wisdom_forget()] discards the wisdom accumulated so far. *)



(** {6 Precision} *)

val float : (float, Bigarray.float64_elt) Bigarray.kind
val complex : (Complex.t, Bigarray.complex64_elt) Bigarray.kind
type float_elt = Bigarray.float64_elt
and complex_elt = Bigarray.complex64_elt

(** The FFTW library can be compiled with double or single precision
  (this is detected during the install of this interface).  The above
  definitions allow to write code that is independent of the FFTW
  precision. *)
