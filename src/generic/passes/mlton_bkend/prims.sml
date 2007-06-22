

exception WSError of string
fun wserror str = raise WSError str

exception WSAssertFailed
fun assert b = if b then () else raise WSAssertFailed


(********************************************************************************)
(* ARRAYS *)


(* Should do this with functors, but not entirely sure about modules and inlining. *)

type 'a wsarray   = 'a array
val wslen         = Array.length 
fun wsget a i     = Array.sub (a,i)
fun wsset a i x   = Array.update (a,i,x)
fun wsnull ()     = Array.fromList []
fun wsmakearr n x = Array.array (n,x)

(*fun wssub a st len = Array.extract (a, st, SOME len)*)
fun wssub a st len = 
  (* GRR Can't use Array.extract or Array.copy!! *)
  Array.tabulate(len, fn i => Array.sub(a, st+i))

fun wsappend a b = 
  let open Array
      val len1 = Array.length a
      val len2 = Array.length b 
(*      val newarr = Array.array (len1 + len2, Array.sub(a,0))*)
  in 
   (* This is probably inefficient *)
   Array.tabulate (len1 + len2,
                   fn i => if i < len1 
                     then Array.sub (a,i) else Array.sub (b,i-len1))
  end


(*let rec insert_between x ls =
  match ls with
    | []   -> []
    | [h]  -> [h]
    | h::t -> h:: x:: insert_between x ls*)
fun arrayToList arr =
  let val acc = ref []
      val i   = ref (Array.length arr - 1)
  in
    (while !i >= 0 do
      (acc := Array.sub(arr, !i) :: !acc;
       i := !i - 1);
     !acc)
  end 

fun arrayEqual eq (a1, a2) = 
  let val len = Array.length a1 
      fun loop i = 
        if i = len
	then true
	else if eq( Array.sub(a1,i), Array.sub(a2,i))
        then loop (i+1)
        else false
  in 
    loop 0
  end 

fun concat_wsep sep ls = 
  if ls = []
  then ""
  else if tl ls = []
  then hd ls
  else hd ls ^ sep ^ concat_wsep sep (tl ls)


(********************************************************************************)


    
(*
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
*)

(* Need to halve the size of the output!! *)

(*let fftR2C (flarr : (float, Bigarray.float64_elt) BigArray.Array1.t) : (Complex.t, Bigarray.complex64_elt) BigArray.Array1.t = *)

(*
let fftR2C flarr = 
  let newsize = (Array1.dim flarr / 2) + 1 in 
  let bigarr = Array1.create complex64 c_layout (Bigarray.Array1.dim flarr) in
  for i = 0 to Bigarray.Array1.dim flarr - 1 do
    Array1.set bigarr i { Complex.re= Array1.get flarr i; Complex.im = 0.0 };
  done;
    let result = autofft bigarr in
    (* Really lame to still need to copy out only the bottom half.*)
    let complexarr = Array1.create complex64 c_layout newsize in
      for i = 0 to newsize-1 do
	Array1.set complexarr i  (Array1.get result i);
      done;
      (* Start is zero for this frequency domain array: *)
      complexarr
*)

(* If our arrays were already bigarrays... well the we'd still need to
   copy to get the half of the output that we want. 
   Wouldn't have to copy on input...*)


fun powInt n x = 
  let fun loop acc i =
    if i = 0
    then acc
    else loop (n * acc) (i - 1)
  in loop 1 x end

fun powInt16 n x = 
  let fun loop acc i =
    if i = 0
    then acc
    else loop (Int16.*(n, acc)) (i - 1)
  in loop (Int16.fromInt 1) x end


