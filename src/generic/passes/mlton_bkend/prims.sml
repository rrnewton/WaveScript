

exception WSError of string
fun wserror str = raise WSError str

exception WSAssertFailed
fun assert b = if b then () else raise WSAssertFailed

exception SigSegFailure of string

(*
structure Complex =
struct
  type complex = (Real32.real * Real32.real)
  add 
end
*)

val nullChar = "\000"

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
  (
(*   print ("wssub: arrlen "^(Int.toString (Array.length a))^
          " start "^(Int.toString st)^" len "^(Int.toString len)^" \n");*)
   Array.tabulate(len, fn i => Array.sub(a, st+i)))
  

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
  let val len  = Array.length a1 
      val len2 = Array.length a2
      fun loop i = 
        if i = len
	then true
	else if eq( Array.sub(a1,i), Array.sub(a2,i))
        then loop (i+1)
        else false
  in 
    (len = len2) andalso loop 0
  end 


(* Same as arrayEqual *)
fun vectorEqual eq (v1, v2) = 
  let val len  = Vector.length v1 
      val len2 = Vector.length v2
      fun loop i = 
        if i = len
	then true
	else if eq( Vector.sub(v1,i), Vector.sub(v2,i))
        then loop (i+1)
        else false
  in 
    (len = len2) andalso loop 0
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

(* Takes a Word64 to a complex struct. *)
val unpack_complex = 
  let val bytes = Word8Array.array(8, Word8.fromInt 0)
  in
    fn (w64 : Word64.word) =>
    let 
        val _  = PackWord64Little.update (bytes, 0, Word64.toLarge w64)
        val rl = PackReal32Little.subArr (bytes, 0)
        val im = PackReal32Little.subArr (bytes, 1)

(* 	val _ = print ("UNPACKING COMPLEX "^ (Real32.toString rl) ^" "^ (Real32.toString im) ^"\n") *)
(*
        open Word64
	val twobyte = fromLarge (Word.toLarge 0wxFFFF)
        val ones32  = orb(<<(twobyte,0w16), twobyte)
	val lower   = andb(ones32,w64)
	val upper   = >>(w64,0w32)
	val _ = print ("UNPACKING COMPLEX "^ (toString lower) ^" "^ (toString upper) ^"\n")
*)	
    in
      {real=rl, imag=im}

(*       {real= Real32.fromInt 3, imag= Real32.fromInt 4} *)
    end
  end 

(* This packs a complex array into a word64 array *)
val pack_complex_array =  
  let val bytes = Word8Array.array(8, Word8.fromInt 0) in
  fn arr => 
  Array.tabulate(Array.length arr,
    fn i => 
      let val {real,imag} = Array.sub(arr,i) 
          val _ = PackReal32Little.update(bytes,0, real)
          val _ = PackReal32Little.update(bytes,1, imag)
      in        
       PackWord64Little.subArr(bytes,0)
      end)
  end


fun unpack_complex_Pointer p i = 
  let val rl = MLton.Pointer.getReal32(p,i)
      val im = MLton.Pointer.getReal32(p,i+1)
  in
    {real=rl, imag=im}
  end

fun unpack_complex_array_Pointer p len = 
  Array.tabulate (len, fn i => unpack_complex_Pointer p (i*2))

fun unpack_int16_array_Pointer p len = 
  Array.tabulate (len, fn i => MLton.Pointer.getInt16(p,i))




(* [2007.07.01] Having problems with this currently *)
(*
val memoized_fftR2C_wrapper = 
  let 
    val cached_size = ref 0    
    val outbuf      = ref (Array.fromList [])
    val outbuf2      = ref (Array.array (4096, Word64.fromInt 0))
    val _ = (outbuf := Array.array (1, Word64.fromInt 0))
  in
    fn arr => 
      let val len = Array.length arr
	  val len2 = (len div 2) + 1
          val _ = if not(!cached_size = len)
	          then (cached_size := len;
(*  		        outbuf := Array.array (len2*10, Word64.fromInt 0);  *)
		        set_cached_plan(!outbuf2, len))
		  else ()
          val _ = memoized_fftR2C(arr, len)
      in       
        Array.tabulate (len2, fn i => unpack_complex (Array.sub (!outbuf2,i)))
      end
  end  
*)


(* Here we parse command line args to look for an output limit *)
val element_limit = 
  let val args = CommandLine.arguments()
  in if not (null args) andalso (hd args = "-n")
     then case Int.fromString (hd (tl args)) 
          of SOME x => x 
           | NONE => raise (WSError ("bad argument to -n flag: "^(hd (tl args))))
     else ~1
  end 


fun errprnt str = TextIO.output (TextIO.stdErr, str)
fun exit_process flag = 
  if flag 
  then OS.Process.exit OS.Process.success
  else OS.Process.exit OS.Process.failure
fun exit_foreignsrc _ = ~1

(* Returns an error code. *)
fun run_w_handlers comp stdprnt errprnt exitfun =
  (comp (); 0)
  handle WSError str => 
    (stdprnt ("wserror: " ^ str ^ "\n"); exitfun false)
  | SigSegFailure str => 
    (stdprnt ("sigseg failure: " ^ str ^ "\n"); exitfun false)
  | WSEndOfFile => 
    (errprnt "Reached end of file. \n";  exitfun true)

(* This is only used for a normal "timer" driven query foreign_source
   driven queries don't use it.*)
fun runMain f = 
  (*   print "Setting up error handlers\n";*)
  run_w_handlers f print errprnt exit_process
  
(*
			   "handle WSError str => \n"
			   "  (print (\"wserror: \" ^ str ^ \"\\n\");\n"
;                           "   raise (WSError str)) \n"
                           "   OS.Process.exit OS.Process.failure) \n"
			   " | SigSegFailure str => \n"
			   "  (print (\"sigseg failure: \" ^ str ^ \"\\n\");\n"
;                           "   raise (SigSegFailure str)) \n"
                           "   OS.Process.exit OS.Process.failure) \n"
			   " | WSEndOfFile => "
			   "  (TextIO.output (TextIO.stdErr, \"Reached end of file. \\n\");\n"
                           "   OS.Process.exit OS.Process.success) \n"			   

*)
