

(********************************************************************************)
(* BYTE INDEXED READING *)

(* Will this be inefficient? *)
(* This produces a LargeWord... There's no Word16. *)
(* Wow, look how verbose this SML is compared to the Caml: *)
(* [2007.06.24] Currently this first version is more efficient than the PACK version... might be the DIV. *)
fun read_uint16 vec (i : int) = 
  let val lower : LargeWord.word = Word8.toLarge (Word8Vector.sub(vec,i))
      val upper : LargeWord.word = LargeWord.<< (Word8.toLarge (Word8Vector.sub(vec,i+1)), 
                                                 Word.fromInt 8)
  in
     LargeWord.+ (lower, upper)
  end 

(* read_int16 : string -> int -> int *)
fun read_int16 vec i : Int16.int =
  let val unsigned = read_uint16 vec i in
    Int16.fromInt (Word16.toIntX (Word16.fromLarge unsigned))
  end 

(*
fun read_real32 vec i = 
    let chopped = ??? (* chop a subvector with appropriate starting point. *)
    in PackReal32Little.subVec(vec, i) end     
*)

(********************************************************************************)
(* "WORD" INDEXED READING *)


(* Would be nice not to do a division here *)
fun read_int16_wordIndexed vec i  =
(*   Int16.fromLarge(LargeWord.toLargeIntX (PackWord16Big.subVecX(vec, i)))*)
(* THIS IS FASTER THAN THE ABOVE: *)
    let (*val _ = print ("Got ind "^ (Int.toString i)^ "\n")*)
    in
(*      Int16.fromInt(Word16.toIntX (Word16.fromLarge (PackWord16Big.subVecX(vec, i))))  *)
     Int16.fromInt(Word16.toIntX (Word16.fromLarge (PackWord16Little.subVecX(vec, i)))) 
    end

(*   assert (0 = Int.rem(i,2)); *)
(*   Int16.fromLarge(LargeWord.toLargeIntX (PackWord16Big.subVecX(vec, Int.quot(i,2)))) *)

fun read_int32_wordIndexed vec i  =
(*   assert (0 = Int.rem(i,4)); *)
   (* IN MLTON COULD GO THROUGH PLAIN INT INSTEAD OF LARGE INT?? *)

(*   Int32.fromLarge(Word32.toLargeIntX (Word32.fromLarge (PackWord32Little.subVecX(vec, i))))*)
   Int32.fromInt(Word32.toIntX (Word32.fromLarge (PackWord32Little.subVecX(vec, i))))


fun read_real32_wordIndexed vec i = PackReal32Little.subVec(vec, i)

(********************************************************************************)


fun textFileReader (file, rowsize, tokenParser) outchan = 
  let 
    val timestamp = ref 0
    val hndl = TextIO.openIn file
    (* This reads an entire tuple from a single line *)
    fun read n = 
      case TextIO.inputLine hndl
      of NONE => raise (WSEndOfFile "")
       | SOME line =>
         let 
(* 	  val _ = print ("PARSING LINE for "^Int.toString rowsize^" elements "^line^"\n") *)
          val port = TextIO.openString line	
	  fun pulltoken flag = 
            case TextIO.lookahead port
	      of NONE => raise (WSEndOfFile "")
               | SOME c =>
	        (
(*                  print ("pulltoken " ^ String.implode [c] ^ "\n"); *)
	         if Char.isSpace c
	         then (if flag then [] else (TextIO.input1 port;  pulltoken false))
	         else (TextIO.input1 port;  c :: pulltoken true))
          (* This reads each field of the tuple from a single line. *)
          fun lineloop n =
	    (
(* 	    print ("Lineloop " ^ Int.toString n ^ "\n"); *)
	    if n = 0
	    then []
	    else (let val t = String.implode (pulltoken false) in t :: lineloop (n - 1) end))
         in 
	   lineloop n
         end
    fun strm () =
      let val obj = tokenParser (read rowsize) 
      in
        outchan obj
      end
  in 
    strm
  end

(* datatype scheduleEntry = SE of (timestamp * (unit -> scheduleEntry)) *)


(********************************************************************************)


(* Binary reading, produces a scheduler entry, "SE" *)
(* mode & Textreader parameter are unused  and should be removed *)
fun dataFile (file:string,  mode:string,  repeats:int)
             ( binreader,  bytesize:int,  skipbytes:int,  offset:int)
	     (outchan (*:(Int16.int -> unit)*) )
  =
	  let               
	      val hndl = BinIO.openIn file 
	      val st = ref 0  (* Inclusive *) 
	      val en = ref 0  (* Exclusive *)	      
	      (* Produce a scheduler function *)
	      fun f ()  =
	          (let 
		     val vec = BinIO.inputN(hndl, bytesize)
		     val obj = if not( Vector.length vec = bytesize)
    		               then raise (WSEndOfFile "")
                               else binreader vec 0
	             val _ = outchan obj
                  in ()
		  end;
		 (* Now skip some bytes: *)
		 if 0 = skipbytes then () else (BinIO.inputN(hndl, skipbytes); ()))
	   in 	  
	     (* First we need to skip ahead by the offset. *)
	     (if 0 = offset then () else (BinIO.inputN(hndl, offset); ());
	     f)
	   end

(* This simply constructs a reader function that reads a whole window. 
   Thus it can reuse dataFile above. *)
fun dataFileWindowed config 
    ( binreader (*: BinIO.vector -> int -> 'elem*), 
     bytesize:int, skipbytes:int, offset:int)

     (outchan (*: 'elem -> unit*))
     (winsize:int)

     (* HACK: This should be either 1, or equal to bytesize. *)
     (index_coef:int)

     (arrcreateUnsafe, 
      arrset : 'a array * int * 'a -> unit , 
      tosigseg)
=
    let
      (* TEMP: THIS SHOULD BE INT64: *)
      val winsize_ = Int64.fromInt winsize
      val sampnum = ref (Int64.fromInt 0)
      val bytesperwin : int = bytesize+skipbytes 
      fun block_bread vec baseind = 
      (* Array.init might not be the most efficient: *)
       let val arr = arrcreateUnsafe winsize 
           val i = ref 0
	   val skipwords = Int.quot (skipbytes, bytesize)
       in        
          (* Skipbytes must be in word granularity for our "homogenous" mode reading hack. *)
	  (* This is too harsh actually... because bytesize might be for multiple fields in a tuple. *)
	  (* FIXME!!! *)
	  (assert (Int.rem (skipbytes,bytesize) = 0);
	   assert (index_coef = 1 orelse index_coef = bytesize);
	   while !i < winsize do 
	     (
(*	     
	     print ("reading window with index "^ (Int.toString (!i * index_coef))^
   	            " on baseind "^ (Int.toString baseind) ^
   	            " bytesize "^ (Int.toString bytesize) ^
   	            " bytesperwin "^ (Int.toString bytesperwin) ^
		    "\n");*)
		      
	      arrset (arr, !i, (binreader vec (baseind + (!i * index_coef) + (!i * skipwords))));
	      i := !i + 1);
           let val result = ( tosigseg (arr, !sampnum, newTimebase 8888888))
           in
	   (sampnum := Int64.+(!sampnum, winsize_);
	    result)
           end)
       end
  in
    dataFile config ( block_bread, bytesperwin * winsize, 0, offset) outchan
  end




(*
    (string * string * int * int)
 -> ('a * (BinIO.vector -> int -> 'b) * int * int * int)
 -> (Int32.int -> unit)
 -> int
 -> ((int -> 'c) * ('c -> int ref -> 'b -> 'd) * 
     ('c -> ('e ref -> 'e) -> int ref -> int -> Int32.int))
  -> scheduleEntry
*)

(*
val _ = print_endline "ARGUMENTS: "
val _ = List.map print_endline (CommandLine.arguments())
*)
