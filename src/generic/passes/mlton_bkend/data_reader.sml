

(* Will this be inefficient? *)
fun read_uint16 str (i : int) = 
  Word.toInt
  (Word.+ ( Word.<< (Word.fromInt (Char.ord (String.sub (str, i + 1))), Word.fromInt 8),
            Word.fromInt (Char.ord (String.sub (str, i)))))


(* read_int16 : string -> int -> int *)
fun read_int16 str i =
  let val unsigned = read_uint16 str i in
    if 0 = Word.toInt (Word.andb (Word.fromInt unsigned, Word.fromInt 32768))
    then unsigned 
    else unsigned - 65536
  end 


exception WSError
fun wserror str = raise WSError


(* Binary reading, produces a scheduler entry, "SE" *)
(* mode & Textreader parameter are unused  and should be removed *)
fun dataFile (file, mode, repeats, period) 
             (textreader,binreader, bytesize, skipbytes, offset)
	     outchan 
  =
	  (* Produce a scheduler function *)
	  (* Feel free to change this constant: *)
	  let val buffer_min_size = 32768 
              val chunk = Int.max buffer_min_size (bytesize + skipbytes) 
	      val buf = String.make chunk #"_"
	      val hndl = open_in_bin file 
	      val timestamp = ref 0
	      val st = ref 0    (* Inclusive *) 
	      val en = ref 0  (* Exclusive *)
	      fun scan offset =
	        if offset>0
	        then (really_input hndl buf 0 (min chunk offset); scan (offset - min chunk offset))
                else ()
	   fun f acc =
		let val read = TextIO.input hndl 
		    val acc2 = acc ++ read
		in
		if String.size acc2 >= chunk 
                then 

	    (* Read by block, don't read more than we have space for *)
	    let val read = input hndl buf !en (chunk - !en) in
	      (* TODO: Check for end of file!!! *)
	      (if read = 0 then (print_endline "dataFile out of data"; exit 0) else ();
	      en := !en + read;
	      while !en - !st >= bytesize + skipbytes do
	        (outchan (binreader buf !st);
		 st := !st + bytesize + skipbytes);
	      (* If we're too near the end of the buffer, bring us back to the start: *)
	      if !en + bytesize >= chunk 
	      then (
		String.blit buf !st buf 0 (!en - !st);
		en := !en - !st;
		st := 0)
              else ();
	      timestamp := !timestamp + period;
	      SE (!timestamp, f))
            end
	   in (scan offset; SE (0, f)) end
(*

(*


let dataFileWindowed config (* (file, mode, repeats, period) *)
    (textreader,binreader, bytesize, skipbytes, offset)
    outchan winsize (arrcreate_unsafe, arrset, tosigseg)  =
  let sampnum = ref 0 in
  let wordsize = bytesize+skipbytes in
  let block_bread str baseind = 
    (* Array.init might not be the most efficient: *)
    let arr = arrcreate_unsafe winsize in
      for i = 0 to winsize - 1 do 
        arrset arr i (binreader str (baseind + i*wordsize));
      done;
      let result = tosigseg arr !sampnum 3339 in
	sampnum := !sampnum + winsize;
	result
  in
    dataFile config (38383, block_bread, wordsize * winsize, 0, offset) outchan





(*
let dataFileWindowed config (tread, bread, size) outchan winsize bigarrformat = 
  let sampnum = ref 0 in
  let block_bread str i = 
    (* Array.init might not be the most efficient: *)
(*    let arr = Array.init winsize (fun i -> bread str (i*size)) in      *)
    let arr = Array1.create bigarrformat c_layout winsize in
      for i = 0 to winsize - 1 do 
	Array1.set arr i (bread str (i*size));
      done;
      let result = toSigseg arr !sampnum 3339 in
	sampnum := !sampnum + winsize;
	result
  in
    dataFile config (38383, block_bread, size * winsize) outchan


*)


*)

*)
