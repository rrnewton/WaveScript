



let read_uint16 str i : int = 
  256 * (Char.code str.[i+1]) + (Char.code str.[i])

(*
read_int16 : string -> int -> int
let read_int16 str i =
  let unsigned = read_int16 str i in
    if 0 == (fxlogand unsigned 32768)
    then unsigned  
    else unsigned - 65536

*)


let wserror str = raise (Failure str)

let dataFile (file, mode, period, repeats) (textreader,binreader,bytesize) outchan =
    match mode with 
      | "text" -> wserror "doesn't support text mode yet";
      | "binary" ->
	  (* Produce a scheduler function *)
	  let chunk = max 1024 bytesize in
	  let buf = String.make chunk '_' 
	  and hndl = open_in_bin file 
	  and timestamp = ref 0
	  and st = ref 0 
	  and en = ref 0 in 
	  let rec f() =
	    (* Read by block, don't read more than we have space for *)
	    let read = input hndl buf !en (chunk - !en) in
	      (* TODO: Check for end of file!!! *)
	      if read == 0 then (print_endline "dataFile out of data"; exit 0);
	      en := !en + read;
	      while !en - !st > bytesize do
		outchan (binreader buf !st);
		st := !st + bytesize;
	      done;
	      (* If we're too near the end of the buffer, bring us back to the start: *)
	      if !en + bytesize >= chunk 
	      then begin 
		String.blit buf !st buf 0 (!en - !st);
		en := !en - !st;
		st := 0;
	      end;
	      timestamp := !timestamp + period;
	      SE (!timestamp, f)
	  in SE (0, f)
      | _ -> wserror ("unknown mode: "^mode)


let dataFileWindowed config (tread, bread, size) outchan winsize = 
  let sampnum = ref 0 in
  let block_bread str i = 
    (* Array.init might not be the most efficient: *)
    let arr = Array.init winsize (fun i -> bread str (i*size)) in      
    let result = SS(arr, !sampnum, winsize) in
      sampnum := !sampnum + winsize;
      result
  in
    dataFile config (38383, block_bread, size * winsize) outchan
