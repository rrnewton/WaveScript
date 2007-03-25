



let read_uint16 str i : int = 
  (Char.code str.[i+1] lsl 8) + 
  (Char.code str.[i])

(* read_int16 : string -> int -> int *)
let read_int16 str i =
  let unsigned = read_uint16 str i in
    if 0 == (unsigned land 32768)
    then unsigned  
    else unsigned - 65536

let wserror str = raise (Failure str)

let dataFile (file, mode, repeats, period) 
             (textreader,binreader, bytesize, skipbytes, offset)
	     outchan =
    match mode with 
      | "text" -> wserror "doesn't support text mode yet";
      | "binary" ->
	  (* Produce a scheduler function *)
	  let chunk = max 1024 (bytesize + skipbytes) in
	  let buf = String.make chunk '_' 
	  and hndl = open_in_bin file 
	  and timestamp = ref 0
	  and st = ref 0 
	  and en = ref 0 in 
	  let rec scan offset =
	    if offset>0
	    then (really_input hndl buf 0 (min chunk offset); scan (offset - min chunk offset))
	  in scan offset;
	  let rec f() =
	    (* Read by block, don't read more than we have space for *)
	    let read = input hndl buf !en (chunk - !en) in
	      (* TODO: Check for end of file!!! *)
	      if read == 0 then (print_endline "dataFile out of data"; exit 0);
	      en := !en + read;
	      while !en - !st > bytesize + skipbytes do
		outchan (binreader buf !st);
		st := !st + bytesize + skipbytes;
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


let dataFileWindowed config (* (file, mode, repeats, period) *)
    (textreader,binreader, bytesize, skipbytes, offset)
    outchan winsize bigarrformat =
  let sampnum = ref 0 in
  let wordsize = bytesize+skipbytes in
  let block_bread str baseind = 
    (* Array.init might not be the most efficient: *)
    let arr = Array1.create bigarrformat c_layout winsize in
      for i = 0 to winsize - 1 do 
	Array1.set arr i (binreader str (baseind + i*wordsize));
      done;
      let result = toSigseg arr !sampnum 3339 in
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
