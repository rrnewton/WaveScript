





let wserror str = raise (Failure str)

let dataFile file mode period repeats (textreader,binreader,bytesize) outchan =
    match mode with 
      | "text" -> wserror "doesn't support text mode yet";
      | "binary" ->
	  (* Produce a scheduler function *)
	  let chunk = 1024 in
	  let buf = String.make chunk '_' 
	  and hndl = open_in_bin file 
	  and timestamp = ref 0
	  and st = ref 0 
	  and en = ref 0 in 
	  let rec f() =
	    (* Read by block, don't read more than we have space for *)
	    let read = input hndl buf !en (chunk - !en) in
	      (* TODO: Check for end of file!!! *)
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


