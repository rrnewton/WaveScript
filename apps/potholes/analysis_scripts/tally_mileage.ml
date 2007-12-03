



let gridsize = 0.001 ;;

(* Lewis uses 0.0001 *) 

(* let file = "/home/newton/data/slave18.txt"  *)
if Array.length Sys.argv == 1
then (raise (Failure "must pass data filename as first command line argument"));;

open Graphics;;
open Printf;;
open Bigarray;;

(* 
open_graph ":0.0 300x100+50-0" 
open_graph "foo:0.0 500x500+50-50";;

*)

      
open_graph "";;
auto_synchronize false;;
resize_window 800 800;;


let file = Sys.argv.(1);;
set_window_title file;;

let dat = open_in file
let distlog = open_out (file ^ ".dstlog")
let maxx = 4096 
let maxy = 4096


let shiftx = 2000
let shifty = 2500
(* Allow scrolling around with mouse. *)
let scrollx = ref 0
let scrolly = ref 0

    
(********************************************************************************)

(* simple bit arrays *)


(*
let bitarray n m = Array.make_matrix n m 0
let get ba i j = ba.(i).(j)
let set ba i j x = ba.(i).(j) <- x
let size_x ba = Array.length ba
let size_y ba = Array.length ba.(0)
*)

(* Let's try just using a mapped file from the start: *)
let bitarray n m = 
  (*Array2.create int8_unsigned c_layout  n m;;*)
  let p = Unix.openfile (file ^ ".bitmap") [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644  in
    Array2.map_file p int8_unsigned c_layout true n m

let get ba i j   = Array2.get ba i j
let set ba i j x = Array2.set ba i j x
let size_x ba    = Array2.dim1 ba 
let size_y ba    = Array2.dim2 ba   

(*
let bitarray n m = 
  assert (n mod 16 == 0);
  Array2.create int16_unsigned c_layout 
    (int_of_float (ceil ((float_of_int n) /. 16.))) m;;
let get ba i j = 
  let word = Array2.get ba (i asr 4) j in
    printf "setting i %d i_ind %d word %d\n" i (i asr 4) word;
    (* annoying way to test a bit *)
    1 land (word asr (i land 15))
let set ba i j x = 
  let word = Array2.get ba (i asr 4) j in
    (* annoying way to test a bit *)
    Array2.set ba (i asr 4) j 
     (let bit = 1 asr (i land 15) in
       (if x == 0
	then word land (lnot bit)
	else word lor bit))
let size_x ba = Array2.dim1 ba * 16
let size_y ba = Array2.dim2 ba   
*)

(********************************************************************************)

let theworld = bitarray maxx maxy

let draw_screen arr = 
  for i = 0 to size_x arr - 1 do
    for j = 0 to size_y arr - 1 do
      if 1 == get arr i j 
      then plot (i - shiftx ) (j - shifty + 300)
    done 
  done;
  printf "drawn!\n";
  flush stdout;
  synchronize ()
;;



let filepos = ref 0.0 (*Int64.zero*)

(* This is used for redraws and stuff. *)
let count   = ref 0 
(*let totalcount = ref Int64.zero*)
(*let sincelaststep = ref 0.0
let ismoving = ref false*)
(*let timemoving = ref 0.0*)

(* The number of steps forward spent moving.  In 400ths of a second.*)
let stepsforward = ref Int64.zero

(* The number of cells currently activated in the bitmap *)
let cellcount = ref Int64.zero
let lastpos = ref 0

let timeorigin = ref None
let denone x = match x with None -> raise (Failure "denone") | Some y -> y
(*
[2007.12.03] I think that I used %s to get the damn newline also.
*)
let rec loop linecount lastx lasty lasttime  = 
  let (time,lat,lon) = 
    try Scanf.fscanf dat "%f %f %f %Ld %Ld %Ld %Ld %s" (fun time lat lon _x _y _z _dir _speed -> (time,lat,lon))
    with Scanf.Scan_failure str -> 
      (printf "!!! Got a scan failure: %s\n" str;
       printf "  Line number was %Ld\n" linecount;
       printf "Continuing on rest of file.\n";
       let _ = input_line dat in 
       loop (Int64.add  linecount Int64.one) lastx lasty lasttime;     
       ) 
   | Sys_error str -> 
       printf "XXX Got a fatal Sys_error: %s\n" str;
       printf "  Line number was %Ld\n" linecount;
       raise (Sys_error str);
  in 
  let x = (int_of_float (lat /. 0.001))
  and y = (int_of_float (lon /. -0.001)) in 
  let xp = x - 42000 + shiftx
  and yp = y - 71000 + shifty in

(* using a calculator:
   dist 42 long to 43 long: 
   dist 71 lat to 71 lat:

     x = 69.1 * (lat2 - lat1)
 and y = 69.1 * (lon2 - lon1) * cos(lat1/57.3) 
   
   x = 69.1
   y = 69.1 *. cos ()
   

lat = 51 miles
lon = 68.9 miles

~60 miles * 0.001 = 0.06 m

*)

(*	 if xp<0 then (printf "x neg %d\n" xp; assert false);
	 if yp<0 then (printf "y neg %d\n" xp; assert false);*)

	 (* HACKISH*)
	 if time < 4900000. || xp < 0 || yp < 0 
	 then loop (Int64.add linecount Int64.one) lastx lasty lasttime 
	 else 
	   begin
	     if !count mod 400000 == 0 
	     then (
	       count := 0;
	       (*printf " grid loc %d %d\n" xp yp;
		 flush stdout;*)
	       draw_screen theworld;
	     );
	     if !count mod 100000 == 0
	     then (
	       let fp = pos_in dat in
		 if fp - !lastpos > 0
		 then filepos := !filepos +. float_of_int (fp - !lastpos);
		 lastpos := fp;
		 printf " Filepos: %f mb \n" (!filepos /. 1000000.);
		 flush stdout;
	     );
	     let origin = 
	       match !timeorigin with
		   None -> timeorigin := Some time; time
		 | Some x -> x in
	     (*let delta = time -. lasttime in*)
	       if (xp == lastx && yp == lasty)
	       then () (*sincelaststep := !sincelaststep +. delta*)
	       else 
	       begin
		   stepsforward := Int64.add !stepsforward Int64.one;
		 (* timemoving := !timemoving +. delta;*)
		   let old = get theworld xp yp in
		     if 0 == old then (
		       set theworld xp yp 1;
		       cellcount := Int64.add !cellcount Int64.one;
		       fprintf distlog "%f  %Ld %Ld \n" 
			 (time -. origin)
			 (*(!timemoving -. origin)*)
			 !stepsforward !cellcount (*!sincelaststep*);
		     );
		     (*sincelaststep := 0.0;*)
	       end;
	     incr count;
	     loop (Int64.add  linecount Int64.one) xp yp time 
	   end
;;

loop Int64.zero 0 0 0.0


