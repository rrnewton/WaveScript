

(* OLD OLD OLD  USE ALLCARS VERSION *)




let gridsize = 0.001 ;;

(* Lewis uses 0.0001 *) 

(* let file = "/home/newton/data/slave18.txt"  *)
if Array.length Sys.argv == 1
then (raise (Failure "must pass data filename as first command line argument"));;

open Utils;;
open Graphics;;
open Printf;;
open Bigarray;;

(* 
open_graph ":0.0 300x100+50-0" 
open_graph "foo:0.0 500x500+50-50";;

*)
    
(********************************************************************************)
      
open_graph "";;
auto_synchronize false;;
resize_window 800 800;;

printf "Running script to analyze all car traces.\n";;
flush stdout;;

(*let dat = open_in file
let distlog = open_out (file ^ ".dstlog")*)

let (gridsize, shiftx, shifty, maxx, maxy) = 0.0001, 900, 3700, 8192, 8192;;   

let smallgrid = "_SMALLGRID"
let theworld = bitarray ("~/data/ALLCARS"^smallgrid^".bitmap") maxx maxy
let distlog = open_out ("~/data/ALLCARS"^smallgrid^".dstlog")

(* Allow scrolling around with mouse. *)
let scrollx = ref 0
let scrolly = ref 0

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


(* The number of steps forward spent moving.  In 400ths of a second.*)
let stepsforward = ref Int64.zero 

(* The number of cells currently activated in the bitmap *)
let cellcount = ref Int64.zero

(*let timeorigin = ref None*)
(*let actualinterval = ref None*)
let desiredinterval = 0.002631579 

let denone x = match x with None -> raise (Failure "denone") | Some y -> y

let linecount = ref Int64.zero 

let rec process_file lastcount file =
  printf "\n;===========================================\n";
  (*printf "STARTING NEW FILE: basetime: %f  last time was: %f\n\n" basetime !lastfiletime;*)
  printf "STARTING NEW FILE: %s\n\n" file;

  set_window_title file;;

  let filepos = ref 0.0 (*Int64.zero*) in
  (* This is used for redraws and stuff. *)
  let count   = ref 0 in
  (*let totalcount = ref Int64.zero*)
  (*let sincelaststep = ref 0.0
  let ismoving = ref false*)
  (*let timemoving = ref 0.0*)

  let lastpos = ref 0 in

  let rec loop lastx lasty lasttime  = 
   let (time,lat,lon) = 
   (* [2007.12.03] I think that I used %s to get the damn newline also. *)
    try Scanf.fscanf dat "%f %f %f %Ld %Ld %Ld %Ld %s" (fun time lat lon _x _y _z _dir _speed -> (time,lat,lon))
    with Scanf.Scan_failure str -> 
      (printf "!!! Got a scan failure: %s\n" str;
       printf "  Line number was %Ld\n" linecount;
       printf "Continuing on rest of file.\n";
       let _ = input_line dat in 
       loop lastx lasty lasttime;     
       ) 
   | Sys_error str -> 
       printf "XXX Got a fatal Sys_error: %s\n" str;
       printf "  Line number was %Ld\n" linecount;
       raise (Sys_error str);
  in 

  linecount := (Int64.add !linecount Int64.one);

(*  let x = (int_of_float (lat /. 0.001))
  and y = (int_of_float (lon /. -0.001)) in *)

  let x = (int_of_float ((lat -. 42.) /. gridsize))
  and y = (int_of_float ((lon +. 71.) /. (0.0 -. gridsize))) in 

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
	 (*if time < 4900000. || xp < 0 || yp < 0 
	 then loop (Int64.add linecount Int64.one) lastx lasty lasttime 
	 else *)
	   begin
	     if !count mod 400000 == 0 
	     then (
	       count := 0;
	       (*printf " grid loc %d %d\n" xp yp;
		 flush stdout;*)
	       draw_screen theworld;
	     );
	     (if !count mod 100000 == 0
	     then (
	       let fp = pos_in dat in
		 if fp - !lastpos > 0
		 then filepos := !filepos +. float_of_int (fp - !lastpos);
		 lastpos := fp;
		 printf " Filepos: %f mb \n" (!filepos /. 1000000.);
		 flush stdout;
	     );
(*
	     let origin = 
	       match !timeorigin with
		   None -> timeorigin := Some time; time
		 | Some x -> 
		 (match !actualinterval with
		    None -> actualinterval := Some (time -. x);
		            printf "Discovered interval of %f\n" (denone !actualinterval)
		  | Some inter -> ());
		  x in
*)

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
(*		       match !actualinterval with
                         None -> ()
		       | Some inter -> *)
		          fprintf distlog "%f  %Ld %Ld \n" 
 			    (Int64.to_float linecount *. desiredinterval)
			    (*(abs_float ((time -. origin) /. inter) *. desiredinterval)*)
			    (*(!timemoving -. origin)*)
			    !stepsforward !cellcount (*!sincelaststep*);
			  flush distlog;
		     );
		     (*sincelaststep := 0.0;*)
	       end);
	     incr count;
	     loop  xp yp time 
	   end
  in loop 0 0 0.0
;;


(*let file = Sys.argv.(1);;*)

try process_file "~/data/slave6.txt" with End_of_file -> 

   printf "Finished.  Reached end of all files."


