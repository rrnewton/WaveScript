


(*
let (gridsize, shiftx, shifty, maxx, maxy) = 0.001, 2000, 2500, 4096, 4096;;    (* APPROX 316 feet *)
let (gridsize, shiftx, shifty, maxx, maxy) = 0.0007, 1900, 2100, 4096, 4096;;

*)

let (gridsize, shiftx, shifty, maxx, maxy) = 0.0001, 900, 3700, 8192, 8192;;   

(* let smallgrid = "" *)
let smallgrid = "_SMALLGRID"


(* Lewis uses 0.0001 *) 

open Utils;;
open Graphics;;
open Printf;;
open Bigarray;;
open_graph "";;
auto_synchronize false;;
resize_window 800 800;;

printf "Running script to analyze all car traces.\n";;
flush stdout;;

scrollx := (maxx / 2);;
scrolly := (maxy / 2 - 300);;

the_maxx := maxx;;
the_maxy := maxy;;

let theworld = bitarray ("/home/newton/data/ALLCARS"^smallgrid^".bitmap") maxx maxy
let distlog = open_out ("/home/newton/data/ALLCARS"^smallgrid^".dstlog")

(* The number of steps forward spent moving.  In 400ths of a second.*)
let stepsforward = ref Int64.zero

(* The number of cells currently activated in the bitmap *)
let cellcount = ref Int64.zero

let denone x = match x with None -> raise (Failure "denone") | Some y -> y

let lastfiletime = ref 0.0

let rec process_file basetime file =
  printf "\n;===========================================\n";
  printf "STARTING NEW FILE: basetime: %f  last time was: %f\n\n" basetime !lastfiletime;
  let filepos = ref 0.0 (*Int64.zero*) in
  let lastpos = ref 0 in
  let count   = ref 0 in
  let timeorigin = ref None in

  set_window_title file;
  let dat = open_in file in

  let rec loop lastx lasty  = 
  Scanf.fscanf dat "%f %f %f %d %d %d %d %s" 
    (fun time lat lon x y z dir speed ->    
       let x = (int_of_float ((lat -. 42.) /. gridsize))
       and y = (int_of_float ((lon +. 71.) /. (0.0 -. gridsize))) in 
       let xp = x + shiftx
       and yp = y + shifty in

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

	 (* HACKISH: Here we just throw out the data *)
	 if time < 4900000. 
	    || xp < 0 || yp < 0 
            || xp >= maxx || yp >= maxy
	 then loop lastx lasty 
	 else 
	   begin
	     if !count mod 400000 == 0 
	     then (
	       count := 0;
	       (*printf " grid loc %d %d\n" xp yp;
		 flush stdout;*)
	       draw_screen theworld;
	     );
	     if !count mod 10000 == 0 
	     then (
	     	 update_scroll();
(*		 printf "line read lat %f  lon %f   x %d y %d  xp %d  yp %d\n" lat lon x y xp yp; flush stdout;*)
		  );
	     if !count mod 100000 == 0
	     then (
	       let fp = pos_in dat in
		 if fp - !lastpos > 0
		 then filepos := !filepos +. float_of_int (fp - !lastpos);
		 lastpos := fp;
		 printf " Filepos in file %s:   %f mb \n" file (!filepos /. 1000000.);
		 flush stdout;
	     );
	     let origin = 
	       match !timeorigin with
		   None -> timeorigin := Some time; time
		 | Some x -> x 
	     in
	     let thistime = (basetime +. time -. origin) in		       
	       (* Having problems with this: *)
	       (* printf "  HUH? thistime %f\n" thistime;*)
	       lastfiletime := max !lastfiletime thistime;

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
			 thistime
			 (*(!timemoving -. origin)*)
			 !stepsforward !cellcount (*!sincelaststep*);
		     )
		     else 
		      set theworld xp yp (old + 1);
		     (*sincelaststep := 0.0;*)
	       end;
	     incr count;
	     loop xp yp 
	 end)
  in loop 0 0 
;;



(*process_file Sys.argv.(1);;*)

try process_file !lastfiletime "/home/newton/data/slave6.txt" with End_of_file -> 
try process_file !lastfiletime "/home/newton/data/slave10.txt" with End_of_file -> 
try process_file !lastfiletime "/home/newton/data/slave11.txt" with End_of_file -> 
try process_file !lastfiletime "/home/newton/data/slave14.txt" with End_of_file -> 
try process_file !lastfiletime "/home/newton/data/slave16.txt" with End_of_file -> 
try process_file !lastfiletime "/home/newton/data/slave18.txt" with End_of_file -> 
try process_file !lastfiletime "/home/newton/data/slave19.txt" with End_of_file -> 
try process_file !lastfiletime "/home/newton/data/slave22.txt" with End_of_file -> 
try process_file !lastfiletime "/home/newton/data/slave24.txt" with End_of_file -> 
try process_file !lastfiletime "/home/newton/data/slave29.txt" with End_of_file -> 

   printf "Finished.  Reached end of all files."
;;

(* Then just leave the picture up till the user kills us. *)
while true do
  update_scroll();
  draw_screen theworld;
done
