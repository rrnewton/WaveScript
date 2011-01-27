open Utils;;
open Graphics;;
open Printf;;
open Bigarray;;


(* 
  HACK HACK HACK.. DUPLICATED CODE AND PARAMETER VALUES:
   MANUALLY MUST SET THE RIGHT PARAMETERS:
  *)

(*
let (gridsize, shiftx, shifty, maxx, maxy, kind) = 0.001, 2000, 2500, 4096, 4096, int16_unsigned;;    (* APPROX 316 feet *)
let (gridsize, shiftx, shifty, maxx, maxy, kind) = 0.0007, 1900, 2100, 4096, 4096, int16_unsigned;;
let (gridsize, shiftx, shifty, maxx, maxy, kind) = 0.0001, 900, 3700, 8192, 8192, int16_unsigned;;    (* smallgrid *)
*)

let (gridsize, shiftx, shifty, maxx, maxy, kind) = 0.0001, 900, 3700, 8192, 8192, int16_unsigned;;    (* smallgrid *)



if Array.length Sys.argv == 1
then (raise (Failure "must pass data filename as first command line argument"));;


(*
let maxx = 
  if Array.length Sys.argv > 2
  then int_of_string Sys.argv.(2)
  else 4096
let maxy = 
  if Array.length Sys.argv > 3
  then int_of_string Sys.argv.(3)
  else 4096
    *)
      
open_graph "";;
auto_synchronize false;;
resize_window 800 800;;
set_window_title (Sys.argv.(1));;

printf "Running script to generated CDF and display stored bitmap.\n";;
flush stdout;;

scrollx := (maxx / 2);;
scrolly := (maxy / 2 - 300);;
the_maxx := maxx;;
the_maxy := maxy;;

(* Allow scrolling around with mouse. *)
let scrollx = ref 0
let scrolly = ref 0


let tofile fn ba = 
  let p = Unix.openfile fn [] 0o644  in
  let f = Array2.map_file p (Array2.kind ba) (Array2.layout ba) 
            true (Array2.dim1 ba) (Array2.dim2 ba) in
    (* Copy it into the file. *)
    Array2.blit ba f;
    Unix.close p    

let fromfile fn = 
  let p = Unix.openfile fn [Unix.O_RDONLY] 0o644  in
  let f = Array2.map_file p kind c_layout false maxx maxy in
    f
;;

let counthits arr = 
  let sum = ref 0 in
  for i = 0 to maxx-1 do
    for j = 0 to maxy-1 do
      if get arr i j != 0 
      then incr sum
    done 
  done;
  !sum

let sumhits arr = 
  let sum = ref 0 in
  for i = 0 to maxx-1 do
    for j = 0 to maxy-1 do
      sum := !sum + get arr i j
    done 
  done;
  !sum
;;


let buildhisto arr = 
  let hist = Array.make 10000 0 in
  for i = 0 to maxx-1 do
    for j = 0 to maxy-1 do
      let cnt = get arr i j in
	hist.(cnt) <- 1 + hist.(cnt);
    done 
  done;
    hist 
;;



(printf "YAY\n");;
flush stdout;;

let arr = (fromfile (Sys.argv.(1))) ;;

(printf "GOT ARR\n");; flush stdout;;
printf "COUNT: %d\n" (counthits arr);; flush stdout;;
printf "SUMHITS: %d\n" (sumhits arr);; flush stdout;;

let hist = buildhisto arr;;

printf "SUMHITS: %d\n" (sumhits arr);; flush stdout;;
let histfile = open_out "./histogram.dat";;

for i = 0 to Array.length hist - 1 do
  if hist.(i) > 0
  then begin 
    fprintf histfile "%d %d\n" i hist.(i);
    printf "%d:%d " i hist.(i)
  end 
done;;
close_out histfile;;

printf "\n\nWrote histogram to file histogram.dat\n";;

(* Then just leave the picture up till the user kills us. *)
while true do
  update_scroll();
  draw_screen arr;
done
