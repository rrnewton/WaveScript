
open Graphics;;
open Printf;;
open Bigarray;;

if Array.length Sys.argv == 1
then (raise (Failure "must pass data filename as first command line argument"));;

      
open_graph "";;
auto_synchronize false;;
resize_window 800 800;;
set_window_title (Sys.argv.(1));;

(* 
  HACK HACK HACK.. DUPLICATED CODE AND PARAMETER VALUES:
  *)

let shiftx = 2000
let shifty = 2500
let maxx = 4096 
let maxy = 4096
(* Allow scrolling around with mouse. *)
let scrollx = ref 0
let scrolly = ref 0


let bitarray n m = 
  (*Array2.create int8_unsigned c_layout  n m;;*)
  let p = Unix.openfile (Sys.argv.(1)) [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644  in
    Array2.map_file p int8_unsigned c_layout true n m

let get ba i j   = Array2.get ba i j
let set ba i j x = Array2.set ba i j x
let size_x ba    = Array2.dim1 ba 
let size_y ba    = Array2.dim2 ba   



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


let tofile fn ba = 
  let p = Unix.openfile fn [] 0o644  in
  let f = Array2.map_file p (Array2.kind ba) (Array2.layout ba) 
            true (Array2.dim1 ba) (Array2.dim2 ba) in
    (* Copy it into the file. *)
    Array2.blit ba f;
    Unix.close p    

let fromfile fn = 
  let p = Unix.openfile fn [Unix.O_RDONLY] 0o644  in
  let f = Array2.map_file p int8_unsigned c_layout false maxx maxy in
    draw_screen f
    
;;

(fromfile (Sys.argv.(1))) ;;
(printf "YAY\n");;

input_line stdin;


