
open Printf;;
open Bigarray;;
open Graphics;;
    
(********************************************************************************)

(* simple bit arrays *)


(*
let bitarray n m = Array.make_matrix n m 0
let get ba i j = ba.(i).(j)
let set ba i j x = ba.(i).(j) <- x
let xdim ba = Array.length ba
let ydim ba = Array.length ba.(0)
*)





(* Let's try just using a mapped file from the start: *)
let bitarray fn n m = 
  (*Array2.create int8_unsigned c_layout  n m;;*)
  let p = Unix.openfile fn [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC] 0o644  in
    Array2.map_file p int16_unsigned c_layout true n m

let get ba i j   = Array2.get ba i j
let set ba i j x = Array2.set ba i j x
let xdim ba    = Array2.dim1 ba 
let ydim ba    = Array2.dim2 ba   


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
let xdim ba = Array2.dim1 ba * 16
let ydim ba = Array2.dim2 ba   
*)

(********************************************************************************)


let the_maxx = ref 0 
let the_maxy = ref 0 

(* Allow scrolling around with mouse. *)
let scrollx = ref 0 
let scrolly = ref 0 

let draw_screen arr = 
  clear_graph();
(*   for i = 0 to xdim arr - 1 do *)
(*     for j = 0 to ydim arr - 1 do *)
(*       if 1 == get arr i j  *)
(*       then plot (i - shiftx + !scrollx) (j - shifty + 300 + !scrolly) *)
(*     done  *)
(*   done; *)
  for x = 0 to size_x()-1 do
    for y = 0 to size_y()-1 do
      let i = x + !scrollx in
      let j = y + !scrolly in 
      if 0 != get arr i j 
      then plot x y
    done 
  done;  
  printf "drawn! screen location: %d %d\n" (!scrollx) (!scrolly);
  flush stdout;
  synchronize ()
;;


let update_scroll = 
  let lastmousepos = ref None in
  fun () -> 
    let (x,y) = mouse_pos() in 
    let width = size_x() in
    let height = size_y() in
    if button_down() &&
       x >= 0 && x < width && 
       y >= 0 && y < height 
    then 
      let (oldx,oldy) = match !lastmousepos with
          None ->
	    let p = mouse_pos() in 
	      lastmousepos := Some p; 
	      p
	|	Some p -> p 
      in	
      let (dx,dy) = (x-oldx, y-oldy) in
      printf "  SCROLLING: %d %d\n" dx dy;
      scrollx := min (!the_maxx - width) (max 0 (!scrollx - dx));
      scrolly := min (!the_maxy - height) (max 0 (!scrolly - dy));
      lastmousepos := Some (x,y)
    else lastmousepos := None

