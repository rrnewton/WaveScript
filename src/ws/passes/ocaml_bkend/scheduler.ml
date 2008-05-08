





(* TODO: make this int64 *)
type timestamp = int

(* Data sources are functions that generate new schedule entries *)
type scheduleEntry = SE of (timestamp * (unit -> scheduleEntry))

let schedule : scheduleEntry list ref = ref []

(*
let default_baseSink = 
  let counter = ref 0 in 
    fun x -> 
      incr counter;
      if !counter == 1000000
      then (counter := 0; 
	    print_string "Got element !\n";
	    flush stdout);;
*)

let rec insert (SE(t1,f)) ls =
  match ls with 
    | [] -> [SE(t1,f)]
    | SE(t2,g)::tl -> 
	if t1 < t2 
	then SE(t1,f) :: ls	
	else SE(t2,g) :: insert (SE(t1,f)) tl;;

let rec insert2 x ls =
  match ls with 
    | [] -> [x]
    | h::t -> if x < h then x::ls else h :: (insert2 x t);;

let runScheduler () = 
  while not(!schedule == []) do
    let next = 
      match List.hd !schedule with 
	  SE (t, f) -> 
	    (* Printf.printf "  Running... %d\n" t; *)
	    f()
    in      
      (* schedule := insert next (List.tl !schedule);  *)
      schedule := insert2 next (List.tl !schedule);
      (* schedule := List.merge compare [next] (List.tl !schedule); *)
  done;;




(********************************************************************************)


(* Should do this with functors, but not entirely sure about modules and inlining. *)

(* VER1: Normal arrays: *)
(*
type ('t,'ign) wsarray = 't array
let wslen       = Array.length
let wsget       = Array.get
let wsset       = Array.set
let wssub       = Array.sub
let wsnull t    = [||]
let wsmakearr t = Array.make
let wsappend    = Array.append
*)

(* VER2: Bigarray library *)
(*
type ('a,'b) wsarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
let wslen       = Bigarray.Array1.dim
let wsget       = Bigarray.Array1.get
let wsset       = Bigarray.Array1.set
let wssub       = Bigarray.Array1.sub
let wsnull t    = Bigarray.Array1.create t Bigarray.c_layout 0
let wsmakearr t n x  = 
  let arr = Bigarray.Array1.create t Bigarray.c_layout n in
    Bigarray.Array1.fill arr x;
    arr  
    *)

let wsappend a b = 
  let len1 = Bigarray.Array1.dim a
  and len2 = Bigarray.Array1.dim b in
  let newarr = Bigarray.Array1.create 
    (Bigarray.Array1.kind a) (Bigarray.Array1.layout a) (len1 + len2)
  in 
    for i = 0 to len1 -1 do
      Bigarray.Array1.set newarr i (Bigarray.Array1.get a i);
    done;
    for i = len1  to len1 + len2 - 1 do
      Bigarray.Array1.set newarr i (Bigarray.Array1.get b (i-len1));
    done;
    newarr   

