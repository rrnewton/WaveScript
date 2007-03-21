





(* TODO: make this int64 *)
type timestamp = int

(* Data sources are functions that generate new schedule entries *)
type scheduleEntry = SE of (timestamp * (unit -> scheduleEntry))

let schedule : scheduleEntry list ref = ref []

let baseSink = 
  let counter = ref 0 in 
    fun x -> 
      incr counter;
      if !counter == 1000000
      then (counter := 0; 
	    print_string "Got element !\n";
	    flush stdout);;


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

