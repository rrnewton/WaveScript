





(* TODO: make this int64 *)
type timestamp = int

(* Data sources are functions that generate new schedule entries *)
datatype scheduleEntry = SE of (timestamp * (unit -> scheduleEntry))

val schedule : scheduleEntry list ref = ref []


fun print_endline x = (print x; print "\n")

fun insert (SE(t1,f)) ls =
  case ls 
   of [] => [SE(t1,f)]
    | SE(t2,g)::tl => 
	if t1 < t2 
	then SE(t1,f) :: ls	
	else SE(t2,g) :: insert (SE(t1,f)) tl

(* Hmm this one used caml's very overloaded < operator... *)
fun insert2 x ls =
  case ls 
   of [] => [x]
    | h::t => if x < h then x::ls else h :: (insert2 x t)

fun runScheduler () = 
  while not (null (!schedule)) do 
    let val next = 
       case List.hd (!schedule)
       of SE (t, f) => f()
    in      
      (* schedule := insert next (List.tl !schedule);  *)
      schedule := insert next (List.tl (!schedule))
      (* schedule := List.merge compare [next] (List.tl !schedule); *)
end


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

(*

fun wsappend a b = 
  let val len1 = Bigarray.Array1.dim a
      val len2 = Bigarray.Array1.dim b 
      val newarr = Bigarray.Array1.create 
                   (Bigarray.Array1.kind a) (Bigarray.Array1.layout a) (len1 + len2)
  in 
()
(*
    for i = 0 to len1 -1 do
      Bigarray.Array1.set newarr i (Bigarray.Array1.get a i);
    done;
    for i = len1  to len1 + len2 - 1 do
      Bigarray.Array1.set newarr i (Bigarray.Array1.get b (i-len1));
    done;
    newarr   
*)
  end

**)
