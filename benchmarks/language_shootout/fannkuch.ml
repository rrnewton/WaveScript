open Array open Printf

(*global variables*)
let n = try if length Sys.argv>1 then int_of_string Sys.argv.(1)else 7  with _->7
let r = init(n+2)(fun x -> x-1) and p=init n((+)1) and s=create n 0

(*pretty printing function*)
let q() = iter print_int p;print_newline()

(*counting permutations*)
let rec a n = r.(n)<-(r.(n)+1);
  if r.(n)=n-1 then a(n+1)
  else (if r.(n)=n then r.(n)<-0;n)

(*swapping arrays*)
let w m= let rec a i=i=n||(p.(i)<>(i+1)&&a(i+1))in
if a 0 then
  (for i=0 to n-1 do s.(i)<-p.(i)done;
   let rec y m= let x=s.(0)-1 in
   if x=0 then m
   else (for i=0 to((x-1) lsr 1)do
	   let t=s.(i)in let o = x-i in s.(i)<-s.(o);
	   s.(o)<-t done;y(m+1))
   in y m) else 0

(*building new permutations*)
let x n =
  for i=1 to n-1 do let t=p.(0)in
  for j=0 to i-1 do p.(j)<-p.(j+1) done; p.(i)<-t done

(* main *)
let _ = let rec f i m z= (* printing loop *)
  if i <=n && z>0
  then(q();x i;f(a 2)(max m(w 0))(z-1))
  else (if z>0 then q();g i m)
	and g i m= if i <=n (* non printing loop *)
	then(x i; g(a 2)(max m(w 0)))
	else m in
printf "Pfannkuchen(%i) = %i\n" n (f (a 2) 0 30)

