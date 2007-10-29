



// This is a benchmark from the "great language shootout"
// Transcribed from the ocaml version.

include "stdlib.ws";

// open Array open Printf
using Array;

// global variables
n = 7
r = build(n+2, fun(x) x-1)
p = build(n  , (+ 1))
s = make(n,0)

// counting permutations
fun a(init) {
  go = Mutable:ref(true);
  n  = Mutable:ref(init);
  while go {
    r[n] := r[n] + 1;  
    if r[n] == n-1 then () else {
      if r[n] == n   then r[n] := 0;
      go := false
    }
  };
  n
}

/* swapping arrays */
fun w(m) {
  fun a2(ind) {
    i = Mutable:ref(ind);
    stilltrue = Mutable:ref(true);
    while i < n && stilltrue {
      if p[i] == i+1 
      then stilltrue := false
      else i += 1; // Keep going
    };
    stilltrue;
  };

  if a2(0) then {
    // Copy p to s:
    for i=0 to n-1 { s[i] := p[i] };
    
    fun y(m) {
      x = s[0]-1;
      if x==0 then m
      else {
        for i=0 to ((x-1) / 2) { // lsr 2
	  t = s[i];
	  o = x-i;
	  s[i] := s[0];
	  s[o] := t;
	  y(m+1)
	}
      }
    };
    y(m);
  } else 0
}


/*


let w m= let rec a i=i=n||(p.(i)<>(i+1)&&a(i+1))in
if a 0 then
  (for i=0 to n-1 do s.(i)<-p.(i)done;
   let rec y m= let x=s.(0)-1 in
   if x=0 then m
   else (for i=0 to((x-1) lsr 1)do
	   let t=s.(i)in let o = x-i in s.(i)<-s.(o);
	   s.(o)<-t done;y(m+1))
   in y m) else 0

/* building new permutations */
let x n =
  for i=1 to n-1 do let t=p.(0)in
  for j=0 to i-1 do p.(j)<-p.(j+1) done; p.(i)<-t done

/*  main  */
let _ = let rec f i m z= /*  printing loop  */
  if i <=n && z>0
  then(q();x i;f(a 2)(max m(w 0))(z-1))
  else (if z>0 then q();g i m)
	and g i m= if i <=n /*  non printing loop  */
	then(x i; g(a 2)(max m(w 0)))
	else m in
printf "Pfannkuchen(%i) = %i\n" n (f (a 2) 0 30

*/



BASE <- ONCE(fun() {
  print("\nRunning!\n");

  println("r: "++r);
  println("p: "++p);
  println("s: "++s);
  
})



/// RECURSIVE VERSIONS: ///
/*

fun a(n) {
  r[n] := r[n] + 1;  
  if r[n] == n-1      then a(n+1)
  else if r[n] == n   then { r[n] := 0; n }
}

fun w(m) {
  fun a(i) (i==n || (p[i] != i+1 && a(i+1)));

  if a(0) then {
    for i=0 to n-1 { s[i] := p[i] };
    fun y(m) {
      x = s[0]-1;
      if x==0 then m
      else {
        for i=0 to ((x-1) / 2) { // lsr 2
	  t = s[i];
	  o = x-i;
	  s[i] := s[0];
	  s[o] := t;
	  y(m+1)
	}
      }
    };
    y(m);
  } else 0
}


*/
