
// fun bar(y) {  return 3;  } 

fun foo(x) {
  //  return 3; 
  3;
}

// fun bar(y) {  return 3 + (4 * 8);  } 
fun bar(y) {3 + (4 * 8);}


baz : list list (int);
fun baz(s) {
  iterate (w in s) { }; 
  iterate (w in s) { emit 3; };

  newv : int = {{{3;};};};
//  neww : bool;
  neww = {{{4}}};

  map (x in s) {x + 3};

  map (x in map (y in S) {y+3}) {
    x*4
  };

  a = if true then 3 else 4 + 1;
  a := if true then {3} else (4 + 1);

  inner : (int, 'a) -> (int, 'b);
  fun inner(x) {x};
  
  c = 'c';

  ls = [1,2];
  
  x = (1 : int);

  f(x,y);
  
  w.width;

  (3 >= 4);  

  x = fun(x){x};

}


(* 

if 3 then x;
if (4) {x};  
(3 + if true then 4 else 5)
(3 + if (true) {4} else {5})
(3 + if true {4} else {5})

(if x<4 then {
   4; 5; 6
 } else 7)

fun conds(x,y) {
  if (x) { x+y; } else { x-y; };
}
*)




(*  filter (x in s) iseven(x);
    
  f(x:=x+5);
  map(\x.x+3, s);
  
  map(fun(x) {return x+3;}, S);  

  
    s2 = map(fun x.x+3, s1); 
    s2 = map (x in s1) x+3;  

      map(\x . x+3, 
      map(\x . x*4, S))

 {3; 4; 5}
*)

