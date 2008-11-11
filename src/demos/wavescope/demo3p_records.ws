

//include "stdlib.ws"


fun testit() { 
  empt = {|};
  print(empt ++ "\n"); 

  rec = (| A=3, B=true);
  print(rec ++ "\n"); 

  rec2 = (rec | A="yay", C=99.9 );
  print(rec2 ++ "\n");

  print("And a little projection: " ++ rec2.A ++" "++ rec2.B ++"\n");

  rec3 = ( if true then (rec2 | D='t') else (rec2 | D='f') | ~A, B := () );
  print(rec3 ++ "\n");

  print("And more projection: "++ rec3.A ++"\n");

  // [2008.11.11] This causes lingering polymorphism... investigate:
  //print("A stack of identical names: " ++ (| A=1, A=(), A=true, A=(3,4), A="str") ++ "\n");
}

_ = testit();

main = iterate _ in 3 . timer {
  // Uh oh... parser problem... can't have JUST a variable binding in a block.  WITHOUT a semicolon. 

  testit();

/*  
  x :: (B:Int, A:Int) = (| A=3, B=4); 
  x :: ( p | B:Int, A:Int) = (| A=3, B=4); 
  x = (A=3, B=4); 
  y = ( x | c=4, d=5 );  

  z = (|);

  //test = x.A;
  
  //x = (a:3, b:4); 
  //x = {a:3, b:4}; 

  //x = (A:3, B:4); 
  //x = {A:3, B:4}; 

  x = {| a=3, b=4 }; 
  y = {x| c=4, d=5 };
  z :: (|) = {|};
*/
  emit 3;
}


/*

  \r → if True then {x = 2 | r } else {y = 2 | r }
  \r → if True then {x = 1 | r } else {x = 2 | {}}


 */

