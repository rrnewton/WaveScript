


uniontype Foo = A Int | B Float | C ();

//res2 = match (1,2) { (a,b) : b }

BASE <- iterate _ in timer(3.0) { 

  x = A(3);
  y = (+ 3) $ 4;
  f = (3 *);
  z = y`f + y.f;

  print("A data value: "++ x ++ "\n");

  res = case x {
    A(x) : 39
    C(z) : 999
  }; 

  print("Result of match: "++ res ++ "\n");

  print("Another match: "++ case C(()) { A(x) : 39  C(z) : 100 } ++ "\n");

  //  print(res2 ++ "\n");
  emit x 
}


