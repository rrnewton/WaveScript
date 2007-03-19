include "stdlib.ws";

BASE <- iterate (() in timer(3.0)) {
  state { first = true }
  
  if first then {
    println("\n");
    {
      using Array;
      arr = build(10, fun(x) x);
      println("Fold:  " ++ fold((+), gint(0), arr));
      println("Fold1: " ++ fold1((+), arr) ++ "  (should be same as previous)");

      //flipped = fun(x,y) (y:::x); 
      println("FoldCons:  " ++ fold(fun(x,y)(y:::x), [], arr));

      println("FoldRange: " ++ foldRange(3, 4, 0, (+)));
      println("FoldRange: " ++ foldRange(3, 7, [], fun(x,y)(y:::x)));
    };

    first := false;
    println("");
  };
  
  emit ();
}
