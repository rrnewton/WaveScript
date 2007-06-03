include "stdlib.ws";

// [2007.04.09] Hmm... having a problem with this not elaborating
// properly if I do a using rather than Array:fold.

// OH! This has to do with the source positions.

src = union2(timer(3.0), timer(4.0))

BASE <- iterate (_ in src) {
  state { first = true }
  
  if first then {
    println("\n");
    {
      using Array;
      arr = build(10, fun(x) x);
      println("Fold:  " ++ Array:fold((+), 0, arr));
      println("Fold1: " ++ Array:fold1((+), arr) ++ "  (should be same as previous)");

      //flipped = fun(x,y) (y:::x); 
      //      println("FoldCons:  " ++ Array:fold(fun(x,y)(y:::x), [], arr));

      //      println("FoldRange: " ++ Array:foldRange(3, 4, 0, (+)));
      //      println("FoldRange: " ++ Array:foldRange(3, 7, [], fun(x,y)(y:::x)));
    };

    first := false;
    println("");


  };
  
  emit ();
}
