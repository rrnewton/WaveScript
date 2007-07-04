include "stdlib.ws";

// [2007.04.09] Hmm... having a problem with this not elaborating
// properly if I do a using rather than Array:fold.

// OH! This has to do with the source positions.

// [2007.06.29] Disabling this till it works in more backends.
//src = union2(timer(3.0), timer(4.0))
src = timer(3.0);

BASE <- iterate (x in src) {
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

    /*
    print("Type Unions: ");
    case x {
      Oneof2(x): println("Got left! ")
      Twoof2(y): println("Got right! ")
    };
    */

    {
       print("List primitives: ");
       ls1 = [1,2,3];
       ls2 = [4,5,6];        
       summed = List:map2((+),ls1,ls2);
       assert_eq("List:map2", [5,7,9], summed);
       println("passed");
    };
    
    
  };
  
  emit true;
}
