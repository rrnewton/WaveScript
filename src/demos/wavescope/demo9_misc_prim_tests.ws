
//include "stdlib.ws";

// Test various primitives in WS.

fun assert(str, bool) {
  if not(bool)
  then { 
    wserror("assert FAILED: "++str);
  }
}

main = iterate (_ in (readFile("./countup.raw", "mode: binary  window: 1000", timer(10.0)) :: Stream (Sigseg Int16))) {
  state { 
    run = true;
    arr1 = Array:null;
    ls1  = [];
  }
  if run then {

    //    x = int16ToInt(_x);

    ls = [1, 2, 3];

    fun plus(x,y) x +_ y;
    
    print("\n");
    //  print("fold: "++ show(fold(plus, 0, ls)) ++"\n");

    alst = [("foo",33), ("a", 1)];

    //    print("alistLookup: "++ show(List:assoc(alst, "a")) ++"\n");
    //    print("alistLookup: "++ show(List:assoc(alst, "b")) ++"\n");
    //    print("alistUpdate: "++ show(List:assoc_update(alst, "a", 99)) ++"\n");

    print("realpart: "++ show(realpart(3.0+4.0i)) ++"\n");
    print("imagpart: "++ show(3.0+4.0i . imagpart) ++"\n");

    print("string conversions: " ++ stringToInt("3") ++ stringToFloat("3.0") ++ stringToComplex("3.0+4.0i") ++"\n");

    // ============================================================
    // Test semantics against constants.  This should test the elaborator.


    // ============================================================
    print("Testing equality.\n");
    assert("array equality, null==null", arr1 == (Array:null :: Array ()));
    assert("list equality, []==[]",      ls1  == ([] :: List Int));
    ls1 := [1,2,3];
    assert("non-null list equality",     ls1  == [1,2,3]);
    ls1 := [1,2,3,4];
    assert("non-null list inequality",   not(ls1 == [1,2,3]));
    //assert("false",false);
   
    // atan2

    run := false;
  };
  emit ();
}
