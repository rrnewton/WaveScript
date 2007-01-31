
//include "stdlib.ws";

// Test various primitives in WS.

BASE <- iterate (x in audioFile("./countup.raw", 1000, 0)) {
  state { run = true }
  if run then {

    ls = [1, 2, 3];

    fun plus(x,y) x +_ y;
    
// fold 
// alist_*

// atan2
// 
    print("\n");
    print("fold: "++ show(fold(plus, 0, ls)) ++"\n");

    alst = [("foo",33), ("a", 1)];

    print("alistLookup: "++ show(alist_lookup(alst, "a")) ++"\n");
    print("alistLookup: "++ show(alist_lookup(alst, "b")) ++"\n");

    run := false;
  };
  emit ();
}
