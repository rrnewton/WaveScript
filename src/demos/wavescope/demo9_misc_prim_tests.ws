
//include "stdlib.ws";

// Test various primitives in WS.

BASE <- iterate (x in audioFile("./countup.raw", 1000, 0)) {
  state { run = true }
  if run then {

    ls = [1, 2, 3];

    fun plus(x,y) x +_ y;
    
    print("\n");
    print("fold: "++ show(fold(plus, 0, ls)) ++"\n");

    alst = [("foo",33), ("a", 1)];

    print("alistLookup: "++ show(alist_lookup(alst, "a")) ++"\n");
    print("alistLookup: "++ show(alist_lookup(alst, "b")) ++"\n");
    print("alistUpdate: "++ show(alist_update(alst, "a", 99)) ++"\n");

    print("realpart: "++ show(realpart(3.0+4.0i)) ++"\n");
    print("imagpart: "++ show(3.0+4.0i . imagpart) ++"\n");

    // atan2

    run := false;
  };
  emit ();
}
