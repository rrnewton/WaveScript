
//include "stdlib.ws";

// Test various primitives in WS.

BASE <- iterate (x in (readFile("./countup.raw", "mode: binary  window: 1000") :: Stream (Sigseg Int))) {
  state { run = true }
  if run then {

    ls = [1, 2, 3];

    fun plus(x,y) x +_ y;
    
    print("\n");
    print("fold: "++ show(fold(plus, 0, ls)) ++"\n");

    alst = [("foo",33), ("a", 1)];

    print("alistLookup: "++ show(List:assoc(alst, "a")) ++"\n");
    print("alistLookup: "++ show(List:assoc(alst, "b")) ++"\n");
    print("alistUpdate: "++ show(List:assoc_update(alst, "a", 99)) ++"\n");

    print("realpart: "++ show(realpart(3.0+4.0i)) ++"\n");
    print("imagpart: "++ show(3.0+4.0i . imagpart) ++"\n");

    print("string conversions: " ++ stringToInt("3") ++ stringToFloat("3.0") ++ stringToComplex("3.0+4.0i"));

    // atan2

    run := false;
  };
  emit ();
}
