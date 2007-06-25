
s0 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int16));

fun println(s) {
  print("  ");
  print(s);
  print("\n");
};

BASE <- iterate(w in s0) {  
  n = w`width;
  i = n`intToInt16;
  f = n`intToFloat;
  c = n`intToComplex;

  println("roundF: " ++ roundF(f + 0.6));

  println("trig: "++ f`sin ++" "++ f`cos ++" "++ f`tan);
  println("reverse trig: "++ f`asin ++" "++ f`acos ++" "++ f`atan);

  emit ();
}
