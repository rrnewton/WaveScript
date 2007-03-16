
s0 = audioFile("./countup.raw", 4096, 0, 44000);

fun println(s) {
  print("  ");
  print(s);
  print("\n");
};

BASE <- iterate(w in s0) {  
  n = w\width;
  i = n\intToInt16;
  f = n\intToFloat;
  c = n\intToComplex;

  println("roundF: " ++ roundF(f + 0.6));

  println("trig: "++ f\sin ++" "++ f\cos ++" "++ f\tan);
  println("reverse trig: "++ f\asin ++" "++ f\acos ++" "++ f\atan);

  emit ();
}
