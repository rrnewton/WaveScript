
s0 = (readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) :: Stream (Sigseg Int16));

fun println(s) {
  print("  ");
  print(s);
  print("\n");
};

fun assert(str,b) 
  if b 
  then print("passed: "++str++"\n")
  else wserror("Assert failed: "++ str ++"\n");
fun assert_eq(str,a,b) if a==b 
  then print("passed: "++str++"\n")
  else wserror("Assert failed: "++str++ " "++ a ++" not equal "++ b);

BASE <- iterate(w in s0) {  
  n = w`width;
  i = n`intToInt16;
  l = n`intToInt64;
  f = n`intToFloat;
  c = n`intToComplex;

  assert_eq("additive identity", l, l + gint(0));
  
  println("roundF: " ++ roundF(f + 0.6));

  println("trig: "++ f`sin ++" "++ f`cos ++" "++ f`tan);
  println("reverse trig: "++ f`asin ++" "++ f`acos ++" "++ f`atan);

  println("sqrt complex: "++ sqrtC(3.0+4.0i) );  

  assert_eq("complex equality", 2.0+1.0i, 2.0+1.0i);
  assert("complex inequality", not(2.0+1.0i == 2.0+9.0i));

  //print("diff " ++ sqrtC(3.0+4.0i) - 2.0+1.0i ++ "\n");

  assert("complex sqrt", absC(sqrtC(3.0+4.0i) - 2.0+1.0i) < 0.000001);

  emit ();
}
