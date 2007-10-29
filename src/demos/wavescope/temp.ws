
include "stdlib.ws";

//s0 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int));
s1 = (readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) :: Stream (Sigseg Int16));

//	       "mode: binary  rate: 44000  repeats: 0 "++
//	       "skipbytes: 2  window: 50 offset: 2")


//s1 = deep_smap(int16ToInt, s0);

fun assert(str,b) if not(b) then wserror("Assert failed: "++ str ++"\n");
fun assert_eq(s,a,b) if not(a==b) then wserror("Assert failed in '"++s++"' : "++ a ++" not equal "++ b);

fun assert_prnt(str,a,b) {
  assert_eq(str,a,b);
  print("Assert passed: "++ str ++ "\n");
}

fun println(str) {
  print("  ");
  print(str);
  print("\n");
};


BASE <- iterate(w in s1) {  
  arr = toArray(subseg(w, w.start, 20));

  arr2 = Array:map((/ 100), arr);

  print("LENGTH1: "++ arr`Array:length ++"\n");
  print("LENGTH2: "++ arr2`Array:length ++"\n");

  emit arr2;
}
