


// [2007.06.22]
// Just got MLton working on this demo.
// Benchmarks... This stresses reading from the file + array allocation.
// Taking total process time here (except for Scheme):
// On Faith:
//   Caml 3.09.2                  :   5.8   seconds
//   MLton                        :   2.3
//   WSC (with new struct-arrays) :   6.7 
//   Scheme                       :   4.5 - 5.6
// On my core2 duo laptop:
//   Caml 3.09.2                  :   15.6   seconds  Strange!
//   MLton                        :   1.4    

// [2007.06.24] This takes 6.5 seconds on breeze and 67 seconds on an ENSbox.

// [2007.06.25]Oops, above must not have been with -O3 for WSC!
// Doing it agaain, using the default countup.raw from the repository:
// On Faith:
//   Caml -O3                     :   5.8   seconds
//   MLton                        :   2.3
//   WSC -O3                      :   1.0
//   ws.opt                       :   4.3

// To be fair Caml & Scheme aren't really using Int16s... only MLton & c++ are.
// I should also revisit my file reading code in C++...

/* [2008.03.24]
  Just ran this again with the new wsjava backend.
  Using sun's jvm, running ten tuples takes 2 seconds in java, 1.9 Scheme,
  .8 mlton and .4 wsc2/gcc.
*/


//fun assert_eq(a,b) if not(a==b) then wserror("Assert failed: "++ a ++" not equal "++ b);

include "common.ws";

// Audio channel 1 with no overlap.
//s1 = (readFile("./countup.raw", "mode: binary  window: 15000", timer(1.0)) :: Stream (Sigseg Int16));
s1 = timer(10.0)

/* s2 = iterate(w in s1) { */
/*   arr = makeArray(3, 99.9); */
/*   print(show(arr[0]) ++ "\n"); */
/*   emit (); */
/* } */

s2 = iterate( _ in s1 ) {
  print("Building Array\n");
  arr = Array:make(3, 0.0);
  assert_eq("",Array:length(arr), 3);
  emit arr;
};

s3 = iterate( arr in s2) {
  assert_eq("",arr[0], 0.0);
  assert_eq("",arr[1], 0.0);
  print("Initially: "++ show(arr[0]) ++" "++ show(arr[1]) ++"\n");
  arr[0] := 3.0;
  arr[1] := 4.0;
  print(" Assigned: "++ show(arr[0]) ++" "++ show(arr[1]) ++"\n");
  assert_eq("",arr[0], 3.0);
  assert_eq("",arr[1], 4.0);

  assert_eq_prnt("zerolen", Array:length(Array:null), 0);

  print(" Stressing array allocation and GC.\n");
  results = Array:make(1,0);
  for i = 1 to 10000 {
    temp = Array:make(10000,34);
    t2 = temp;
    results[0] := results[0] + Array:length(t2);
  }
  print(" Iterated "++ results[0]/10000 ++" times\n");


  emit arr;
}

main = s3;

