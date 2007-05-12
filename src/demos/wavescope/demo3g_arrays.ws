
fun assert_eq(a,b) if not(a==b) then wserror("Assert failed: "++ a ++" not equal "++ b);

// Audio channel 1 with no overlap.
s1 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int));

/* s2 = iterate(w in s1) { */
/*   arr = makeArray(3, 99.9); */
/*   print(show(arr[0]) ++ "\n"); */
/*   emit (); */
/* } */

s2 = iterate( w in s1 ) {
  print("Building Array\n");
  arr = Array:make(3, 0.0);
  assert_eq(Array:length(arr), 3);
  emit arr;
};

s3 = iterate( arr in s2) {
  assert_eq(arr[0], 0.0);
  assert_eq(arr[1], 0.0);
  print("Initially: "++ show(arr[0]) ++" "++ show(arr[1]) ++"\n");
  arr[0] := 3.0;
  arr[1] := 4.0;
  print(" Assigned: "++ show(arr[0]) ++" "++ show(arr[1]) ++"\n");
  assert_eq(arr[0], 3.0);
  assert_eq(arr[1], 4.0);
  emit arr;
}

BASE <- s3;

