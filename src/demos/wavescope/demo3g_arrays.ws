


// Audio channel 1 with no overlap.
s1 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int));

/* s2 = iterate(w in s1) { */
/*   arr = makeArray(3, 99.9); */
/*   print(show(arr[0]) ++ "\n"); */
/*   emit (); */
/* } */

s2 = iterate( w in s1 ) {
  print("Building Array\n");
  emit Array:make(3, 0.0);
};

s3 = iterate( arr in s2) {
  print("Initially: "++ show(arr[0]) ++" "++ show(arr[1]) ++"\n");
  arr[0] := 3.0;
  arr[1] := 4.0;
  print(" Assigned: "++ show(arr[0]) ++" "++ show(arr[1]) ++"\n");
  emit arr;
}

BASE <- s3;

