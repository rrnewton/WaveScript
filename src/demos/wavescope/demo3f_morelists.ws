// Test all those other list primitives.

fun assert(b) {
  if not(b)
  then wserror("Assert failed.");
}

// Audio channel 1 with no overlap.
s1 = audioFile("./countup.raw", 4096, 0, 44000);

// Test reverse:
s2 = iterate( w in s1 ) {
  emit reverse([w.start, w.end]);
};

// Test append:
s3 :: Stream (List Int);
s3 = iterate( w in prim_window(s2, 2)) {
  emit(append(w[[0]], w[[1]]));
  //emit [10,11,12,13];
  //emit append([10,11], [12,13]);
 }


// Test equality, printing:
s4 = iterate( ls in s3) {
  state { myls :: List Int = [3,4,5,6] }
  // Assignment overwriting state:
  myls := ls;
  
  print("\n  Test "++ show(ls==ls) ++" "++ show([]==[]) ++"\n");
  assert(ls==ls);
  assert([]==[]);

  print("  Manually printed: "++ 
	show(myls.head) ++" "++ 
	show(myls.tail.head) ++" "++ 
	show(myls.tail.tail.head) ++" "++ 
	show(myls.tail.tail.tail.head) ++"\n");
  print("  Printed at once: "++ ls ++"\n");
  //emit myls;
  emit ls;
}

// Test List:ref, listLength, makeList.
s5 = iterate(ls in s4) {
  print("  Length: " ++ ls.listLength ++"\n");
  assert(ls.listLength == 4);
  print("  Second element: "++ ls.List:ref(1) ++"\n");
  assert(ls.List:ref(1) == ls.tail.head);
  ml = makeList(3, 0.0);
  print("  makeList: "++ ml ++ " Length: "++ ml.listLength ++"\n");
  emit ();
}

BASE <- s5;

