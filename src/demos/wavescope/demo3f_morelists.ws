// Test all those other list primitives.

// [2007.09.15] Having a problem with static-elaborate on this pass curretly.
// It runs into the int64ToInt app as an argument to cons and gets messed up.

//include "stdlib.ws";


fun assert(str,b) if not(b) then wserror("Assert failed: "++ str ++"\n");
fun assert_eq(s,a,b) if not(a==b) then wserror("Assert failed in '"++s++"' : "++ a ++" not equal "++ b);

// Audio channel 1 with no overlap.
s1 = (readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) :: Stream (Sigseg Int16));

// Test reverse:
s2 = iterate( w in s1 ) {
  emit List:reverse([w.start.int64ToInt, w.end.int64ToInt]);
};

s3 = iterate ( x in s2) { emit append(x,x) }

// Test append:

/*
s3 :: Stream (List Int);
s3 = iterate( w in mywindow(s2, 2)) {
  emit(append(w[[0]], w[[1]]));
  //emit [10,11,12,13];
  //emit append([10,11], [12,13]);
 }
*/


// Test equality, printing:
s4 = iterate( ls in s3) {
  state { myls :: List Int = [3,4,5,6];
          testappend = [] }
  // Assignment overwriting state:
  myls := ls;
  
  print("\n  Test "++ show(ls==ls) ++" "++ show([]==[]) ++"\n");
  assert("self equality",ls==ls);
  assert("nulls equal",[]==[]);
  assert("null inequality",not([1]==[]));

  testappend := List:append(testappend, [99]);
  print(" testappend after append: "++ testappend  ++"\n");
  assert("null inequal2", (not (testappend == [])));  
  assert("tail lens", List:length(testappend) > 0);

  print("  Manually printed: "++ 
	show(myls.head) ++" "++ 
	show(myls.tail.head) ++" "++ 
	show(myls.tail.tail.head) ++" "++ 
	show(myls.tail.tail.tail.head) ++"\n");
  print("  Printed at once: "++ myls ++"\n");
  print("  Length of ORIG copy: "++ ls`List:length ++"\n");
  print("  Length before emit: "++ myls`List:length ++"\n");

  print("  Manually printed after length check "++ 
	show(myls.head) ++" "++ 
	show(myls.tail.head) ++" "++ 
	show(myls.tail.tail.head) ++" "++ 
	show(myls.tail.tail.tail.head) ++"\n");
  print("  Manually (ORIG)printed after length check "++ 
	show(ls.head) ++" "++ 
	show(ls.tail.head) ++" "++ 
	show(ls.tail.tail.head) ++" "++ 
	show(ls.tail.tail.tail.head) ++"\n");


  //  emit ls;
  emit myls;
}

// Test List:ref, List:length, List:make.
s5 = iterate(ls in s4) {

  print("  Manually printed FIRST 3 after emit: "++ 
	show(ls.head) ++" "++ 
	show(ls.tail.head) ++" "++ 
	show(ls.tail.tail.head) ++" "++        
	"\n");

  print("  Length after emit: " ++ ls.List:length ++"\n");
  assert("check length", ls.List:length == 4);
  print("  Second element: "++ ls.List:ref(1) ++"\n");
  assert("check List:ref",ls.List:ref(1) == ls.tail.head);
  ml = List:make(3, 0.0);
  print("  makeList: "++ ml ++ " Length: "++ ml.List:length ++"\n");
  emit ();
}

// No problem
//BASE <- s2;

// This has no problem
//BASE <- iterate x in s2 { emit List:append(x,x) };

// PROBLEM!
//BASE <- s3;
//BASE <- s3b;

BASE <- s5;
