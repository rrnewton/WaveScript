// Test all those other list primitives.

//include "stdlib.ws";

/*
fun mywindow(S, len)
  iterate(x in S) {
    state{
      arr = Array:null;
      ind = 0;
      startsamp = 0;
    }
    if ind == 0 then arr := Array:make(len, x);
    arr[ind] := x;
    ind := ind + 1;
    if ind == len
    then {
      emit toSigseg(arr, startsamp, nulltimebase);
      ind := 0;
      arr := Array:make(len, x);
      startsamp := startsamp + len;
    }
  };
*/


fun assert(str,b) {
  if not(b)
  then wserror("Assert failed: "++ str ++"\n");
}

// Audio channel 1 with no overlap.
s1 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int16));

// Test reverse:
s2 = iterate( w in s1 ) {
  emit List:reverse([w.start, w.end]);
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
  state { myls :: List Int = [3,4,5,6] }
  // Assignment overwriting state:
  myls := ls;
  
  print("\n  Test "++ show(ls==ls) ++" "++ show([]==[]) ++"\n");
  assert("self equality",ls==ls);
  assert("nulls equal",[]==[]);

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
BASE <- s3;

//BASE <- s3b;
