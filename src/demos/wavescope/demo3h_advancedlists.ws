
// This tests all the additional list primitives.
//  reverse, List:ref, etc.

// Audio channel 1 with no overlap.
//s1 = timer(30.0);
s1 = (readFile("./countup.raw", "mode: binary  window: 4096",timer(10.0)) :: Stream (Sigseg Int));

s2 = iterate( w in s1 ) {
  emit List:reverse([w.start, w.end]);
};

s3 = iterate( x in s2) {
  emit List:append(x, x);
  //emit [10,11,12,13];
  //emit append([10,11], [12,13]);
 }

s4 = iterate( ls in s3) {
  state { myls = [3,4,5,6] }
  myls := map(int64ToInt, ls);
  
  print("  Test "++ show(ls==ls) ++" "++ show([]==[]) ++"\n");
  print(show(myls.head) ++" "++
	show(myls.tail.head) ++" "++
	show(myls.tail.tail.head) ++" "++
	show(myls.tail.tail.tail.head) ++"\n");
  //emit myls;
  emit ();
}

BASE <- s4;

