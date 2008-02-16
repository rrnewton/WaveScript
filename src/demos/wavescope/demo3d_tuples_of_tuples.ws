

// Audio channel 1 with no overlap.
//s1 = (readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) :: Stream (Sigseg Int16));
include "common.ws";

s1 = timer(1.0);

s2 = iterate( w in s1 ) {
  //emit (w.width, w.start);
  emit (10, 20);
};

s3 = iterate((x,y) in s2) {
  //assert_prnt("comparison", (1,(2,3)) == (1,(2,3)));
  //assert_prnt("negative comparison", not((1,(2,3)) == (4,(4,4))));

  //led2Toggle();
  
  emit (y, (x,x), 3.0);
}

main = s3;


