

//include "stdlib.ws";

// Audio channel 1 with no overlap.
s1 = (readFile("./countup.raw", "mode: binary  window: 4096") :: Stream (Sigseg Int16));

s2 = iterate( w in s1 ) {
  emit (w.width, w.start, w[[0]]);
};

s3 = iterate((x,y,z) in s2) {

  print(y); print(" "); print(x); print(" "++show(z)++"\n");  
  emit ();
}

BASE <- s3;


