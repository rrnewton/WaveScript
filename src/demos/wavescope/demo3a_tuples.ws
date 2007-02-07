

//include "stdlib.ws";

// Audio channel 1 with no overlap.
s1 = audioFile("./countup.raw", 4096, 0, 44000);

s2 = iterate( w in s1 ) {
  emit (w.width, w.start);
};

s3 = iterate((x,y) in s2) {



  //emit (y, x, 3.0);

  print(y); print(" "); print(x); print(" "++show(3.0)++"\n");   emit ();
}

BASE <- s3;


