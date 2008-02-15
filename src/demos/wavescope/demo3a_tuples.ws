

//include "stdlib.ws";

// Audio channel 1 with no overlap.
//s1 = (readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) :: Stream (Sigseg Int16));
s1 = timer(10.0);

s2 = iterate( w in s1 ) {
  //emit (w.width, w.start, w[[0]]);
  emit (10, 20, 30);
};

s3 = iterate((x,y,z) in s2) {

  //tup = ([1], 2, Array:make(5,4));
  //println("Tuple of pointer type: "++tup);

  print(y); print(" "); print(x); print(" "++show(z)++"\n");  
  emit ();
}

main = s3;


