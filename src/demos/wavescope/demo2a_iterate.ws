











// Audio channel 1 with no overlap.
s1 = audioFile("./countup.raw", 40, 0, 44000);

// Identity iterate.
s2 = iterate( w in s1 ) {
  //print("TEST: "++ show(w) ++"\n");
  emit w;
};


BASE <- s2;






