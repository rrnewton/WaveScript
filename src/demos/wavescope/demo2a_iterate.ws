













// Audio channel 1 with no overlap.
s1 = audioFile("./countup.raw", 4096, 0);

// Identity iterate.
s2 = iterate( w in s1 ) {
  emit w;
};


BASE <- s2;






