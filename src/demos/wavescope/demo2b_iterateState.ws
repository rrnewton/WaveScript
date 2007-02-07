








// Audio channel 1 with no overlap.
s1 = audioFile("./countup.raw", 4096, 0, 44000);

// Identity iterate.
s2 = iterate( w in s1 ) {
  state{ counter = 0 }
  counter := counter + 1;
  emit counter;
};

BASE <- s2;
