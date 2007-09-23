








// Audio channel 1 with no overlap.
s1 = (readFile("./countup.raw", "mode: binary  window: 40", timer(1000.0 / 40.0)) :: Stream (Sigseg Int16));

// Identity iterate.
s2 = iterate( w in s1 ) {
  //  state{ static counter = 0 }
  //  counter := static(statref(counter) + 1);
  state{ counter :: Int = 0 }
  counter += 1;
  emit counter;
};

BASE <- s2;
