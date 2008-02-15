








// Audio channel 1 with no overlap.
//s1 = (readFile("./countup.raw", "mode: binary  window: 40", timer(1000.0 / 40.0)) :: Stream (Sigseg Int16));
s1 = timer(10);

// Identity iterate.
s2 = iterate( w in s1 ) {
  //  state{ static counter = 0 }
  //  counter := static(statref(counter) + 1);
  //state{ counter :: Int = 0 }
  state{ counter = 99 } // [2007.10.02] It will default to int....
  counter += 1;
  emit counter;
};

main = s2;
