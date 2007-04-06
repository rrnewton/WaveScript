











// Audio channel 1 with no overlap.
s1 = (readFile("./countup.raw", "mode: binary  window: 40") :: Stream (Sigseg Int));

// Identity iterate.
s2 = iterate w in s1 {
  //print("TEST: "++ show(toArray(w)) ++"\n");

  //gnuplot_array(toArray(w));
  
  emit w;
};


//BASE <- gnuplot_sigseg_stream(s2);
BASE <- s2;
