










// Audio channel 1 with no overlap.
s1 = (readFile("./countup.raw", "mode: binary  window: 40") :: Stream (Sigseg Int16));

// Identity iterate.
s2 = iterate w in s1 {
  //print("TEST: "++ show(toArray(w)) ++"\n");

  //  x = 1 +. 2.0;

  //if 1 then 3.0 else 4.0;
  //if true then "foo" else 4.0;
  
  //gnuplot_array(toArray(w));
  
  //  wserror("testing...");  

  emit w;
};


//BASE <- gnuplot_sigseg_stream(s2);
BASE <- s2;
