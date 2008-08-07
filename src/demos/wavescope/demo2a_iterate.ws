







//foo :: a -> a;
foo :: Int -> Int;
fun foo(x) x+(1::Int)


// Audio channel 1 with no overlap.
//s1 = (readFile("./countup.raw", "mode: binary  window: 40", timer(1000.0 / 40.0)) :: Stream (Sigseg Int16));
s1 = timer(1.0);

// Identity iterate.
s2 = iterate w in s1 {
  //print("TEST: "++ show(toArray(w)) ++"\n");

  //  x = 1 +. 2.0;

  //if 1 then 3.0 else 4.0;
  //if true then "foo" else 4.0;
  
  //gnuplot_array(toArray(w));
  
  //  wserror("testing...");  
  print(" ");
  emit foo(39);
};


//main = gnuplot_sigseg_stream(s2);
main = s2;
