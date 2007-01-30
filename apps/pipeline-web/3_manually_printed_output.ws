// This shows how to manually print the output to get it in the desired format.
// It also demonstrates how to include a library file (found in $REGIMENTD/lib/).

include "stdlib.ws";

data = (dataFile("data.txt", "text", 0) :: Stream (Int * Float));

wins = window(data, 10);

// Use rewindow (from stdlib) to get overlapping windows:
wins2 = rewindow(wins, 100, -50);

avgs = iterate (w in wins2) {
  sum = 0.0; 
  for i = 0 to w.width-1  {
    let (n,f) = w[[i]];
    sum := sum +. intToFloat(n);
  };  

  println("AVG OF RANGE ["++ show(w.start) ++":"++ show(w.end) ++"] IS "++ show(sum));
  emit ();
};

BASE <- avgs;
