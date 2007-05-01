
// [2007.05.01] rrn: updating to use the newer, simpler "readFile" interface.

// 0 says not to replay the data-file after it's finished:
// 1000 says to set sample rate at 1Khz -- not important for this app:
data = (readFile("data.txt", "") :: Stream (Int * Float));

// prim_window will be obsoleted at some point.
wins = prim_window(data, 10);

avgs = iterate (w in wins) {
  // This is a purely functional way to iterate over the array:
  sum = Array:fold(fun (acc, (n,_)) acc + n.intToFloat,
		   0.0,
		   w.toArray);
  emit sum /. intToFloat(w.width);
};

BASE <- avgs;
