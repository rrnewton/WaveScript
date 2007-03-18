

// 0 says not to replay the data-file after it's finished:
// 1000 says to set sample rate at 1Khz -- not important for this app:
data = (dataFile("data.txt", "text", 1000, 0) :: Stream (Int * Float));

wins = prim_window(data, 10);

avgs = iterate (w in wins) {
  // This is a purely functional way to iterate over the array:
  sum = Array:fold(fun (acc, (n,_)) acc + n.intToFloat,
		   0.0,
		   w.toArray);
  emit sum /. intToFloat(w.width);
};

BASE <- avgs;
