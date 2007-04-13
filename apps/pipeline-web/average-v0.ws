include "stdlib.ws";

// 0 says not to replay the data-file after it's finished:
// 1000 says to set sample rate at 1Khz -- not important for this app:
data = (dataFile("data.txt", "text", 1000, 0) :: Stream (Float));
wins = prim_window(data, 100);


// BASE <- snoop("hello" , wins);


avgs = iterate (w in wins) {
  // This is a purely functional way to iterate over the array:
  sum = Array:fold(fun (acc, n) acc + n,
		   0.0,
		   w.toArray);
  emit sum / intToFloat(w.width);
};

BASE <- avgs;
