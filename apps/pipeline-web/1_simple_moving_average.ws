

// 0 says not to replay the data-file after it's finished:
// 1000 says to set sample rate at 1Khz -- not important for this app:
data = (dataFile("data.txt", "text", 1000, 0) :: Stream (Int * Float));

wins = prim_window(data, 10);

avgs = iterate (w in wins) {
  sum = 0.0; 
  for i = 0 to w.width-1  {
    let (n,f) = w[[i]];
    sum := sum +. intToFloat(n);
  };
  emit sum /. intToFloat(w.width);
};

BASE <- avgs;
