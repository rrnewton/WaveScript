

data = (dataFile("data.txt", "text", 0) :: Stream (Int * Float));

wins = window(data, 10);

avgs = iterate (w in wins) {
  sum = 0.0; 
  for i = 0 to w.width-1  {
    let (n,f) = w[[i]];
    sum := sum +. intToFloat(n);
  };
  emit sum /. intToFloat(w.width);
};

BASE <- avgs;
