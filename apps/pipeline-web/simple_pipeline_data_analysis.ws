
data = (dataFile("data.txt", "text", 0) :: Signal (Int, Float));

wins = window(data, 10);
//wins = rewindow(wins, 10, 5)

avgs = iterate (w in wins) {
  sum = 0.0; 
  for i = w.start to w.end  {
    let (n,f) = w[[i]];
    sum := sum +. int_to_float(n);
  };  
  emit sum /. int_to_float(w.width);
};

BASE <- avgs;
