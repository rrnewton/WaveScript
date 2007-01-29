



data = (dataFile("data.txt", "text", 0) :: Signal (Int, Float));

avgs = iterate ((n,f) in data) {
  state {
    ind = 0;
    buffer = makeArray(10, 0);
    sum = 0.0;
  }
  buffer[ind] := n;
  ind := ind + 1;
  if ind == 10 then ind := 0;
  sum := 0.0; 
  for i = 0 to 9 {
    sum := sum +. int_to_float(buffer[i]);
  };  
  emit sum /. 10.0;
};

BASE <- avgs;
