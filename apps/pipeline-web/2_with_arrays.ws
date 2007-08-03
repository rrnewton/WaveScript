// This version shows how to use an array to manually keep the state
// of the moving average.

data = (readFile("data.txt", "", timer(1000.0)) :: Stream (Int * Float));

avgs = iterate ((n,f) in data) {
  state {
    ind = 0;
    buffer = Array:make(10, 0);
    sum = 0.0;
  }
  buffer[ind] := n;
  ind := ind + 1;
  if ind == 10 then ind := 0;
  sum := 0.0; 
  for i = 0 to 9 {
    sum := sum +. intToFloat(buffer[i]);
  };  
  emit sum /. 10.0;
};

BASE <- avgs;
