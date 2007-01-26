
data = (dataFile("data.txt", "text", 0) :: Signal (Int, Float));

wins = window(data, 10);
//wins = rewindow(wins, 10, 5)

avgs = iterate (w in wins) {
  //print("...ITERATING: "++ show(w.start) ++"\n");
/*   print("...ITERATING: ");  */
/*   print(w.start);  */
/*   print(" "); */
/*   print(w.end);  */
/*   print(" "); */
/*   print(w[[0]]);  */
/*   print("\n"); */
  sum = 0.0; 
  for i = 0 to w.width-1  {
  //for i = w.start to w.end  {
    let (n,f) = w[[i]];
    sum := sum +. int_to_float(n);
  };  
  //print("SUM: "); print(sum); print("\n");
  emit sum /. int_to_float(w.width);
};

BASE <- avgs;
