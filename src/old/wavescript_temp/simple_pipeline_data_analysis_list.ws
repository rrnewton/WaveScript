

fun take(n,ls) 
  if n == 0 
  then ls
  else (ls.head : take(n-1, ls.tail))

fun sum(n,ls)
  if n == 0
  then 0
  else (ls.head + sum(n-1, ls.tail))

data = (dataFile("data.txt", "text", 0) :: Signal (Int, Float));

avgs = iterate ((n,f) in data) {
  state {
    buffer = [];
  }
  buffer := n : take(9,buffer);
  emit sum(10,buffer);
};

BASE <- avgs;
