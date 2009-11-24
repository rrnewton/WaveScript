
include "stdlib.ws";
include "date.ws";
include "plot.ws";

// Consists of Rev, Y, M, D, Lines
s0 = (dataFile("regiment_traffic.txt", "text", 10000, 0)
   :: Stream (Int * Int * Int * Int * Int));

data = iterate( (r, y,m,d, l) in s0 ) { emit ((y,m,d), l) }

lines = iterate( (d,ln) in data ) { emit ln };

cum = iterate( ln in lines ) {
  state { sum=0 }
  sum += ln;
  emit sum;
}

