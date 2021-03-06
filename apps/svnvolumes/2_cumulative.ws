include "stdlib.ws";
include "date.ws";
include "plot.ws";


// Consists of Rev, Y, M, D, Lines
s0 = (dataFile("regiment_traffic.txt", "text", 10000, 0)
   :: Stream (Int * Int * Int * Int * Int));

lines = iterate( (_, _,_,_, ln) in s0 ) { emit ln };

cum = iterate( ln in lines ) {
  state { sum=0 }
  sum += ln;
  emit sum;
}

BASE <- Plot:live1d(window(cum, 10))
