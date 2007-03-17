include "stdlib.ws";
include "date.ws";
include "plot.ws";


// Consists of Rev, Y, M, D, Lines
s0 = (dataFile("regiment_traffic.txt", "text", 10000, 0)
   :: Stream (Int * Int * Int * Int * Int));

lines = iterate( (_, _,_,_, ln) in s0 ) { emit ln };

filtered = stream_filter(fun(x) x<500, lines)

cum = iterate( ln in filtered ) {
  state { sum=0 }
  sum += ln;
  emit sum;
}

BASE <- livePlot(window(cum, 10))
