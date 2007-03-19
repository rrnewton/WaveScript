include "stdlib.ws";
include "date.ws";
include "plot.ws";


// Consists of Rev, Y, M, D, Lines
s0 = (dataFile("regiment_traffic.txt", "text", 10000, 0)
   :: Stream (Int * Int * Int * Int * Int));

lines = iterate( (_, _,_,_, ln) in s0 ) { emit ln };

BASE <- Plot:live1d(window(lines, 10))
