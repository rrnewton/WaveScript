include "stdlib.ws";
include "date.ws";
include "plot.ws";

smap = stream_map;
sfilter = stream_filter;

BASE <- {
  // Consists of Rev, Y/M/D, Lines 
  s0 as (r, y,m,d, l) = 
    (dataFile("regiment_traffic.txt", "text", 10000, 0) 
     :: Stream (Int * Int * Int * Int * Int));
  
  asdays = smap(fun(y,m,d, lns) (Date:toDays((y,m,d)), lns),  
		s0.(y,m,d, l));

  filtered = sfilter(fun((d,l)) l<500, asdays);

  // Integrate the stream:
  cum = iterate( (date,ln) in filtered ) {
    state { sum=0 }
    sum += ln;
    emit (date,sum);
  };
  
  // Return a plot:
  livePlot(window(cum, 10))
}
