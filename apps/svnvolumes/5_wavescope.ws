include "stdlib.ws";
include "date.ws";
include "plot.ws";

smap = stream_map;
sfilter = stream_filter;

BASE <- {
  // Consists of Rev, Y/M/D, Lines 
  s0 as (r, y,m,d, l) = 
    (dataFile("wavescope_traffic.txt", "text", 10000, 0) 
     :: Stream (Int * Int * Int * Int * Int));

  asdays = smap(fun((y,m,d, lns)) (Date:toDays((y,m,d)), lns),  
		s0.(y,m,d, l));

  // Shift so first day in stream is ZERO:
  shifted = iterate((d,l) in asdays) {
    state{ first = -1 } // Need option types!!
    if first < 0 then first := d;
    emit (d - first, l)
  };

  filtered = sfilter(fun((d,l)) l<500, asdays);

  // Integrate the stream:
  cum = iterate( (date,ln) in filtered ) {
    state { sum=0 }
    sum += ln;
    emit (date,sum);
  };
  
  interpolated = iterate( (d,tot) in cum) {
    emit ();
  };

  asyears = smap(fun((d,l)) (d`intToFloat / 365.0, l), cum);
  
  // Return a plot:
  Plot:live2d( window(asyears, 10));  

}
