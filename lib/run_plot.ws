
include "stdlib.ws";
include "plot.ws";

data = (readFile ("plot.ws", "mode: binary  window: 10") :: Stream (Sigseg Int16))

widths = iterate w in data {
  emit Array:make(1, w`width)
};

widths2 = iterate w in data {
  emit w`width
};


//BASE <- Plot:Sigseg:cumulative1d(s1)
//BASE <- Plot:Array:cumulative1d(widths)
//BASE <- Plot:cumulative1d(widths2)
//BASE <- Plot:seggaps(data)
//BASE <- Plot:seggaps_sliding(data)
//BASE <- Plot:sqrwave_cumulative(data)
BASE <- Plot:sqrwave(data,10)

