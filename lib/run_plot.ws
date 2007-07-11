
include "plot.ws";

data = (readFile ("plot.ws", "mode: binary  window: 100") :: Stream (Sigseg Int16))

widths = iterate w in data {
  emit Array:make(1, w`width)
};

widths2 = iterate w in data {
  emit w`width
};


//BASE <- Plot:Sigseg:cumulative1d(s1)
//BASE <- Plot:Array:cumulative1d(widths)
BASE <- Plot:cumulative1d(widths2)







