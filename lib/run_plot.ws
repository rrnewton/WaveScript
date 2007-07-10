
include "plot.ws";

s1 = (readFile ("plot.ws", "mode: binary  window: 100") :: Stream (Sigseg Int16))

s2 = iterate w in s1 {
  emit Array:make(1, w`width)
}

BASE <- Plot:Array:cumulative1d(s2)

//BASE <- Plot:Sigseg:cumulative1d(s1)





