
include "filter.ws";


s0 = (readFile("6sec_marmot_sample.raw", 
               "mode: binary  window: 4096  skipbytes: 6 ", 
               timer(10.0)) :: Stream (Sigseg Int16));

sfloats = iterate w in s0 {
  arr = Array:build(w.width, fun (i) int16ToFloat(w[[i]]));
  emit toSigseg(arr, w.start, nulltimebase)
}


/*
BASE <-
smap(toArray,
   fft_filter(
    fft_filter(sfloats, low_pass(1024,768)),
    high_pass(1024,256)))
*/

BASE <-
smap(toArray,
    fft_filter(sfloats, low_pass(1024,32)))
