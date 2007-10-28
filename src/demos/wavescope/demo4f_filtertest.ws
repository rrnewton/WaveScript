
include "filter.ws";


s0 = (readFile("6sec_marmot_sample.raw", 
               "mode: binary  window: 4096  skipbytes: 6 ", 
               timer(10.0)) :: Stream (Sigseg Int16));

sfloats = iterate w in s0 {
  arr = Array:build(w.width, fun (i) int16ToFloat(w[[i]]));
  emit toSigseg(arr, w.start, nulltimebase)
}

fun gnuplot_snoop(s) {
  iterate a in s {
    gnuplot_array(a);
    emit(a)
  }
}

/*
BASE <-
 merge(
  gnuplot_snoop(psd(sfloats, 64)),
  gnuplot_snoop(psd(fft_filter(sfloats, low_pass(1024,32)), 32)))
*/

/*
BASE <-
  fft_filter(sfloats, low_pass(1024,32))
*/

BASE <-
  gnuplot_snoop(psd(
   fft_filter(
    fft_filter(sfloats, low_pass(1024,768)),
    high_pass(1024,256)), 1024))
