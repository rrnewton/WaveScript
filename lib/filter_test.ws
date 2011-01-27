
include "stdlib.ws";
include "filter.ws";

sm = stream_map;

// ============================================================
// Main query:

chans = (readFile("filter_dat.txt", "")
          :: Stream (Float * Float));

z = window(sm(fun((a,_)) a, chans), 512);

z2 = gnuplot_sigseg_stream(z);

z3 = fft_filter(z2,notch_filter(1025,10*4,20*4));

BASE <- unionList([gnuplot_sigseg_stream(rewindow(z3, 4096, 0)),gnuplot_sigseg_stream(rewindow(z, 4096, 0))]);



