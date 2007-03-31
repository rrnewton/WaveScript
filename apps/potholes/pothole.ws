
include "stdlib.ws";
include "filter.ws";
//include "matrix.ws";

//======================================================================



sm = stream_map;

// ============================================================
// Main query:

chans = (readFile("~/potholes/3-27/drive.txt", "")
          :: Stream (int16 * int16 * int16));

x = window(sm(fun((a,_,_)) int16ToFloat(a), chans), 512);
z = window(sm(fun((_,a,_)) int16ToFloat(a), chans), 512);
y = window(sm(fun((_,_,a)) int16ToFloat(a), chans), 512);

z2 = gnuplot_sigseg_stream(z);

z3 = fft_filter(z2,notch_filter(1025,10*4,20*4));

BASE <- unionList([gnuplot_sigseg_stream(rewindow(z3, 4096, 0)),gnuplot_sigseg_stream(rewindow(z, 4096, 0))]);



