




include "../sources_from_file.ws";
include "marmot1-slow.ws";

chans = detector((ch1i,ch2i,ch3i,ch4i));

ch1 = stream_map(fun(x) List:ref(x, 0), chans)
ch2 = stream_map(fun(x) List:ref(x, 1), chans)
ch3 = stream_map(fun(x) List:ref(x, 2), chans)
ch4 = stream_map(fun(x) List:ref(x, 3), chans)

BASE <- zipN_sametype(10,
 [gnuplot_sigseg_stream(ch1),
  gnuplot_sigseg_stream(ch2),
  gnuplot_sigseg_stream(ch3),
  gnuplot_sigseg_stream(ch4)])

