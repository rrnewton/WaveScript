
// Here we hackishly redefine wserror before loading stdlib.

include "ensbox_logger.ws";

origprint = print;
origerror = wserror;

fun print(str) {
  //log_file(1,str);
  log(1,str);
}
fun wserror(str) { 
  msg = "wserror: "++ str++"\n";
  log_file(0, msg);
  log(0, msg);
  origerror(str); 
}

include "stdlib.ws";
include "gnuplot.ws";
include "vxpsource.ws";
include "netsource.ws";
include "types.ws";

samp_rate = 48000.0; // HACK - we should get this from the stream/timebase/sigseg

max_gap = 128000;

inter = vxp_source_init(1);
ch1i = degap(vxp_source_stream(inter, 0),0`gint,max_gap);
ch2i = degap(vxp_source_stream(inter, 1),0`gint,max_gap);
ch3i = degap(vxp_source_stream(inter, 2),0`gint,max_gap);
ch4i = degap(vxp_source_stream(inter, 3),0`gint,max_gap);

