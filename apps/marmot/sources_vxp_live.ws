
include "stdlib.ws";
include "vxpsource.ws";
include "netsource.ws";

inter = vxp_source_init();
ch1i = degap(vxp_source_stream(inter, 0),0`gint);
ch2i = degap(vxp_source_stream(inter, 1),0`gint);
ch3i = degap(vxp_source_stream(inter, 2),0`gint);
ch4i = degap(vxp_source_stream(inter, 3),0`gint);

