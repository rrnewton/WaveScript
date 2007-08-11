
// Here we hackishly redefine wserror before loading stdlib.

include "ensbox_logger.ws";

origprint = print;
fun print(str) {
  log_file(1,str);
  log(1,str);
}

origerror = wserror;
fun wserror(str) { msg = "wserror: "++ str++"\n";
                   log_file(0, msg);
                   log(0, msg);
                   origerror(str); }

include "stdlib.ws";
include "vxpsource.ws";
include "netsource.ws";

inter = vxp_source_init();
ch1i = degap(vxp_source_stream(inter, 0),0`gint);
ch2i = degap(vxp_source_stream(inter, 1),0`gint);
ch3i = degap(vxp_source_stream(inter, 2),0`gint);
ch4i = degap(vxp_source_stream(inter, 3),0`gint);

