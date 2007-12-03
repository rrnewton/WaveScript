
include "stdlib.ws";
include "vxpsource.ws";

inter = vxp_source_init();
ch0 = vxp_source_stream(inter, 0);

main = snoop("ch0",ch0)

