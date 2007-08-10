
include "stdlib.ws";
include "vxpsource.ws";

inter = vxp_source_init();
ch1 = deep_stream_map(int16ToFloat,
		      degap(vxp_source_stream(inter, 0),0`gint));
ch2 = deep_stream_map(int16ToFloat,
		      degap(vxp_source_stream(inter, 1),0`gint));
ch3 = deep_stream_map(int16ToFloat,
		      degap(vxp_source_stream(inter, 2),0`gint));
ch4 = deep_stream_map(int16ToFloat,
		      degap(vxp_source_stream(inter, 3),0`gint));

include "marmot_first_phase.ws";

BASE <- synced 
