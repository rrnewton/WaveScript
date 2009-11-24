
// rrn: This doesn't do anything different yet.
// But the intention is to inspect some intermediate stream values.

//========================================
// Main query:

include "marmot_first_phase.ws";
include "marmot2.ws";

// 'synced' is defined in marmot_first_phase.ws
//doas = FarFieldDOAb(synced, sensors);
doas = oneSourceAMLTD(synced, sensors,4096);

BASE <- gnuplot_array_stream(doas)
