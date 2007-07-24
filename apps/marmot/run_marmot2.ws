
//========================================
// Main query:

include "marmot_first_phase.ws";
include "marmot2.ws";

// 'synced' is defined in marmot_first_phase.ws
//doas = FarFieldDOAb(synced, sensors);
doas = oneSourceAMLTD(synced, sensors,4096);

BASE <- (doas)
