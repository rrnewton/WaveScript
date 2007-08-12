
//========================================
// Main query:

include "sources_from_file.ws";
include "marmot_first_phase.ws";

synced = stream_map(fun (x) 
            map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);

include "marmot2.ws";

// 'synced' is defined in marmot_first_phase.ws
//doas = FarFieldDOAb(synced, sensors);
doas = oneSourceAMLTD(synced, sensors,4096);

BASE <- (doas)
