
//========================================
// Main query:

include "ensbox_logger.ws";
include "stdlib.ws";
include "netsource.ws";

synced_ints = netsub_4sigseg("192.168.3.167", "detections"); 
synced = stream_map(fun (x) 
            map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);

include "marmot2.ws";

doas = oneSourceAMLTD(synced, micgeometry, 4096);

BASE <- (doas)
