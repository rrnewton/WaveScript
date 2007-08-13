

include "sources_vxp_live.ws";
include "marmot_first_phase.ws";

phase1result = detector((ch1i,ch2i,ch3i,ch4i));

BASE <- netpub_sigseg4(phase1result, "detections"); 
