

include "sources_vxp_live.ws";
include "marmot_first_phase.ws";

phase1result = detector((ch1i,ch2i,ch3i,ch4i));





// **********************  UNCOMMENT FOR SERVER SIDE AML ************************ //
//================================================================================//

BASE <- netpub_sigseg4(phase1result, "detections"); 




// **********************  UNCOMMENT FOR CLIENT SIDE AML ************************ //
//================================================================================//

//amls :: List (Stream AML);
//amls = map(fun (slsf) oneSourceAMLTD(slsf, 4096),phase1result)
//BASE <- netpub_sigseg4(amls, "amls"); 
