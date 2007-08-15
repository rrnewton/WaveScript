

include "sources_vxp_live.ws";
include "marmot_first_phase.ws";

phase1result = detector((ch1i,ch2i,ch3i,ch4i));


// Assumes they're the same size of course:
chopped = iterate ls in phase1result {
  fst = List:ref(ls,0);
  emit map(fun(ss) subseg(ss, fst`start, fst`width), ls);
}


// **********************  UNCOMMENT FOR SERVER SIDE AML ************************ //
//================================================================================//

BASE <- netpub_sigseg4(chopped, "detections");


// **********************  UNCOMMENT FOR CLIENT SIDE AML ************************ //
//================================================================================//

/* include "marmot2.ws"; */
/* amls :: Stream AML; */
/* amls = oneSourceAMLTD(phase1result, 4096) */
/* BASE <- netpub_aml(amls, "amls");  */
