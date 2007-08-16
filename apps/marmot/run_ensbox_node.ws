

include "sources_vxp_live.ws";

//include "sources_from_file.ws";  include "netsource.ws";

include "marmot_first_phase.ws";

phase1result = detector((ch1i,ch2i,ch3i,ch4i));

/*
phase1result = iterate x in _phase1result {
  emit x; emit x; emit x; emit x; emit x; emit x; emit x; emit x; emit x; emit x;
}
*/

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
/* amls :: Stream IntAML; */
/* amls = smap(normalized_aml_to_int16s, */
/*        smap(normalize_aml, */
/*             oneSourceAMLTD(phase1result, 4096) */
/* 	    //smap(fun(dets)    ) */
/* 	    )) */

/* amls2 = iterate x in amls{ */
/*   log(1, "WHATS UP HERE?"); */
/*   emit x; */
/* } */

/* //BASE <- netpub_aml(snoop("SNOOPIN", amls2), "amls"); */
/* //BASE <- netpub_aml(snoop("SNOOPIN", amls2), "amls"); */
/* BASE <- snoop("SNOOPIN", amls2) */

