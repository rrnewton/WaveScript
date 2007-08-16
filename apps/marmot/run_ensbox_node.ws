

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
  emit map(fun(ss) subseg(ss, fst`start, min(4096,fst`width)), ls);
}


// **********************  SERVER SIDE AML ************************ //
//==================================================================//

result_dets = smap(fun(_) (), netpub_sigseg4(chopped, "detections"));

// **********************  CLIENTSIDE AML ************************ //
//=================================================================//

include "marmot2.ws";
amls :: Stream IntAML;
amls = smap(normalized_aml_to_int16s,
       smap(normalize_aml,
            oneSourceAMLTD(phase1result, 4096)))

result_amls =  netpub_aml(amls, "amls");

//================================================================================//

BASE <- if AMLSERVERSIDE then result_dets else result_amls
