

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
amls = smap(aml_to_int16s,
            oneSourceAMLTD(phase1result, 4096))

result_amls =  netpub_aml(amls, "amls");

// **********************  ADAPTIVE AML *************************** //
//==================================================================//

decided = iterate d in chopped {
  estimated_time = 3.0;  //teleport_in("aml_timing");  -- would measure later
  if (vxp_buffer_time_remaining() < (estimated_time * 1.25)) then {
    emit(true,d);
  }
  else {
    emit(false,d);
  }
}

do_local = iterate (local,d) in decided { 
  if (local) then emit(d); 
}

send_back = iterate (local,d) in decided { 
  if (!local) then emit(d); 
}


include "marmot2.ws";
amls :: Stream IntAML;
amls = smap(normalized_aml_to_int16s,
       smap(normalize_aml,
            oneSourceAMLTD(do_local, 4096)));

result_adapt = merge(smap(fun(_) (), netpub_aml(amls, "amls")),
		     smap(fun(_) (), netpub_sigseg4(send_back, "detections")));

//================================================================================//

BASE <- 
if AMLSERVERSIDE 
then result_dets 
else if AMLADAPTIVE 
then result_adapt
else result_amls
