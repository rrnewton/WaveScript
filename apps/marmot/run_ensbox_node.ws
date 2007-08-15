

include "sources_vxp_live.ws";
include "marmot_first_phase.ws";

phase1result = detector((ch1i,ch2i,ch3i,ch4i));

//========================================
// PHASE 1 ONLY:
BASE <- netpub_sigseg4(phase1result, "detections"); 

/*

//========================================
// TRYING AML ON NODE

/*
fun LAMESPLITTER((arr,strt)) {
  chunksize = seg`width / 4;
  using Array;
  arr1 = build(chunksize, fun(i) arr[i + 0 * chunksize]);
  arr2 = build(chunksize, fun(i) arr[i + 1 * chunksize]);
  arr3 = build(chunksize, fun(i) arr[i + 2 * chunksize]);
  arr4 = build(chunksize, fun(i) arr[i + 3 * chunksize]);
  ()
}
*/

// This hackishly reuses the C function used to send detections
fun netpub_aml(amlS, name) {
  iterate (vec,stamp) in s {
    state { ns = c_wsnet_register(name); }
    
    c_wsnet_enqueue_sigseg4(ns, stamp, arr`Array:length, 0, arr);
    emit(l);
  }
}


BASE <- netpub_sigseg4(phase1result, "detections"); 
*/
