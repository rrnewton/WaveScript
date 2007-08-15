
//========================================
// Main query:

include "sources_from_file.ws";
include "marmot_first_phase.ws";

synced_ints = detector((ch1i,ch2i,ch3i,ch4i));

/*
synced = stream_map(fun (ls) 
            map(fun (y) sigseg_map(int16ToFloat,y), ls),
	    synced_ints);
*/

include "marmot2.ws";

// 'synced' is defined in marmot_first_phase.ws
//doas = FarFieldDOAb(synced, sensors);
doas = iterate (m,stamp) in oneSourceAMLTD(synced_ints, 4096)
{
  println("Got reading at: "++stamp);
  emit m
}

BASE <- gnuplot_array_stream(doas)
/* BASE <- (doas) */

