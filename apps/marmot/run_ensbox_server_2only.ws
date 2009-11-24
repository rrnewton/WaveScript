
//========================================
// Main query:

include "ensbox_logger.ws";
include "stdlib.ws";
include "netsource.ws";

samp_rate = 48000.0; // HACK - we should get this from the stream/timebase/sigseg

//synced_ints = netsub_4sigseg("192.168.3.167", "detections"); 

fun toFL(synced_ints)
  stream_map(fun (x) 
          map(fun (y) sigseg_map(int16ToFloat,y), x), 
	  synced_ints);

synced1 = toFL$ netsub_4sigseg("192.168.11.100", "detections"); 
synced2 = toFL$ netsub_4sigseg("192.168.11.103", "detections"); 
synced3 = toFL$ netsub_4sigseg("192.168.11.104", "detections"); 
synced4 = toFL$ netsub_4sigseg("192.168.11.108", "detections"); 
synced5 = toFL$ netsub_4sigseg("192.168.11.109", "detections"); 
synced6 = toFL$ netsub_4sigseg("192.168.11.112", "detections"); 
synced7 = toFL$ netsub_4sigseg("192.168.11.113", "detections"); 
synced8 = toFL$ netsub_4sigseg("192.168.11.115", "detections"); 

include "marmot2.ws";

doas1 = oneSourceAMLTD(synced1, micgeometry, 4096);
doas2 = oneSourceAMLTD(synced2, micgeometry, 4096);
doas3 = oneSourceAMLTD(synced3, micgeometry, 4096);
doas4 = oneSourceAMLTD(synced4, micgeometry, 4096);
doas5 = oneSourceAMLTD(synced5, micgeometry, 4096);
doas6 = oneSourceAMLTD(synced6, micgeometry, 4096);
doas7 = oneSourceAMLTD(synced7, micgeometry, 4096);
doas8 = oneSourceAMLTD(synced8, micgeometry, 4096);

merged :: Stream (Array Float);
merged = 
  merge(doas1, 
  merge(doas2, 
  merge(doas3, 
  merge(doas4, 
  merge(doas5, 
  merge(doas6, 
  merge(doas7, 
        doas8)))))))

//synced5 = toFL$ netsub_4sigseg("192.168.11.109", "detections"); 
//doas5 = oneSourceAMLTD(synced5, micgeometry, 4096);

//BASE <- (doas5)
BASE <- (merged)

