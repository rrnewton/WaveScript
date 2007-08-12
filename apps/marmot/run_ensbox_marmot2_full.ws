
//========================================
// Main query:

include "ensbox_logger.ws";
include "stdlib.ws";
include "netsource.ws";
include "ptolemy.ws";

host = "192.168.3.167";
ptolemyhost = "192.168.3.150";

synced_ints = netsub_4sigseg(host, "detections"); 

synced = stream_map(fun (x) 
            map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);

include "marmot2.ws";

doas = oneSourceAMLTD(synced, sensors, 4096);

snoop2 = iterate l in synced_ints {
  state {
    handle = ptolemy_open(ptolemyhost, 4005)
  }

  array_to_ptolemy(handle, host++"-1", toArray(l`List:ref(0)));
  array_to_ptolemy(handle, host++"-2", toArray(l`List:ref(1)));
  array_to_ptolemy(handle, host++"-3", toArray(l`List:ref(2)));
  array_to_ptolemy(handle, host++"-4", toArray(l`List:ref(3)));
};

fun nullify(x) iterate y in x {}

BASE <- merge(snoop2,nullify(doas));

//BASE <- (doas)
