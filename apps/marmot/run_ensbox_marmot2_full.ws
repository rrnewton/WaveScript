
//========================================
// Main query:

//include "ensbox_logger.ws";
include "stdlib.ws";
//include "netsource.ws";
include "ptolemy.ws";

samp_rate = 48000.0; // HACK - we should get this from the stream/timebase/sigseg

//host = "192.168.3.167";
//ptolemyhost = "192.168.3.150";
host = "10.211.55.3";
ptolemyhost = "10.211.55.5";

//synced_ints = netsub_4sigseg(host, "detections"); 
include "sources_from_file.ws";
include "marmot_first_phase.ws";
synced_ints = detector((ch1i,ch2i,ch3i,ch4i));

synced = stream_map(fun (x) 
            map(fun (y) sigseg_map(int16ToFloat,y), x), 
	    synced_ints);

include "marmot2.ws";

doas = oneSourceAMLTD(synced, micgeometry, 4096);

snoop2 = iterate l in synced_ints {
  state {
    fst = true;
    handle = -666;
  }
  if fst then {
    handle := ptolemy_open(ptolemyhost, 4005);
    fst := false;
  };

  array_to_ptolemy(handle, "{tag=\""++host++"\",chan=1,dat=", toArray(l`List:ref(0)));
  array_to_ptolemy(handle, "{tag=\""++host++"\",chan=2,dat=", toArray(l`List:ref(1)));
  array_to_ptolemy(handle, "{tag=\""++host++"\",chan=3,dat=", toArray(l`List:ref(2)));
  array_to_ptolemy(handle, "{tag=\""++host++"\",chan=4,dat=", toArray(l`List:ref(3)));
};

fun nullify(x) iterate y in x {}

BASE <- merge(snoop2,nullify(doas));

//BASE <- (doas)
