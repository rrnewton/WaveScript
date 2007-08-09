
include "stdlib.ws";
include "vxpsource.ws";

chans = vxp_source();

BASE <-
//unionList(List:map2(fun (a,b) snoop(a,b), ["ch0","ch1","ch2","ch3"], chans));

//unionList([ snoop("ch0",List:ref(chans,0)), snoop("ch1",List:ref(chans,1)),
//snoop("ch2",List:ref(chans,2)), snoop("ch3",List:ref(chans,3)) ]);

//unionList(chans);
snoop("blah",chans)

