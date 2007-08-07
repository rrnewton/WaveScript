
include "vxpsource.ws";

chans = vxp_source();

BASE <-
unionList([snoop("ch0",listRef(chans,0)),
	   snoop("ch1",listRef(chans,1)),	   
	   snoop("ch2",listRef(chans,2)),	   
	   snoop("ch3",listRef(chans,3))]);
