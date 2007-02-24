
include "stdlib.ws";

//======================================================================
// QUERY:

// This should output every other 100 samples from both channels.

chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) :: Stream (Int * Int * Int * Int));
ch1 = window(iterate((a,_,_,_) in chans){ emit intToFloat(a) }, 100);
ch2 = window(iterate((_,b,_,_) in chans){ emit intToFloat(b) }, 100);

outwidth = 100;

ctrl = iterate(w in ch1) {
  state {
    pos = 0;
    flag = false;
  }
  emit(flag, pos, pos + outwidth - 1);
  pos := pos + outwidth;
  flag := if flag then false else true;
};

BASE <- syncN(ctrl, [ch1, ch2]);

