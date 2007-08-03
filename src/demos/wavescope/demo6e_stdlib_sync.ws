
include "stdlib.ws";

//======================================================================
// QUERY:

// This should output every other 100 samples from both channels.

chans = (readFile("6sec_marmot_sample.raw", "mode: binary", timer(44000.0))
         :: Stream (Int16 * Int16 * Int16 * Int16));
ch1 = window(iterate((a,_,_,_) in chans){ emit int16ToFloat(a) }, 100);
ch2 = window(iterate((_,b,_,_) in chans){ emit int16ToFloat(b) }, 100);

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

BASE <- syncN(snoop("  CTRL", ctrl), [ch1, ch2]);

