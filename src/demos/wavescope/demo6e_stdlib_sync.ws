
include "stdlib.ws";

//======================================================================
// QUERY:

// This should output every other 100 samples from both channels.

ticks = timer(100.0)
//ticks = iterate _ in timer(100.0) { print("."); emit () }
//ticks = timer(44000.0)
//ticks = timer(10.0)

chans = (readFile("6sec_marmot_sample.raw", "mode: binary", ticks)
         :: Stream (Int16 * Int16 * Int16 * Int16));
ch1 = window(iterate((a,_,_,_) in chans){ emit int16ToFloat(a) }, 100);
ch2 = window(iterate((_,b,_,_) in chans){ emit int16ToFloat(b) }, 100);

outwidth = 100`gint;

ctrl :: CtrlStrm;
ctrl = iterate(w in ch1) {
  state {
    pos = 0`gint;
    flag = false;
  }

  emit(flag, pos, pos + outwidth - 1`gint);
  pos := pos + outwidth;
  flag := if flag then false else true;
};

main = syncN(snoop("  CTRL", ctrl), [ch1, ch2]);

