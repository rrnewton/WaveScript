
include "stdlib.ws";
include "filter.ws";
//include "matrix.ws";

//======================================================================

sm = stream_map;

// ============================================================
// Main query:

chans = (readFile("/tmp/crap", "")
          :: Stream (Float * Float * Float * int16 * int16 * int16));

x = window(sm(fun((_,_,_,a,_,_)) int16ToFloat(a), chans), 512);
y = window(sm(fun((_,_,_,_,a,_)) int16ToFloat(a), chans), 512);
z = window(sm(fun((_,_,_,_,_,a)) int16ToFloat(a), chans), 512);


// assuming sample rate is 380 hz
z3 = fft_filter(z,notch_filter(1025,150*2,260*2));

BASE <- z3;

