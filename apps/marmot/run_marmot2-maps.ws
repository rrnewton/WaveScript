
//========================================
// Main query:

// How long does it take to get one tuple??
// ws: 2631 ms (1941 in ws.opt), 84 ms, 


include "sources_from_file.ws";
include "marmot_first_phase.ws";

synced_ints = detector((ch1i,ch2i,ch3i,ch4i));

/*
synced = stream_map(fun (ls) 
            map(fun (y) sigseg_map(int16ToFloat,y), ls),
	    synced_ints);
*/

include "marmot2-maps.ws";

// 'synced' is defined in marmot_first_phase.ws
//doas = FarFieldDOAb(synced, sensors);
/*
doas :: Stream (Array Float);
doas = iterate (m,stamp,tb) in oneSourceAMLTD(synced_ints, 4096)
{
  println("Got reading at: "++stamp);
  emit m
}
*/

include "gnuplot.ws";

fun QUITAFTERFIRST(s) {
  iterate x in s {
    state { first = true }
    if first == true then { emit x; first := false; }
    else wserror("Got first element: "++x);
  }
}

fun TIMEFIRST(strm) {
  iterate x in strm {
    state { strt = (clock() :: Double);
            first = true;  }
    if first then { println("TimeElapsed: "++ (clock() - strt)); first := false };
    emit x;
  }
}

//BASE <- Gnuplot:array_stream_autopolar("set title \"AML output\"\n", (doas))
BASE <- oneSourceAMLTD(synced_ints, 4096)

/*
  "set polar;\n"++
  "set title \"AML output\"\n"
  , smap(fun(a) Array:mapi(fun(i,mag) (i`i2f / 180.0 * const_PI, mag), a), 
         doas))
     */
//BASE <- gnuplot_array_stream(doas)
/* BASE <- (doas) */

