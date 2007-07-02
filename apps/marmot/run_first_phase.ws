
// [2007.06.26] This is taking 1.5 seconds on fort, 2.6 on partridge.
// For some reason it takes FIFTY seconds on the ensbox right now.

// Reading one channel takes 2.5 seconds.
// But reading all four and doing a union takes 17-20 seconds!

/* [2007.07.01] Timing different implementations:

 Timing against "3min_marmot_sample.raw" (31.4mb) on faith.
 With blocked sigseg reading.
  ws.opt     : 115 seconds (???)
  wsc -O3    : broken Sync
  wsmlton    : 19.6 sec (copyalways)
  Marmot.cpp : 1.6 sec
 
 Timing against "6sec_marmot_sample.raw" X4 (4.6mb) on faith.
 With blocked sigseg reading.
  ws.opt     : 17.6 seconds (???)
  wsc -O3    : (broken sync) 10.6 s (9.1 real) 6.2 with "-j 1 --at_once" 5.8 with one processor 
  wsmlton    : 2.6 s (copyalways)
  Marmot.cpp : 0.23 s (0.44 real)

NOTE: The WSC version must have bad syncing dynamics.  It spits out
all four detection messages (for 6sec X4), then there is a noticable
lag before it spits out the synced sigsegs.

I put in a warning message when syncN's accumulators get too big.  I
was having the same problem with the MLton version (exploding
accumulators).  But that was because I forgot to recompile the
compiler on faith ;).  There must be a bug in the sigseg or list
primitives in the c++ versin.

*/


include "marmot_first_phase.ws";

//BASE <- ch1
//BASE <- unionList([ch1,ch2,ch3,ch4])
//BASE <- rw1
//BASE <- hn
//BASE <- d2
BASE <- synced
