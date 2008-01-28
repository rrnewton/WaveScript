

//==============================================================================
// [2007.06.26] This is taking 1.5 seconds on fort, 2.6 on partridge.
// For some reason it takes FIFTY seconds on the ensbox right now.

// Reading one channel takes 2.5 seconds.
// But reading all four and doing a union takes 17-20 seconds!

/* [2007.07.01] Timing different implementations {faith}

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

/*
================================================================================
[2007.07.02] Trying various things to improve (mlton) performance.

Switched to a memoized version of fftR2C that caches the fftw plans.
Unfortunately it does one more copy than I'd like because of problems 
I had allocating the buffers ML side. 

Testing on my 2.33 ghz Core2 Duo laptop.  Using copy-always sigsegs.
I'm using a measurement system wherein we look at *marginal increase*.
I want to begin considering high-startup cost strategies such as
building an expensive fftw (measurement) plan at startup.

Oh, I just realized that fftw internally caches the last plan.  But
it's still cleaner to have a separate cache for each instance of an
fft stream kernel.

                       (4.6mb)   (9.2mb)   diff
With orig fftw (real):  1.78       3.55    1.77
With memoized  (real):  1.6        3.17    1.57
With MEASURE   (real):  1.66       3.2     1.54
With PATIENT   (real):  1.6        3.14    1.54

               (cpu) :  1.47       2.95    1.48
               (cpu) :  1.62       3.24    1.62
               (cpu) :  1.51       2.96    1.45
               (cpu) :  1.47       2.92    1.45

NOTE: The above all accidentally had Exn.keepHistory turned on!

Now, leaving it with "patient" fftw setting, trying other sigseg
implementations.  Those made a huge difference.

seglist w/dbg  (real):  1.0        2.0     1.0
seglist no/dbg (real):  .98        1.95     .97
wsharing       (real):  .28         .55     .27

               (cpu) :  .94        1.87     .93
               (cpu) :  .90        1.8      .9
               (cpu) :  .24         .47     .23

Wow, also tweaked it to read the file in one stream rather than 4.
This is getting too small to measure on theprevious data sizes...
(This doesn't apply to the version that reads from hardware.)

one file stream (real): .1          .2      .1
                (cpu) : .064        .12     .06


On 18mb of data: 1.5/1.4     vs.  2.1/1.2 for handwritten
On 36mb of data: 3.0/2.9s    vs.  4.0/2.3 for handwritten
It uses about half he memory of the C++ version too.

[2007.07.03] Just ran on the first phase marmot query on the ARM.
For a 1.2 mb file, the handwritten version clocked in at 6.9(6.2 cpu) 
seconds, and the MLton version clocked in at 3.1(2.9) seconds.
(Then, reading as one file stream it takes 0.94(.77).)

I switched the sigseg implementation to use arrays rather than
vectors, that sped things up even further.  (.08/.05 and .16/.097) 
In this way, it can process 73.6 mb in a second.

*/

//==============================================================================
// [2007.09.19] {failure to monomorphize}
//
// interpret-meta is not succesfully making the program monomorphic.
// First, I'm curious if interpret-meta's substantially more bloated
// output produces worse runtime performance, or if mlton cleans up
// after it effectively.  On 15min of data (168mb), on justice:
//   static-elab:    17.1 / 16.3 real/cpu (with ~1s variance)
//   interpret-meta: 16.5 / 15.6, 16.8 / 15.8 ...
// Ok, so no significant difference.
//
// 





include "sources_from_file.ws";
include "marmot_first_phase.ws";

//BASE <- 3 
//BASE <- chans
//BASE <- ch1
//BASE <- unionList([ch1,ch2,ch3,ch4])
//BASE <- rw1
//BASE <- hn
//BASE <- wscores
//BASE <- detections
//BASE <- d2
//BASE <- synced_ints

chans = detector((ch1i,ch2i,ch3i,ch4i));

ch1 = stream_map(fun(x) List:ref(x, 0), chans)
ch2 = stream_map(fun(x) List:ref(x, 1), chans)
ch3 = stream_map(fun(x) List:ref(x, 2), chans)
ch4 = stream_map(fun(x) List:ref(x, 3), chans)

fun projectabit(ls)
 ("GotDetectionFromTo", ls.head.start, ls.head.end)

  /*
withplotting =
 zipN_sametype(10,
  [gnuplot_sigseg_stream(ch1),
   gnuplot_sigseg_stream(ch2),
   gnuplot_sigseg_stream(ch3),
   gnuplot_sigseg_stream(ch4)])
  */

// Turning plotting off in the checked in version (for now)
/* BASE <- smap(projectabit,withplotting) */
BASE <- smap(projectabit,chans)
