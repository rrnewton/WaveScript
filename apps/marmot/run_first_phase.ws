
// [2007.06.26] This is taking 1.5 seconds on fort, 2.6 on partridge.
// For some reason it takes FIFTY seconds on the ensbox right now.

// Reading one channel takes 2.5 seconds.
// But reading all four and doing a union takes 17-20 seconds!

/* [2007.07.01] Timing different implementations:

 Timing against "3min_marmot_sample.raw" (31.4mb) on faith.
 With blocked sigseg reading.
  ws.opt  : ms
  wsc     :  ms ( real)
  wscaml  :  ms ( real) (copyalways)
  wsmlton :  ms ( real) (copyalways)

Ah... good to see, with the current scheduler (rev 1495) wsc
actually does as well with both processors... at least for block
reading.

Just ran on ARM (1.1 mb input)
  MLton:  real  1.273s  user 1.150s
  wsc  :  real  4.519s  user 3.780s

*/


include "marmot_first_phase.ws";

//BASE <- ch1
//BASE <- unionList([ch1,ch2,ch3,ch4])
//BASE <- rw1
//BASE <- hn
//BASE <- d2
BASE <- synced
