
// [2007.06.26] This is taking 1.5 seconds on fort, 2.6 on partridge.
// For some reason it takes FIFTY seconds on the ensbox right now.

// Reading one channel takes 2.5 seconds.
// But reading all four and doing a union takes 17-20 seconds!


include "marmot_first_phase.ws";

//BASE <- ch1
//BASE <- unionList([ch1,ch2,ch3,ch4])
//BASE <- rw1
//BASE <- hn
//BASE <- d2
BASE <- synced

