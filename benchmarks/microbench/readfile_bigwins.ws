

// Read windowed data.

// Let's time reading 1875 windows, approximately 30 mb.

// [2007.10.29] 
// Well, -n 15 takes ~1s in Scheme.
//  320ms real (280 user) in mlton
//  52ms in wsc -O2 (with no usleep at all)
//   45/10ms real/user when setBatchSize is used.

include "stdlib.ws"

fun amplify(n,s)
  iterate x in s {
    for i = 1 to n {
      emit x;
    }
  }

// We put an amplifier on it, so we don't have to run the actual timer source at a high rate.
mytimer = amplify(1000, timer(10.0))

// [2008.08.11] This file should be symlinked to some large file on the local disk:
file = (readFile(//"/tmp/dummyfile.bin", 
                 "6sec_marmot_sample.raw", 
                 "mode: binary window: 16384 ", 
	         mytimer)
     :: Stream (Sigseg (Int16)))

// Print something approx every megabyte read.
//printevery = 63; // 2mb
printevery = 31;   // 1mb

BASE <- iterate w in file {
  state { counter = 0;
          checksum :: Int64 = 0;
         }
   counter += 1;
   
   //checksum += Int64! w[[100]];
   Sigseg:foreach(fun(x) checksum += Int64! x, w);

   if counter == printevery then {
    counter := 0;
    //emit w[[100]];
    emit checksum;
  }
}
