

// Read windowed data.

// Let's time reading 1875 windows, approximately 30 mb.

// [2007.10.29] 
// Well, -n 15 takes ~1s in Scheme.
//  320ms real (280 user) in mlton
//  52ms in wsc -O2 (with no usleep at all)
//   45/10ms real/user when setBatchSize is used.



fun amplify(n,s)
  iterate x in s {
    for i = 1 to n {
      emit x;
    }
  }

// We put an amplifier on it, so we don't have to run the actual timer source at a high rate.
mytimer = amplify(1000, timer(10.0))

file = (readFile("/tmp/dummyfile.bin", "mode: binary window: 16384 ", 
	         mytimer)
     :: Stream (Sigseg (Int16)))

// Print something approx every megabyte read.
printevery = 63;

BASE <- iterate w in file {
   state { counter = 0 } 
   counter += 1;
   if counter == printevery then {
    counter := 0;
    emit w[[100]];
  }
}
