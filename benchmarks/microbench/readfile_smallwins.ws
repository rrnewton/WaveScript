

// Read windowed data.

// Now we read with small windows, 32 samples.

// With -n 15 (just like the bigwin version), on faith:
//   scheme: 1.9s
//   mlton:  .77/.37 real/user
//   cpp (w/ pull style): .84/.38

// We put an amplifier on it, so we don't have to run the actual timer source at a high rate.
mytimer = iterate _ in timer(25.0) {
  //print("  ******* REAL TIMER FIRING *********** \n");
  for i = 1 to 20000 {
    emit ();
  }
}

file = (readFile("/tmp/dummyfile.bin", "mode: binary window: 32 ", 
	         mytimer)
     :: Stream (Sigseg (Int16)))

// Print something approx every megabyte read.
printevery = 63 * 128 * 4;

BASE <- iterate w in file {
   state { counter = 0 } 
   //print("  (Got window) \n");
   counter += 1;
   if counter == printevery then {
    counter := 0;
    emit w[[16]];
  }
}
