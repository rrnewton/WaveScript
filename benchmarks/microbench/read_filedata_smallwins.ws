

// Read windowed data.

// Now we read with small windows, 32 samples.

// With -n 15 (just like the bigwin version), on faith:
//   scheme: 1.9s
//   mlton:  .77/.37 real/user
//   cpp (w/ pull style): .84/.38

file = (readFile("/tmp/dummyfile.bin", "mode: binary window: 32 ", 
	         timer(10.0))
     :: Stream (Sigseg (Int16)))

// Print something approx every megabyte read.
printevery = 63 * 128 * 4;

BASE <- iterate w in file {
   state { counter = 0 } 
   counter += 1;
   if counter == printevery then {
    counter := 0;
    emit w[[16]];
  }
}
