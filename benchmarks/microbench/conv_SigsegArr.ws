

// Convert back and forth between sigsegs and arrays.

// This should be pretty efficient, because the sigseg doesn't become fragmented at all.
// Does 200K conversions of 16K data per output tuple.

// [2007.10.30] The first time I ran it the C++ was going too slow for
// me to wait for it to complete.  I realized I still had an
// implementation that used "seg-get" to unpack the sigseg.  That's
// quadratic in theory, but the sigsegs should have all consisted of
// exactly one array segment.  Still, it's inefficient.

// I fixed that bug and C++ came down to 41s on faith (vs. <1s ml & 3s scheme)

// This is not a fair comparison, however, because both MLton and
// Scheme are doing the optimization wherein they don't really copy
// the data.  C++ can't do this because of garbage collection issues.

// When I turn off that optimization in MLton, its performance drops a
// HUGE amount, to take 533.6 seconds, vs. C++'s 41s.  Apparently, the
// copying it does loses to memcpy.  Perhaps we're also suffering
// garbage collection there.

// What I need to do is implement a consistent "optimize level" for
// the generated WS code, short-circuiting the copy should only happen
// in "-O3" where unsafe optimizations are permitted.

file = (readFile("/tmp/dummyfile.bin", "mode: binary window: 16384 ", 
	         timer(10.0))
     :: Stream (Sigseg (Int16)))

//printevery = 63;
//printevery = 100;
printevery = 2;

main = iterate w in file {
   state { counter = 0;  }
   counter += 1;
   for i = 1 to 5000 {
     tmp1 = toArray(w);
     tmp2 = toSigseg(tmp1, w.start, w.timebase);
     if counter == printevery then {
       counter := 0;
       emit tmp2[[0]];
     }
   };
}
