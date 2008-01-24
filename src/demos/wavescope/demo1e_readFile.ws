
// This is an even more generic reader, but it takes its arguments in
// a string.  They're all optional.

// [2007.03.26] For this demo, wscaml is failing for window size greater than...
//   255 succeeded, 256 failed
// (It could make it up to 500 on demo3k with the same inputfile... but no skipbytes)


s1 = (readFile("./countup.raw", 
	       "mode: binary  repeats: 0 "++
	       "skipbytes: 2  window: 255 offset: 2", 
	       timer(44000.0 / 255.0))
      //:: Stream (Sigseg (Int16 * Int16 * Int16)))
      //:: Stream (Int16 * Int16 * Int16))
      //:: Stream (Sigseg (Int16)))
      :: Stream Int16);

main = s1;
//main = iterate(w in s1) {emit (w[[0]], w[[1]], w[[2]]) }
