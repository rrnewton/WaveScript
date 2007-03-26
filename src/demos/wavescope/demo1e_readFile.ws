
// This is an even more generic reader, but it takes its arguments in
// a string.  They're all optional.


s1 = (readFile("./countup.raw", 
	       "mode: binary  rate: 44000  repeats: 0 "++
	       "skipbytes: 2  window: 50 offset: 2")
      //:: Stream (Sigseg (Int16 * Int16 * Int16)))
      //:: Stream (Int16 * Int16 * Int16))
      :: Stream (Sigseg (Int16)))
      //:: Stream Int16);

//BASE <- s1;
BASE <- iterate(w in s1) {emit (w[[0]], w[[1]], w[[2]]) }
