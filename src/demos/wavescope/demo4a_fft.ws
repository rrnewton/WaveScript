
// FFT every window and output a sample from that FFT, interleaved with zeros.

// [2006.07.24] Takes 163 seconds to process the 40,000 windows of data in a 315mb raw file.
// Was that with a quadruple FFT?

// run ./get_sample_data first

// [2007.03.22] Wow, just for reading streams of data a tuple at a
// time... there's a lot of scheduler overhead.
//
// For example, just returning "s1" from this program on a 1.2 mb file:
//   ws     : 364 ms (from saved heap, using scheme fft)
//   wsc    : 1300-2900 ms (high variance)
//   wscaml : 13 ms

// Then returning 's2' which includes fft:
//   ws     : 772 ms (from saved heap, using scheme fft - 460 minus startup and w/ fftw)
//   wsc    : 2000 ms (high variance)
//   wscaml : 31 ms
// (Total process times.)


// [2007.06.19] It looks like I hacked this demo down to work with wscaml.
// I should restore it to its full glory at some point.

// [2007.06.19] Looking at how my improved array representation affects things.
// Rev 1498 of stable branch of engine... using user time reported by the engine.
// Uniprocessor vanilla options: 0.6 sec vs  4.0     Big improvement!
//   w/ -j 1 --at_once         :  .5     vs  2.5 
//   w/ -O3                    :  .25    vs  1.0
// W/ both processors enabled  :  .40    vs  1.5

// Note that all these numbers are *bad* because the caml version takes
// .15 seconds even though that's counting the entire process's
// execution (and the limitation that it's using double precision).


fun sigseg_fftR2C     (ss) toSigseg(ss`toArray`fftR2C, ss.start, ss.timebase)
fun memosigseg_fftR2C (ss) toSigseg(ss`toArray`memoized_fftR2C, ss.start, ss.timebase)

fun amplify(n,s)
  iterate x in s {
    for i = 1 to n {
      emit x;
    }
  }

winsize = 4096
//winsize = 32;

/*
s1a :: Stream (Sigseg Float);
s1a = if GETENV("WSARCH") != "ensbox" 
     then {chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) 
                    :: Stream (Int16 * Int16 * Int16 * Int16));
	   mywindow(iterate((a,_,_,_) in chans){ emit int16ToFloat(a) }, winsize) }
     else ensBoxAudioF(0);
*/

s0 = (readFile("6sec_marmot_sample.raw", 
               //"mode: binary  rate: 24000  window: 32  skipbytes: 6 ") :: Stream (Sigseg Int16));
	       "mode: binary  window: "++ winsize ++"  skipbytes: 6 ",
	       //amplify(40, amplify(50, timer(10.0)))
	       amplify(1700, timer(10.0))
	       //timer(10.0);
	       ) :: Stream (Sigseg Int16));
s1b = iterate w in s0 {
  arr = Array:build(w.width, fun (i) int16ToFloat(w[[i]]));
  emit toSigseg(arr, w.start, nulltimebase)
}

s1 = s1b;


testarr = List:toArray$ [1.0, 2.0, 3.0, 4.0];

s2a = iterate w in s1 { emit sigseg_fftR2C(w) }
s2b = iterate w in s1 {

  //println("TEST: "++ memoized_fftR2C$ List:toArray$ [1, 2, 3, 4] );
  println("TEST1: "++ testarr );
  println("TEST2: "++ memoized_fftR2C$ testarr );

  println("  Going in: "++w[[0]]);
  out = memosigseg_fftR2C(w);
  println("   Coming out: "++out[[0]]);
  emit out;
}

s2 :: Stream (Sigseg Complex);
s2 = if GETENV("MEMOIZE") == "" then s2b else s2a;

// Emit a number drawn from a fixed position in the fft output.
//s3 :: Stream Float;
s3 = iterate win in s2 {
  state { pos::Int = 0 ;
          strttime = 0;
          first = true;
        }

  if first == true then {
    first := false; strttime := clock();
    print("Setting start time: "++ strttime ++"\n");
  };
  
  x :: Int = 3;  // Explicit type annotation on local var.
  y = (4 == 4);

  ind = 100;

  //  print(win[[ind]].realpart ++ " ");
  if win[[ind]].realpart > 224.0
  then { //emit 0.0; 
    print("UserTimeElapsed: ");
    print(show(max(0, clock() - strttime)));
    print("\n");
    emit (pos/4, win[[ind]].imagpart)
  };

  pos += 1;
};


main = s3
//iterate(x in s2) { emit x[[30]] };
//iterate(x in s1) { emit x`width };
