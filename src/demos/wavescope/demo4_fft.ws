
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


fun mywindow(S, len)
  iterate(x in S) {
    state{
      arr = Array:null;
      ind = 0;
      startsamp = 0;
    }
    if ind == 0 then arr := Array:make(len, x);
    arr[ind] := x;
    ind := ind + 1;
    if ind == len
    then {
      emit toSigseg(arr, startsamp, nulltimebase);
      ind := 0;
      arr := Array:make(len, x);
      startsamp := startsamp + len;
    }
  };

s1 :: Stream (Sigseg Float);
s1 = if GETENV("WSARCH") != "ENSBox" 
     then {chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) 
                    :: Stream (Int16 * Int16 * Int16 * Int16));
	   mywindow(iterate((a,_,_,_) in chans){ emit int16ToFloat(a) }, 4096) }
     else ENSBoxAudio(0,4096,0,24000);

//if GETENV("WSARCH") == "ENSBox" 

s2 :: Stream (Sigseg Complex);
s2 = iterate (w in s1) {
  //state{ foo = (Array:null :: Array Int);   }
  //print(foo);  print("\n");

  emit fft(w);
};

s3 :: Stream Float;
s3 = iterate (win in s2) {
  x :: Int = 3;  // Explicit type annotation on local var.
  y = (4 == 4);

  if win[[100]].realpart > 224.0
  then { emit 0.0; emit win[[100]].imagpart; }
  else { }
};

BASE <- 
//s1
//iterate(x in s2) { emit x[[30]] };
iterate(x in s1) { emit x`width };
