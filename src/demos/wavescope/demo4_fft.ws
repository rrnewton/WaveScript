
// FFT every window and output a sample from that FFT, interleaved with zeros.

// [2006.07.24] Takes 163 seconds to process the 40,000 windows of data in a 315mb raw file.
// Was that with a quadruple FFT?

// run ./get_sample_data first

fun mywindow(S, len)
  iterate(x in S) {
    state{
      arr = nullarr;
      ind = 0;
      startsamp = 0;
    }
    if ind == 0 then arr := makeArray(len, x);
    arr[ind] := x;
    ind := ind + 1;
    if ind == len
    then {
      emit toSigseg(arr, startsamp, nulltimebase);
      ind := 0;
      arr := makeArray(len, x);
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
  //state{ foo = (nullarr :: Array Int);   }
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

BASE <- s3;
