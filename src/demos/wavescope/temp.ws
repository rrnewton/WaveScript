


// FFT every window and output a sample from that FFT, interleaved with zeros.

// Pipeline the ffts into two seperate operators.

fun sigseg_fftR2C (ss) toSigseg(ss`toArray`fftR2C,  ss.start, ss.timebase)

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
     else ENSBoxAudioF(0,4096,0,24000);

s2 :: Stream (Sigseg Complex);
s2 = iterate (w in s1) { emit sigseg_fftR2C(w) };

fun group2(s) { 
 iterate x in s {
  state { fst = Array:makeUNSAFE(0) }
  if Array:length(fst) == 0
  then fst := Array:make(1,x)
  else { 
    emit (fst[0], x);
    fst := Array:makeUNSAFE(0);
  }}
}

fun pipe2(f,s) {
  tmp = iterate (w1,w2) in group2(s) {
    emit (f(w1), w2);
  };
  iterate (w1,w2) in tmp {
    emit w1;
    emit f(w2);
  }
}

s2b = pipe2(sigseg_fftR2C, s1);

//s3 :: Stream Float;
s3 = iterate (win in s2b) {
  state { pos=0 }

  x :: Int = 3;  // Explicit type annotation on local var.
  y = (4 == 4);

  ind = 100;

  if win[[ind]].realpart > 224.0
  then { //emit 0.0; 
    emit (pos/4, win[[ind]].imagpart)
  };

  pos += 1;
};

BASE <- s3
