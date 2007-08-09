fun window(S, len) 
  iterate(x in S) {
    state{ 
      arr = Array:null;
      ind = 0; 
      startsamp = 0`gint;
    }
    if ind == 0 then arr := Array:make(len, x);
    arr[ind] := x;
    ind := ind + 1;
    if ind == len
    then {
      emit toSigseg(arr, startsamp, nulltimebase);
      ind := 0;
      arr := Array:make(len, x); 
      startsamp := startsamp + len`intToInt64;
    }
  };


// This is the skeleton for a simple marmot detection with no inlining.

chans = (readFile("6sec_marmot_sample.raw", "mode: binary", timer(44000.0))
         :: Stream (Int16 * Int16 * Int16 * Int16));
ch1 = window(iterate((a,_,_,_) in chans){ emit int16ToFloat(a) }, 4096);
ch2 = window(iterate((_,b,_,_) in chans){ emit int16ToFloat(b) }, 4096);
ch3 = window(iterate((_,_,c,_) in chans){ emit int16ToFloat(c) }, 4096);
ch4 = window(iterate((_,_,_,d) in chans){ emit int16ToFloat(d) }, 4096);

/* ch1 = ENSBoxAudio(0, 4096, 0, 44000); */
/* ch2 = ENSBoxAudio(1, 4096, 0, 44000); */
/* ch3 = ENSBoxAudio(2, 4096, 0, 44000); */
/* ch4 = ENSBoxAudio(3, 4096, 0, 44000); */

newwidth = 32;
step = 32;
rw1 = iterate (w in ch1) {
  state { acc = nullseg; }
  acc := joinsegs(acc, w);
  while acc.width > newwidth {
    emit subseg(acc, acc.start, newwidth);
    acc := subseg(acc, acc.start + step`intToInt64, acc.width - step);
  }
};

// TODO: Insert hanning code here:
//hn = hanning(rw1);
hn = rw1;

  fun sigseg_fftR2C (ss) toSigseg(ss`toArray`fftR2C,  ss.start, ss.timebase)
freq = iterate(x in hn) { emit sigseg_fftR2C(x) };

//fun marmotscore(w) { 3.8 }

wscores = iterate (w in freq) {
  //emit(marmotscore(w), w);
  emit (3.8, w); 
};

detections = 
  iterate (pr in wscores) {
    let (sc,w) = pr;

    // PAD IT FOR TESTALL_DEMOS:
    emit(false, 0.0);
    emit(true, sc);
  };

//synced = sync4( 

BASE <- detections;
