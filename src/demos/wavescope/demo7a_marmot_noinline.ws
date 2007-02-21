

// This is the skeleton for a simple marmot detection with no inlining.

chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) :: Stream (Int * Int * Int * Int));
ch1 = window(iterate((a,_,_,_) in chans){ emit intToFloat(a) }, 4096);
ch2 = window(iterate((_,b,_,_) in chans){ emit intToFloat(b) }, 4096);
ch3 = window(iterate((_,_,c,_) in chans){ emit intToFloat(c) }, 4096);
ch4 = window(iterate((_,_,_,d) in chans){ emit intToFloat(d) }, 4096);

/* ch1 = ENSBoxAudio(0, 4096, 0, 44000); */
/* ch2 = ENSBoxAudio(1, 4096, 0, 44000); */
/* ch3 = ENSBoxAudio(2, 4096, 0, 44000); */
/* ch4 = ENSBoxAudio(3, 4096, 0, 44000); */

newwidth = 32;
step = 32;
rw1 = iterate (w in ch1) {
  state { acc = nullseg; }
  acc := joinsegs(acc, w);
  for i = 1 to w.width {
    if acc.width > newwidth
    then {emit subseg(acc, acc.start, newwidth);
	  acc := subseg(acc, acc.start + step, acc.width - step)}
    else break;
  }};

// TODO: Insert hanning code here:
//hn = hanning(rw1);
hn = rw1;

freq = iterate(x in hn) { emit fft(x) };

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
