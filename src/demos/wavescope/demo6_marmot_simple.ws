
fun marmotscore(w) { 3.8 }

// This does a simple marmot detection with no inlining.

ch1 = audio(0, 4096, 0);
ch2 = audio(1, 4096, 0);
ch3 = audio(2, 4096, 0);
ch4 = audio(3, 4096, 0);

/* ch1 = audioFile("./countup.raw", 4096, 0); */
/* ch2 = ch1; */
/* ch3 = ch1; */
/* ch4 = ch1; */

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

hn = smap(hanning, rw1);

freq = smap(fft, hn);

wscores = iterate (w in freq) {
  emit(marmotscore(w), w);
};

detections = 
  iterate (pr in wscores) {
    let (x,y) = pr;
    emit(true, x, x);
  };

//synced = sync4( 

BASE <- detections;
