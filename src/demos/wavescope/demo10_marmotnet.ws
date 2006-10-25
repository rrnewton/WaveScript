
// Standard library included by default.
//   Used: syncN, rewindow

include "marmot_lib.ws";
//   Used: marmotscore, detect

//========================================
// Main query:

ch1 = audio(0, 4096, 0);
ch2 = audio(1, 4096, 0);
ch3 = audio(2, 4096, 0);
ch4 = audio(3, 4096, 0);

outwidth=100;
dummydetections = iterate(w in ch1) {
  state { 
    pos = 0; 
    flag = false; 
  }
  emit(flag, pos, pos + outwidth - 1);
  pos := pos + outwidth;
  flag := if flag then false else true;
};

// 96 samples are ignored between each 32 used:
rw1 = rewindow(ch1, 32, 96); 

//hn = smap(hanning, rw1);
hn = myhanning(rw1);

freq = smap(fft, hn);

//wscores = smap(fun(w){(marmotscore(w), w)}, freq);
wscores = iterate (w in freq) { emit (marmotscore(w), w); }

detections = detect(wscores);

//========================================
// Now go to the net for the first time:

//netdetects : Signal (Int, (Bool, Int, Int));
netdetects = broadcast(detections);

// Then we sync:
//synced = syncN(detections, [ch1, ch2, ch3, ch4]);
synced = syncN(dummydetections, [ch1, ch2, ch3, ch4]);

// Then we use network data (as it slowly comes in), to prune our candidates further.

/* round2 = iterate(x in union2(synced, netdetects)) { */
/*   state { */
/*     buffer = makeArray(10,(false,0,0)); */
/*     bufstart = 0; */
/*     bufend = 0; // exclusive */
/*   } */
/*   match x { */
/*     Firstof2 (w1, w2, w3, w4) -> */
/*       if bufstart == bufend  */
/*       // Haven't heard from the network yet... */
/*       then  */
    
    
/*     // Service head of buffer. */

/*     | Secondof2 (b, st, en) -> */
/*       // Drop oldest... add to buffer */
/*       buffer[bufend] := (b,st,en); */
/*       bufend := bufend + 1; */
/*   } */
   
/* } */





// [2006.09.04] RRN: Currently it doesn't ever detect a marmot.
// If you try to do the real syncN, it will process the whole without outputing anything.
//BASE <- synced;

//detections
//QUERY = detections
//fun main() { detections }
//BASE(2) <- detections


BASE <- detections;
