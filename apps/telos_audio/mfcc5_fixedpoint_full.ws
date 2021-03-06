
include "stdlib.ws";
include "fix_fft.ws";

/*
 *	 MFCC Ported from Matlab code
 *	
 *          -- Malcolm Slaney, August 1993
 *         (c) 1998 Interval Research Corporation 
 */


lowestFrequency = 133.3333;
linearFilters = 13;
linearSpacing = 66.66666666;
//logFilters = 27;
logFilters = 19;    // drop top filters since over sample rate 
logSpacing = 1.0711703;
fftSize = 256;      // half size fft for half sample rate
fftSizeLog2 = 8;      // half size fft for half sample rate
//fftSize = 512;
//fftSizeLog2 = 9;      // half size fft for half sample rate
cepstralCoefficients = 13;
windowSize = 200;  // 1/2 window size used for 16khz sample rate
//windowSize = 400;
samplingRate = 8192;
totalFilters = linearFilters + logFilters;
threshFactor = 0.25;
floatAlpha = 0.95;



// Now figure the band edges.  Interesting frequencies are spaced
// by linearSpacing for a while, then go logarithmic.  First figure
// all the interesting frequencies.  Lower, center, and upper band
// edges are all consequtive interesting frequencies. 

mylog = logF; 
mysqrt = sqrtF
//fun mylog(x) x; fun mysqrt(x) x

lowLogFreq = lowestFrequency + ((linearFilters-1) * linearSpacing);
freqs = Array:build(totalFilters+2,
	  fun (x) {
	    if (x < linearFilters) then
 	      (lowestFrequency + intToFloat(x) * linearSpacing)
            else 
	      (lowLogFreq * (logSpacing ^ (intToFloat(x+1) - linearFilters)))
  	  }
	);

// We now want to combine FFT bins so that each filter has unit
// weight, assuming a triangular weighting function.  First figure
// out the height of the triangle, then we can figure out each 
// frequencies contribution

let (mfccFilterWeightsEvenIdx,
     mfccFilterWeightsEven,
     mfccFilterWeightsEvenFix,
     mfccFilterWeightsOddIdx,
     mfccFilterWeightsOdd,
     mfccFilterWeightsOddFix) = {

  mfccFilterWeightsEvenIdx = Array:make(fftSize/2,0);
  mfccFilterWeightsEven = Array:make(fftSize/2,0.0);

  mfccFilterWeightsOddIdx = Array:make(fftSize/2,0);
  mfccFilterWeightsOdd = Array:make(fftSize/2,0.0);

  for j = 0 to totalFilters-1 {

    // 2.0 may be too small for fixed point?  makes for small numbers..
    triangleHeight = 2.0 / (freqs[j+2]-freqs[j]);
  
    fun f(idxarr,weightarr) {
      for i = 0 to (fftSize/2)-1 {
        f = intToFloat(i)/intToFloat(fftSize)*intToFloat(samplingRate);
        if (f > freqs[j]) && (f <= freqs[j+1]) then {
          weightarr[i] := triangleHeight * (f-freqs[j])/(freqs[j+1]-freqs[j]);
	  idxarr[i] := j+1;
        }
        else 
          if (f > freqs[j+1]) && (f < freqs[j+2]) then {
            weightarr[i] := triangleHeight * (freqs[j+2]-f)/(freqs[j+2]-freqs[j+1]);
	    idxarr[i] := j+1;
          }
      }
    };
 
    if (moduloI(j,2) == 0) then
      f(mfccFilterWeightsEvenIdx,mfccFilterWeightsEven)
    else
      f(mfccFilterWeightsOddIdx,mfccFilterWeightsOdd);
  };

  (mfccFilterWeightsEvenIdx,
   mfccFilterWeightsEven,
   Array:map(FIX_F2I, mfccFilterWeightsEven),
   mfccFilterWeightsOddIdx,
   mfccFilterWeightsOdd,
   Array:map(FIX_F2I, mfccFilterWeightsOdd)
   )

}



/* USE FLOATING POINT */

fun complexNorm(x,y) {
  mysqrt(x*x + y*y)
}

FILTER_WEIGHT_E = mfccFilterWeightsEven;
FILTER_WEIGHT_O = mfccFilterWeightsOdd;
fun MAYBEFIX_MPY(x,y) { x*y }
MAYBEFIX_NORM :: (Int16, Int16) -> Float;
fun MAYBEFIX_NORM(r,i) {
   complexNorm((cast_num(r)::Float),
               (cast_num(i)::Float));
}
log10 = mylog(10.0);
fun MAYBEFIX_LOG10(x) { mylog(x)/log10 }
MAYBEFIX_0 = 0.0; 
type MAYBEFIX_TYPE = Float; 

/* USE FIXED POINT */

/*

FILTER_WEIGHT_E = mfccFilterWeightsEvenFix;
FILTER_WEIGHT_O = mfccFilterWeightsOddFix;
fun MAYBEFIX_MPY(x,y) { FIX_MPY(x,y) }
MAYBEFIX_NORM :: (Int16, Int16) -> Int16;
fun MAYBEFIX_NORM(r,i) { FIX_NORM(r,i) }
fun MAYBEFIX_LOG10(x) { FIX_LOG10(x) }
MAYBEFIX_0 = 0; 
type MAYBEFIX_TYPE = Int16;

*/


hamWindow = Array:build(windowSize, fun (x) {
  FIX_F2I(0.54 - 0.46*cos(2.0*const_PI*(intToFloat(x)/intToFloat(windowSize))))
});



_ = {

for i = 0 to windowSize-1{
  println(hamWindow[i]);
}
  println(freqs);
for i = 0 to (fftSize/2)-1{
  println(mfccFilterWeightsEven[i]++"  "++mfccFilterWeightsOdd[i]++"  "++
    mfccFilterWeightsEvenIdx[i]++"  "++mfccFilterWeightsOddIdx[i]);
}

};


fun arrwindow(S, len) {
    arrbuf = Array:makeUNSAFE(len);
    iterate x in S {
      state{ ind = 0; }
      arrbuf[ind] := x;
      ind += 1;
      if ind == len then {
        emit arrbuf;
        ind := 0;
      }
   }
}

fun complexNorm(r,i) {
  //mysqrt(r*r + i*i)
  (r*r + i*i)
}


/* compute envelope of window */
fun computeEnvelope(start,bufR) {
  max = Mutable:ref(0);
  min = Mutable:ref(0);

  // envelope
  for i = start to start+windowSize-1 {
    if (bufR[i] > max) then max := bufR[i];
    if (bufR[i] < min) then min := bufR[i];
  };

  max-min
}



negpoint97 = FIX_F2I(0.0-0.97);

fun preemphasize(start, bufR, win) {
  // preemphasize FIR filter into the buffer
  for i = start+1 to start+windowSize-1 {
    bufR[i] := FIX_MPY(win[i-start-1], negpoint97) + win[i-start];
  };
}

fun hamming(start, bufR) {
  // hamming window
  for i = start to start+windowSize-1 {
    bufR[i] := FIX_MPY(bufR[i],hamWindow[i-start]);
  };
}

/* 

  Cost breakdown for the floating point version.
  Takes almost 36000 ticks to execute earmag.

sqrt op:          2420 ticks
2 log ops + div:  8709 ticks 
1 log op  + div:  122  ticks  !?!?!?!
just one log:     20 ticks
just div:         19 ticks

full complexNorm: 9867 ticks
cNorm - sqrt    : 7639 ticks

sqrt + log/div : 2467

*/



earmagfn :: (Array Int16, Array Int16, Array MAYBEFIX_TYPE) -> ();
fun earmagfn(bufR, bufI, earmag) {
  for i = 0 to (fftSize/2)-1 {
    mag = MAYBEFIX_NORM(bufR[i], bufI[i]);
    if (mfccFilterWeightsEvenIdx[i] > 0) then {
      earmag[mfccFilterWeightsEvenIdx[i]-1] := 
        earmag[mfccFilterWeightsEvenIdx[i]-1] +
        MAYBEFIX_MPY(FILTER_WEIGHT_E[i], mag);
    };
    if (mfccFilterWeightsOddIdx[i] > 0) then {
      earmag[mfccFilterWeightsOddIdx[i]-1] := 
        earmag[mfccFilterWeightsOddIdx[i]-1] +
        MAYBEFIX_MPY(FILTER_WEIGHT_O[i], mag);
    }
  };
}


fun dologs(earmag) {
  for i = 0 to totalFilters-1 {
    earmag[i] := MAYBEFIX_LOG10(earmag[i]);
  };
}



//============================================================

using TOS;




/* 
/* FOR REGULAR PC */
segs = (readFile("./snip.raw", 
	       "mode: binary  repeats: 0 "++
	       "skipbytes: 0  window: "++windowSize ++" offset: 0", 
	       timer(819.20 / 255.0))
      :: Stream (Sigseg (Int16)));
file = iterate seg in segs {
  emit toArray(seg);
};
*/



namespace Node {

// This reads from the audio board:
//sensor = read_telos_audio(windowSize, windowSize / 4);

// Dummy source for java:
//src = smap(fun(_) Array:build(windowSize, fun(i) (99::Int16)), timer$40);
//RATE = 8000 / windowSize;  // realtime
RATE = 0.5;  // slow, for profiling
outbuf = Array:build(windowSize, fun(i) (100::Int16) + Int16! i);
 dummy = iterate _ in IFPROFILE(Server:timer$RATE,timer$RATE) { emit outbuf };



// Pick which one you want:
//src = sensor
//src = file;
src = dummy;



// Statically allocate the storage:
// real and imaginary vectors

bufR :: Array Int16 = Array:make(fftSize,0);
bufI :: Array Int16 = Array:make(fftSize,0);
emag =  Array:make(totalFilters,MAYBEFIX_0);
ceps =  Array:make(cepstralCoefficients,0.0);
signedones = Array:make(windowSize, 0);



/* convert data to signed from unsigned */
signed = iterate arr in src {
  state { cnt :: Uint16 = 0 }
  //led1Toggle();
  for i = 0 to windowSize-1 {
    signedones[i] := (cast_num(arr[i]) :: Int16);
  };

  cnt += 1;
  dropped = getDroppedInputCount();    
  emit (cnt, dropped, signedones);
};


PRINTDBG = false
PRINTOUTPUT = false

preemph = iterate (cnt, dropped, win) in signed {
  Array:fill(bufR, 0);

  start = (fftSize-windowSize) / 2;
  preemphasize(start, bufR, win);

  emit (cnt, dropped, start,bufR);
}

hamm = iterate (cnt, dropped, start,bufR) in preemph {
  hamming(start, bufR);

  emit(cnt, dropped,  start,bufR);
}

/*
Should use a do-style syntax here:
  chain: 
  do x <- preemph 
     y <- hamming(_,_)
    
But... we could get the some effect if the compiler could fission operators effectively.... Extract a pure spine.
Yet that's not something that one can count on...
*/

fixedAlpha = FIX_F2I(floatAlpha);
fixedOneMinusAlpha = FIX_F2I(1.0-floatAlpha);
fixedThreshFactor = FIX_F2I(threshFactor);

prefilt = iterate (cnt, dropped, start,bufR) in hamm {
  state {
    ewma = 0;
    count = 0;
    dets = 0;
  }

  count := count + 1;
  env = computeEnvelope(start,bufR);
  if (ewma == 0) then ewma := env;
  thold = ewma + FIX_MPY(ewma,fixedThreshFactor);
  if (PRINTOUTPUT) then {
    println("#max= "++count++" "++env);  
    println("#ewm= "++count++" "++thold);  
  };
  ewma := FIX_MPY(fixedAlpha,ewma) + 
          FIX_MPY(fixedOneMinusAlpha,env);
  if (env > thold) then {
    if (PRINTOUTPUT) then println("#tck= "++count++" "++env);  
    dets := dets + 1;
    // DETECTED!
  };

  // hack -- always emit!
  emit(cnt, dropped, start,bufR);
}


freq = iterate (cnt, dropped, start,bufR) in prefilt {

  Array:fill(bufI, 0);

  // fft
  fix_fft(bufR,bufI,fftSizeLog2,false);

//led1Toggle();

  emit(cnt, dropped, start,bufR,bufI);
}


emg = iterate (cnt, dropped, start,bufR,bufI) in freq {
  // Clear 
  Array:fill(emag, 0);

  // compute earmag
  earmagfn(bufR, bufI, emag);
  emit (cnt, dropped, emag);
}

 logs = iterate (cnt,  dropped,emag) in emg {
  dologs(emag);
  //led0Toggle();
  emit (cnt, dropped, emag);
}
 
 ceps_stream = iterate (cnt, dropped,emag) in logs {

  // Clear 
  Array:fill(ceps, 0);

  // compute DCT
  for k = 0 to cepstralCoefficients - 1 {
    for n = 0 to totalFilters - 1 {
      ang = const_PI / (cast_num(totalFilters)::Float) *
            ((cast_num(n)::Float) + 0.5) * 
             (cast_num(k)::Float);
      ceps[k] := ceps[k] + (cast_num(emag[n])::Float) * cos(ang);
    }
  };

  if PRINTOUTPUT then println("#cep= "++ceps[0]);

/*
  if PRINTDBG then {
    for i = 0 to totalFilters-1 { 
      println(emag[i]);
    };
    print("\n"); 
    print("#cep1 "++cep1++"\n");
  };
*/
  //led0Toggle();led1Toggle();led2Toggle();

  emit (cnt, dropped, ceps);
}

} // End namespace



using Node;

//_main = Curry:smap(Array:length) $ signed
main = signed
//main = freq
//main = Node:emg
//main = iterate _ in Node:emg { emit 8889 }
//main = ceps_stream

// preEmphasized = filter([1 -.97], 1, input);
//    x(n) - x(n-1)*.97

// Cut it up into bits:
/*
chunk = 7
__main = iterate arr in main {
  state { arrs = Array:build(29, fun(i) Array:make(chunk, 0)) }
  j = 0;
  k = 0; 
  Array:foreachi(fun(i,x) {
    arrs[j][k] := arr[i];
    k += 1;
    if k == chunk then {
      emit arrs[j];
      k := 0;
      j += 1;
    };
  }, arr);
}
*/

/*
len = 3
little = Array:makeUNSAFE(len)
_main = iterate arr in main {
  for i = 0 to len {
    little[i] := arr[i]
  };
  emit little;  
}
*/

foo as (dr,arr) = main
main = foo
//main = iterate (dr,_) in main { emit dr }
//main = iterate _ in timer$ 3 { emit (88 :: Int16) }

//    fftData(1:windowSize) = preEmphasized(first:last).*hamWindow;
//    fftMag = abs(fft(fftData));
//    earMag = log10(mfccFilterWeights * fftMag');
//    ceps(:,start+1) = mfccDCTMatrix * earMag;
