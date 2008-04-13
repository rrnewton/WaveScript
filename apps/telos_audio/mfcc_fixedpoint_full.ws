
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
//fftSize = 512;
cepstralCoefficients = 13;
windowSize = 200;  // 1/2 window size used for 16khz sample rate
//windowSize = 400;
samplingRate = 8192;
totalFilters = linearFilters + logFilters;
threshFactor = 1.25;
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
     mfccFilterWeightsOddFix,
     mfccFilterWeightsOdd) = {

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
fun MAYBEFIX_MPY(x,y) { 
  (cast_num(x)::Float)*(cast_num(y)::Float)
}
MAYBEFIX_NORM :: (Int16, Int16) -> Float;
fun MAYBEFIX_NORM(r,i) {
   complexNorm((cast_num(r)::Float),
               (cast_num(i)::Float));
}
log10 = mylog(10.0);
fun MAYBEFIX_LOG10(x) { mylog(x)/log10 }
MAYBEFIX_0 = 0.0; 
MAYBEFIX_TYPE :: Float; 

/* USE FIXED POINT */

/*

FILTER_WEIGHT_E = mfccFilterWeightsEvenFix;
FILTER_WEIGHT_O = mfccFilterWeightsOddFix;
fun MAYBEFIX_MPY(x,y) { FIX_MPY(x,y) }
MAYBEFIX_NORM :: (Int16, Int16) -> Int16;
fun MAYBEFIX_NORM(r,i) { FIX_NORM(r,i) }
fun MAYBEFIX_LOG10(x) { FIX_LOG10(x) }
MAYBEFIX_0 = 0; 
MAYBEFIX_TYPE :: Int16;

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


// Cheesy, cast and cast back:
file :: Stream (Array Uint16);
/*
file = smap(fun(x) (cast_num(x) :: Uint16),
            (readFile("./snip.raw", "mode: binary skipbytes: 0",
 	  	    timer(819.20 / 255.0))
             :: Stream Int16))
    .arrwindow(windowSize);
*/

// For java just using a timer:
file = iterate _ in timer$1 {
  //state { offset = 0 }
  emit Array:build(windowSize, fun(i) Uint16!i )
}

namespace Node {

//sensor = read_telos_audio(windowSize, windowSize / 4);

// Pick which one you want:
//src = IFPROFILE(file, sensor);
//src = sensor
//src = file;

// Dummy source for java:
//src = smap(fun(_) Array:build(windowSize, fun(i) (99::Int16)), timer$40);
//RATE = 8000 / windowSize;  // realtime
RATE = 0.5;  // slow, for profiling
outbuf = Array:build(windowSize, fun(i) (99::Int16));
src = iterate _ in IFPROFILE(Server:timer$RATE,timer$RATE) { emit outbuf };

// This reads from the audio board:
signedones = Array:make(windowSize, 0);
signed = smap(fun(arr) {
    //led1Toggle();
    for i = 0 to windowSize-1 {
      signedones[i] := (cast_num(arr[i]) :: Int16);
    };
    signedones
}, src); // READ VERY SLOW FOR NOW


// Statically allocate the storage:
// real and imaginary vectors

bufR :: Array Int16 = Array:make(fftSize,0);
bufI :: Array Int16 = Array:make(fftSize,0);
emag =  Array:make(totalFilters,MAYBEFIX_0);
ceps =  Array:make(cepstralCoefficients,0.0);


PRINTDBG = false

preemph = iterate win in signed {
  Array:fill(bufR, 0);

  start = (fftSize-windowSize) / 2;
  preemphasize(start, bufR, win);

  emit(start,bufR);
}

hamm = iterate (start,bufR) in preemph {
  hamming(start, bufR);

  emit(start,bufR);
}


fixedAlpha = FIX_F2I(floatAlpha);
fixedOneMinusAlpha = FIX_F2I(1.0-floatAlpha);
fixedThreshFactor = FIX_F2I(threshFactor);

prefilt = iterate (start,bufR) in hamm {
  state {
    ewma = 0;
    count = 0;
    dets = 0;
  }

  env = computeEnvelope(start,bufR);
  if (ewma == 0) then ewma := env;
  count := count + 1;
  ewma := FIX_MPY(fixedAlpha,ewma) + 
          FIX_MPY(fixedOneMinusAlpha,env);
  if (env > FIX_MPY(ewma,fixedThreshFactor)) then {
    dets := dets + 1;
    // DETECTED!
  };

  // hack -- always emit!
  emit(start,bufR);
}


freq = iterate (start,bufR) in prefilt {

  Array:fill(bufI, 0);

  // fft
  fix_fft(bufR,bufI,9,false);

//led1Toggle();

  emit(start,bufR,bufI);
}


emg = iterate (start,bufR,bufI) in freq {
  // Clear 
  Array:fill(emag, 0);

  // compute earmag
  earmagfn(bufR, bufI, emag);
  emit emag;
}

logs = iterate emag in emg {
  dologs(emag);
  //led0Toggle();
  emit emag;
}

ceps = iterate emag in logs {

  // compute DCT
  for k = 0 to cepstralCoefficients - 1 {
    for n = 0 to totalFilters - 1 {
      ang = const_PI / (cast_num(totalFilters)::Float) *
            ((cast_num(n)::Float) + 0.5) * 
             (cast_num(k)::Float);
      ceps[k] := ceps[k] + (cast_num(emag[n])::Float) * cos(ang);
    }
  };

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

  emit ceps;
}

} // End namespace

//main = Node:signed
//main = Node:freq
//main = Node:emg
//main = iterate _ in Node:emg { emit 8889 }
main = Node:ceps

// preEmphasized = filter([1 -.97], 1, input);
//    x(n) - x(n-1)*.97



//    fftData(1:windowSize) = preEmphasized(first:last).*hamWindow;
//    fftMag = abs(fft(fftData));
//    earMag = log10(mfccFilterWeights * fftMag');
//    ceps(:,start+1) = mfccDCTMatrix * earMag;
