
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
fftSize = 512;
cepstralCoefficients = 13;
windowSize = 200;  // 1/2 window size used for 16khz sample rate
samplingRate = 8192;
totalFilters = linearFilters + logFilters;
threshFactor = 1.25;

// Now figure the band edges.  Interesting frequencies are spaced
// by linearSpacing for a while, then go logarithmic.  First figure
// all the interesting frequencies.  Lower, center, and upper band
// edges are all consequtive interesting frequencies. 

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
     mfccFilterWeightsOddIdx,
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
   //Array:map(FIX_F2I, )
   mfccFilterWeightsEven,
   mfccFilterWeightsOddIdx,
   //Array:map(FIX_F2I, )
   mfccFilterWeightsOdd)

}

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



s1 = (readFile("./snip.raw", 
	       "mode: binary  repeats: 0 "++
	       "skipbytes: 0  window: "++windowSize ++" offset: 0", 
	       timer(819.20 / 255.0))
      :: Stream (Sigseg (Int16)));

fun abs(s) {if (s < 0) then 0-s else s};

fun mfcc(s) {

iterate seg in s {

  state {
    ewma = 0.0;
    count = 0;
  }
   
  win = toArray(seg);

  // real and imaginary vectors
  bufR = Array:make(fftSize,0);
  bufI = Array:make(fftSize,0);

  start = (fftSize-windowSize) / 2;

  // preemphasize FIR filter into the buffer
  for i = start+1 to start+windowSize-1 {
    bufR[i] := FIX_MPY(win[i-start-1],FIX_F2I(0.0-0.97)) + win[i-start];    
  };

  max = Mutable:ref(0);
  min = Mutable:ref(0);

  // hamming window
  for i = start to start+windowSize-1 {
    bufR[i] := FIX_MPY(bufR[i],hamWindow[i-start]);
    if (bufR[i] > max) then max := bufR[i];
    if (bufR[i] < min) then min := bufR[i];
  };

  env = max-min;
  count := count + 1;
  println("&max= "++count++" "++env);  
  if (ewma == 0) then ewma := (cast_num(env)::Float);
  ewma := 0.95*ewma + 0.05*(cast_num(env)::Float);
  println("ewma= "++count++" "++ewma*threshFactor);  
  if ((cast_num(env)::Float) > ewma * threshFactor) then {
  println("tick= "++count++" "++env);  
};

  //println("Post hamming: "++bufR);

  // fft
  fix_fft(bufR,bufI,9,false);

  // compute earmag
  earmag = Array:make(totalFilters,0.0);

  for i = 0 to (fftSize/2)-1 {
    mag = absC(makeComplex((cast_num(bufR[i])::Float),(cast_num(bufI[i])::Float)));
    if (mfccFilterWeightsEvenIdx[i] > 0) then {
      earmag[mfccFilterWeightsEvenIdx[i]-1] := 
        earmag[mfccFilterWeightsEvenIdx[i]-1] +
        mfccFilterWeightsEven[i]*mag;
    };
    if (mfccFilterWeightsOddIdx[i] > 0) then {
      earmag[mfccFilterWeightsOddIdx[i]-1] := 
        earmag[mfccFilterWeightsOddIdx[i]-1] +
        mfccFilterWeightsOdd[i]*mag;
    }
  };

  for i = 0 to totalFilters-1 {
    earmag[i] := logF(earmag[i])/logF(10.0);
  };

  // compute DCT
  dct = Array:make(cepstralCoefficients,0.0);
  for k = 0 to cepstralCoefficients - 1 {
    for n = 0 to totalFilters - 1 {
      ang = const_PI / (cast_num(totalFilters)::Float) *
            ((cast_num(n)::Float) + 0.5) * 
             (cast_num(k)::Float);
      dct[k] := dct[k] + earmag[n] * cos(ang);
    }
  }    

  println("#cep1 "++count++" "++dct[0]);

  emit(dct);
}}

BASE <- iterate w in mfcc(s1) {
  for i = 0 to cepstralCoefficients-1 { 
    println(w[i]);
  };
  print("\n"); 
}

// preEmphasized = filter([1 -.97], 1, input);
//    x(n) - x(n-1)*.97



//    fftData(1:windowSize) = preEmphasized(first:last).*hamWindow;
//    fftMag = abs(fft(fftData));
//    earMag = log10(mfccFilterWeights * fftMag');
//    ceps(:,start+1) = mfccDCTMatrix * earMag;
