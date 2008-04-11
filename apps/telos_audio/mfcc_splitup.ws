
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
windowSize = 256;  // 1/2 window size used for 16khz sample rate
samplingRate = 8192;
totalFilters = linearFilters + logFilters;

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
  mysqrt(r*r + i*i)
}


negpoint97 = FIX_F2I(0.0-0.97);

fun preemphasize(start, bufR, win) {
  // preemphasize FIR filter into the buffer
  for i = start+1 to start+windowSize-1 {
    bufR[i] := FIX_MPY(win[i-start-1], negpoint97) + win[i-start];    
  };
}

fun hamming(start, bufR, win) {
  // hamming window
  for i = start to start+windowSize-1 {
    bufR[i] := FIX_MPY(bufR[i],hamWindow[i-start]);
  };
}

earmagfn :: (Array Int16, Array Int16, Array Float) -> ();
fun earmagfn(bufR, bufI, earmag) {
  for i = 0 to (fftSize/2)-1 {
    //mag = absC(makeComplex((cast_num(bufR[i])::Float),(cast_num(bufI[i])::Float)));
    mag = complexNorm((cast_num(bufR[i])::Float),
                      (cast_num((bufI[i]::Int16))::Float));
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
    earmag[i] := mylog(earmag[i]) / mylog(10.0);
  };  
}

//============================================================

using TOS;


file :: Stream (Array Int16);
file = (readFile("./snip.raw", "mode: binary skipbytes: 2",
		    timer(819.20 / 255.0))
      :: Stream Int16)
    .arrwindow(windowSize);


namespace Node {

// This reads from the audio board:
signedones = Array:make(windowSize, 0);
sensor = smap(fun(arr) {
    //led1Toggle();
    for i = 0 to windowSize-1 {
      signedones[i] := (cast_num(arr[i]) :: Int16);
    };
    signedones
}, read_telos_audio(windowSize, 256 / 4)); // READ VERY SLOW FOR NOW

// Pick which one you want:
src = sensor;
//Node:src = file;

// Statically allocate the storage:
// real and imaginary vectors

bufR :: Array Int16 = Array:make(fftSize,0);
bufI :: Array Int16 = Array:make(fftSize,0);
emag = Array:make(totalFilters,0.0);

PRINTDBG = false

cleared = iterate arr in Node:src {
  //strt = realtime();
  //print("Running..."++strt++"\n");

  Array:fill(bufR, 0);
  Array:fill(bufI, 0);
  Array:fill(emag, 0.0);

  emit(bufR,bufI,emag,arr);
}

preemph = iterate (bufR,bufI,emag,win) in cleared {
  start = (fftSize-windowSize) / 2;
  preemphasize(start, bufR, win);

  emit(start,bufR,bufI,emag,win);
}

hamm = iterate (start,bufR,bufI,emag,win) in preemph {
  hamming(start, bufR, win);

  led2Toggle();
  emit(start,bufR,bufI,emag,win);
}

freq = iterate (start,bufR,bufI,emag,win) in hamm {
  // fft
  fix_fft(bufR,bufI,9,false);

  led1Toggle();

  emit(start,bufR,bufI,emag,win);
}


emg = iterate (start,bufR,bufI,emag,win) in freq {
  // compute earmag
  earmagfn(bufR, bufI, emag);

  led0Toggle();

  emit(start,bufR,bufI,emag,win);
}

ceps = iterate (start,bufR,bufI,emag,win) in emg {
  cep1 = Array:fold(fun(x,y)(x+y),0.0,emag);

  if PRINTDBG then {
    for i = 0 to totalFilters-1 { 
      println(emag[i]);
    };
    print("\n"); 
    print("#cep1 "++cep1++"\n");
  };
  emit cep1;
  //emit 99;
}

} // End namespace

main = iterate _ in Node:emg { emit 8889 }

// preEmphasized = filter([1 -.97], 1, input);
//    x(n) - x(n-1)*.97



//    fftData(1:windowSize) = preEmphasized(first:last).*hamWindow;
//    fftMag = abs(fft(fftData));
//    earMag = log10(mfccFilterWeights * fftMag');
//    ceps(:,start+1) = mfccDCTMatrix * earMag;
