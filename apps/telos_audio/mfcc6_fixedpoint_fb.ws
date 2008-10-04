
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
   Array:map(FIX_F2I_16_16, mfccFilterWeightsEven),
   mfccFilterWeightsOddIdx,
   mfccFilterWeightsOdd,
   Array:map(FIX_F2I_16_16, mfccFilterWeightsOdd)
   )

}



/* USE FLOATING POINT */

fun MAYBEFIX_MPY(x,y) { x*y }
log10 = mylog(10.0);
fun MAYBEFIX_LOG10(x) { mylog((cast_num(x)::Float))/log10 }
MAYBEFIX_0 = 0.0; 
type MAYBEFIX_TYPE = Float; 

/* USE FIXED POINT */

/*

fun MAYBEFIX_MPY(x,y) { FIX_MPY(x,y) }
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


earmagfn :: (Array Int16, Array Int16, Array Int32) -> ();
fun earmagfn(bufR, bufI, fb) {
  for i = 0 to (fftSize/2)-1 {
    ri = lshiftI32((cast_num$ (bufR[i])::Int32),8);
    ii = lshiftI32((cast_num$ (bufI[i])::Int32),8);
    mag = FIX_NORM_16_16(ri,ii);
    //println(" r "++ri++" i "++ii++" n "++mag);
    if (mfccFilterWeightsEvenIdx[i] > 0) then {
      fb[mfccFilterWeightsEvenIdx[i]-1] := 
        fb[mfccFilterWeightsEvenIdx[i]-1] +
        FIX_MPY_16_16(mfccFilterWeightsEvenFix[i], mag);
    };
    if (mfccFilterWeightsOddIdx[i] > 0) then {
      fb[mfccFilterWeightsOddIdx[i]-1] := 
        fb[mfccFilterWeightsOddIdx[i]-1] +
        FIX_MPY_16_16(mfccFilterWeightsOddFix[i], mag);
    }
  };
}




fun dologs(fb,earmag) {
  for i = 0 to totalFilters-1 {
    earmag[i] := MAYBEFIX_LOG10(FIX_I2F_16_16(fb[i]));
  };
}



//============================================================

using TOS;


namespace Node {


// Statically allocate the storage:
// real and imaginary vectors

bufR :: Array Int16 = Array:make(fftSize,0);
bufI :: Array Int16 = Array:make(fftSize,0);
signedones = Array:make(windowSize, 0);

// HACK: Padding this out to slip in some extra data:
// Obviously, this requires that the code below not be sensitive to the length of these arrays:
fbank :: Array Int32 =  Array:make(totalFilters + 2, 0); 
emag =  Array:make(totalFilters + 2, MAYBEFIX_0);
ceps =  Array:make(cepstralCoefficients + 2, 0.0);



/*==================== This uses the env var PLATFORM to switch back and forth. ====================*/

platform = GETENV("PLATFORM")

//PRINTOUTPUT = true
PRINTOUTPUT = false

signed = 
 if List:member(platform, ["", "PC", "pc"]) then {
  // If we're running on the PC side, we just read the data from a file:

  ticks = timer(819.20 / 255.0);
  //ticks :: Stream () = foreign_source("NODE_ENTRY", ["custom_timer.c"]);
  segs = (readFile("./snip.raw", 
   	       "mode: binary  repeats: 0 "++
	         "skipbytes: 0  window: "++windowSize ++" offset: 0", 
  	         ticks)
        :: Stream (Sigseg (Int16)));
	
  print$"Configuring to read audio stream from file 'snip.raw'.\n"; 

  // Return value:
  iterate seg in segs {
    emit toArray(seg);
   }
 } 

else if List:member(platform, ["JAVA", "TELOS", "java", "telos"]) then

/* ==================== FOR JAVA AND TELOS ==================== */
  {
    // This reads from the audio board:
    // Unfortunately, this will only compile with wstiny:

    sensor = read_default_audio(windowSize, windowSize / 4);

    // Dummy source for java:
    //src = smap(fun(_) Array:build(windowSize, fun(i) (99::Int16)), timer$40);
    REALRATE = 8000 / windowSize;  // realtime
    SLOWRATE = 0.5;  // slow, for profiling on telos
    outbuf = Array:build(windowSize, fun(i) (99::Uint16));
    dummy = iterate _ in IFPROFILE(Server:timer$REALRATE, timer$SLOWRATE) { emit outbuf };

    // Pick which one you want:
    //src = sensor;
    //src = dummy;
    src = if GETENV("LIVE") == "" 
          then { print$"Configuring with DUMMY data stream.\n";     dummy } 
          else { print$"Configuring for LIVE audio data stream.\n"; sensor};

    /* convert data to signed from unsigned */
    signed = iterate arr in src {
      //led1Toggle();
      for i = 0 to windowSize-1 {
	signedones[i] := (cast_num(arr[i]) :: Int16);
      };
      emit signedones;
    };
    signed
  }

 else wserror("Unknown value for environment variable PLATFORM: "++platform)
/* ============================================================= */

marked = iterate x in signed {
    state { cnt :: Int = 0 }
    cnt += 1;
    dropped = getDroppedInputCount();    
    emit (cnt, dropped, x);
}

PRINTDBG = false

preemph = iterate (cnt, dropped, win) in marked {
  Array:fill(bufR, 0);

  start = (fftSize-windowSize) / 2;
  preemphasize(start, bufR, win);

  emit(cnt,dropped, start,bufR);
}

hamm = iterate (cnt,dropped, start,bufR) in preemph {
  hamming(start, bufR);

  emit(cnt,dropped, start,bufR);
}


fixedAlpha = FIX_F2I(floatAlpha);
fixedOneMinusAlpha = FIX_F2I(1.0-floatAlpha);
fixedThreshFactor = FIX_F2I(threshFactor);

prefilt = iterate (cnt,dropped, start,bufR) in hamm {
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
    //println("#max= "++count++" "++env);  
    //println("#ewm= "++count++" "++thold);  
  };
  ewma := FIX_MPY(fixedAlpha,ewma) + 
          FIX_MPY(fixedOneMinusAlpha,env);
  if (env > thold) then {
    //if (PRINTOUTPUT) then println("#tck= "++count++" "++env);  
    dets := dets + 1;
    // DETECTED!
  };

  // hack -- always emit!
  emit(cnt,dropped,  start,bufR);
}


freq = iterate (cnt,dropped, start,bufR) in prefilt {

  Array:fill(bufI, 0);

  // fft
  fix_fft(bufR,bufI,fftSizeLog2,false);

//led1Toggle();

  emit(cnt,dropped, start,bufR,bufI);
}


emg = iterate (cnt,dropped, start,bufR,bufI) in freq {
  // Clear 
  Array:fill(fbank, 0);

  // compute earmag
  earmagfn(bufR, bufI, fbank);

  // Hack: stick the metadata into the array so that we can send it
  // back in spite of the WEAK marshaling support under tinyos
  // presently.
    
  //emit (cnt,dropped, fbank);
  fbank[totalFilters]   := Int32! cnt;
  fbank[totalFilters+1] := Int32! dropped;
  emit fbank;  
}

logs = iterate fbank in emg {

  // clear
  Array:fill(emag, 0);

  // compute logs
  dologs(fbank,emag);
  //led0Toggle();

  // HACK: Copy over cnt, dropped:
  emag[totalFilters]   := Float! fbank[totalFilters];
  emag[totalFilters+1] := Float! fbank[totalFilters+1];
  emit emag;
}

//final_results = #[0,0];

ceps_stream = iterate emag in logs {

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


  //if PRINTOUTPUT then println("#cep= "++ceps[0]);

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


  //emit (cnt,dropped, ceps);
  //emit cnt;

  // HACK: Copy over cnt, dropped:  
  ceps[cepstralCoefficients]   := emag[totalFilters];
  ceps[cepstralCoefficients+1] := emag[totalFilters+1];
  emit ceps;

}


} // End Node namespace
using Node;

//main = signed
//main = freq
//main = emg
main = ceps_stream

//main = smap(fun((x,y,z)) x, emg)
//main = smap(fun((x,y,z)) z, ceps_stream)
