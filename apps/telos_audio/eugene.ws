
//include "staticfifo.ws"
include "stdlib.ws"
include "coeffs.ws"

// ============================================================
// Constant parameters

SAMPLING_RATE_IN_HZ = 200
SAMPLES_PER_WINDOW  = 512 //(2*SAMPLING_RATE_IN_HZ)

//NUM_CHANNELS        = 21;
NUM_CHANNELS        = 2;
// MASSIVE code explosion.
// 10 Channels -> 222 kloc .c, 2mb executable, -O0
//  (Can't even compile -O3 for lack of memory.)
//  (-O0 8 sec, -O1 30 sec 1mb, -O2 50s )
// Execution time for 1000 svmKernelPar boolean outputs drops from
// 2sec to .82 sec with -O1.  -O2 = .7 sec


channelNames = ["FT10-T8","FT9-FT10","T7-FT9","P7-T7",
		"CZ-PZ","FZ-CZ","P8-O2","T8-P8","F8-T8",
		"FP2-F8","P4-O2","C4-P4","F4-C4","FP2-F4",
		"P3-O1","C3-P3","F3-C3","FP1-F3","P7-O1",
		"T7-P7","F7-T7", "FP1-F7"]


// detector values
svmKernelPar = 1.00
threshold    = 0.1

consWindows = 3 // number of consecutive windows of detections;

winsize = 512

// ============================================================
// Classifiers

fun SVMOutput(svmVectors, svmCoeffs, svmBias, svmKernelPar, strm)  {
  using Array;

  //diff_norm_squared :: (Array Float, Array Float) -> Float;
  fun diff_norm_squared(a, b) {
    // vectors should be the same length
    foldi(fun(i,acc,ai) {
        x = logF(absF(ai));
        y = b[i];
        acc + (x-y) * (x-y);
      }, 
      0, a)
  };
  smap(fun(arr)
         foldi(fun(i, ySVM, vec) {
	        norm :: Float = diff_norm_squared(arr, vec);
   	        //norm = 0.0;
	        denom = svmKernelPar * arr.length.gint;
                ySVM + svmCoeffs[i] * expF((0-norm)/denom)
              },
	      svmBias, svmVectors), // wonder if this will work   
       strm)
}

fun BinaryClassify(threshold, consWins, strm) {
  using FIFO;
  iterate ySVM in strm {
    state { 
      detect = { tmp = make(consWins);
                 for i = 1 to consWins { tmp.enqueue(false) };  
   	         tmp }
    }
    detect.dequeue();
    if ySVM > threshold then {
      detect.enqueue(true);
      // if both detections were true, then we trigger
      // we can do this more intelligently, but just saving previous info
      emit FIFO:andmap(fun(x) x, detect);
    } else {
      detect.enqueue(false);
      emit false;
    }
  }
}

// ============================================================
// Stream / Signal operators

//zip_bufsize = winsize * 2
zip_bufsize = 1

fun FlattenZip(strmlst)
  smap(Array:flatten, zipN(zip_bufsize,strmlst))

fun AddOddAndEven(s1,s2) 
 iterate arr in zipN(zip_bufsize, [s1,s2]) { //zip2_sametype(s1,s2) {
    state { _stored_value = 0 }
    first = arr[0];
    second = arr[1];
    using Array;
    assert_eq("AddOddAndEven", first.width, second.width);
    buf = make(first.width, 0);
    for i = 0 to first.width - 1 {
      buf[i] := first[[i]] + _stored_value;
      _stored_value = second[[i]]; // we don't add the last odd guy, but store
    };
    emit toSigseg(buf, first.start, first.timebase);
  }

fun GenericGet(offset, strm)
  iterate seg in strm {
    arr = Array:build(seg.width / 2, fun(i) seg[[(i*2)+offset]]);
    emit toSigseg(arr, seg.start, seg.timebase);
  }

fun GetOdd (strm) GenericGet(1, strm)
fun GetEven(strm) GenericGet(0, strm)

// implementation of an FIR filter using convolution 
// you have to provide an array of coefficients 
fun FIRFilter(filter_coeff, strm) {
    using Array;
    nCoeff = filter_coeff.length;
    
    _flipped_filter_coeff =  // array of _filter_coefficients
      build(nCoeff, fun(i) filter_coeff[nCoeff-1-i]);      

    iterate seg in strm {
      state {
        // remembers the previous points needed for convolution
        _memory = { fifo = FIFO:make(nCoeff+1);
                    for i = 1 to nCoeff { FIFO:enqueue(fifo, 0) };
                    fifo }
      }
      buf = seg.toArray;
      outputBuf = make(seg.width,0);
      for j = 0 to seg.width - 1 {
        // add the first element of the input buffer into the array
        FIFO:enqueue(_memory, buf[j]);
        for i = 0 to nCoeff-1 {
	  outputBuf[j] := outputBuf[j] + _flipped_filter_coeff[i] * FIFO:peek(_memory, i);
        };
	FIFO:dequeue(_memory);
      };
      emit toSigseg(outputBuf, seg.start, seg.timebase);
    }
}


fun MagWithScale(scale, stm) {
  fun sum(acc,n) (n + acc/scale);
  smap(fun(seg) Sigseg:fold(sum, 0, seg), stm)
}

hHigh_Even = #[-0.2304, -0.6309, 0.1870, -0.0329]            
hHigh_Odd  = #[0.7148, -0.0280, 0.0308, -0.0106 ]

fun HighFreqFilter(input) {
  evenSignal = GetEven(input);
  oddSignal  = GetOdd(input);

  // now filter
  highFreqEven = FIRFilter(hHigh_Even, evenSignal);
  highFreqOdd  = FIRFilter(hHigh_Odd, oddSignal);

  // now recombine
  AddOddAndEven(highFreqEven, highFreqOdd)
}

// Filter coefficients
hLow_Even = #[-0.0106, 0.0308, -0.0280, 0.7148]    
hLow_Odd  = #[0.0329, -0.1870, 0.6309, 0.2304]
  
fun LowFreqFilter(input) {
  evenSignal = GetEven(input);
  oddSignal  = GetOdd (input);

  // now filter
  lowFreqEven = FIRFilter(hLow_Even, evenSignal);
  lowFreqOdd  = FIRFilter(hLow_Odd,  oddSignal);

  // now recombine them
  AddOddAndEven(lowFreqEven, lowFreqOdd)
}


filterGains = #[1.4142, 1.8684, 2.6412, 3.7352, 5.2818, 7.4668, 10.5596, 11.3137]

fun GetFeatures(input) {
  lowFreq3 = LowFreqFilter $ LowFreqFilter $ LowFreqFilter $ input;

  highFreq4 = HighFreqFilter(lowFreq3); // we want this one
  lowFreq4  = LowFreqFilter(lowFreq3); 
  level4    = MagWithScale(filterGains[3], highFreq4);

  highFreq5 = HighFreqFilter(lowFreq4); // and this one 
  lowFreq5  = LowFreqFilter(lowFreq4); 
  level5    = MagWithScale(filterGains[4], highFreq5); 

  highFreq6 = HighFreqFilter(lowFreq5); // and this one
  // lowFreq6 = LowFreqFilter(lowFreq5); 
  level6    = MagWithScale(filterGains[5], highFreq6);

  zipN(zip_bufsize, [level4, level5, level6])
}

fun process_channel(stm) {
  rw = window(stm,winsize);
  filter_results = GetFeatures(rw);  
  filter_results
}


// ============================================================
// Wire together the application stream graph:

// These are the types for the top-level streams:
inputs  :: List (Stream Float);
flat    :: Stream (Array Float);
svmStrm :: Stream Float;
detect  :: Stream Bool;

inputs = {
  //prefix = "patient8_files/XXXXE3I5_";
  prefix = "patient36_file16/";
  ticktock = Server:timer(SAMPLING_RATE_IN_HZ);
  map(fun(ch) smap(int16ToFloat, 
                  (readFile(prefix++ch++"-short.txt",  "mode: binary", ticktock)
                   :: Stream Int16)), List:prefix(channelNames, NUM_CHANNELS))
}

filtered = map(process_channel, inputs)

flat = FlattenZip(filtered);

// Hack: currently uses global variables: svmVectors, svmCoeffs, and svmBias
nVectors = 30
pruned = Array:build(nVectors, fun(i) svmVectors[i])
svmStrm = SVMOutput(pruned, svmCoeffs, svmBias, svmKernelPar, flat)

detect = BinaryClassify(threshold, consWindows, svmStrm)

     //main = detect
//main = LowFreqFilter $ LowFreqFilter $ inputs.head.window(winsize)
main = filtered.head
