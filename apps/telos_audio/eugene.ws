

include "stdlib.ws"

include "coeffs.ws"

// ============================================================
// Constant parameters

SAMPLING_RATE_IN_HZ = 256
NUM_CHANNELS        = 21
SAMPLES_PER_WINDOW  = (2*SAMPLING_RATE_IN_HZ)


channelNames = ["FT10-T8","FT9-FT10","T7-FT9","P7-T7",
		"CZ-PZ","FZ-CZ","P8-O2","T8-P8","F8-T8",
		"FP2-F8","P4-O2","C4-P4","F4-C4","FP2-F4",
		"P3-O1","C3-P3","F3-C3","FP1-F3","P7-O1",
		"T7-P7","F7-T7", "FP1-F7"]


// detector values
svmKernelPar = 1.00
threshold    = 0.1

consWindows = 3 // number of consecutive windows of detections;

// ============================================================
// Stream operators

fun SVMOutput(a,b,c,d) ()
fun BinaryClassify(a,b) ()


fun GetEven(s) s
fun GetOdd(s) s


fun AddOddAndEven(s1,s2) s1

// implementation of an FIR filter using convolution 
// you have to provide an array of coefficients 
fun FIRFilter(filter_coeff, strm) {
    using Array;
    len = filter_coeff.length;
    
    _flipped_filter_coeff =  // array of _filter_coefficients
      build(len, fun(i) filter_coeff[len-1-i]);      

    iterate seg in strm {
      state {
        // remembers the previous points needed for convolution
        _memory = { fifo = FIFO:make(len+1);
	
		    //      _memory.push_back((T)0);????

                    for i = 1 to len { FIFO:enqueue(fifo, 0) };
                    fifo
                  }
      }
      buf = seg.toArray;
      outputBuf = make(len,0);
      for j = 0 to len-1 {
        // add the first element of the input buffer into the array
        FIFO:enqueue(_memory, buf[j]);
        for i = 0 to len-1 {
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

  defaultZipN([level4, level5, level6])
}

fun process_channel(stm) {
  rw = window(stm,512);
  filter_results = GetFeatures(rw);  
  filter_results
}


// ============================================================
// Wire together the application stream graph:

inputs :: List (Stream Float);
inputs = {
  //prefix = "patient8_files/XXXXE3I5_";
  prefix = "patient36_file16/";
  ticktock = timer(SAMPLING_RATE_IN_HZ);
  map(fun(ch) smap(int16ToFloat, 
                  (readFile(prefix++ch++"-short.txt",  "mode: binary", ticktock)
                   :: Stream Int16)), channelNames)
}

// get the SVM box that we're going to use

svmClass = SVMOutput(SVs, SVCoeff, 30, svmKernelPar)
detect = BinaryClassify(threshold, consWindows)

filtered = map(process_channel, inputs)

//flatten = BinaryClassify( svmClass( FlattenZip(filtered)))

//main = inputs.head
main = filtered.head

// ============================================================

/*


*/
