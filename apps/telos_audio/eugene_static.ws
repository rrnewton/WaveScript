include "stdlib.ws"
include "fifostatic.ws"
include "coeffs.ws" 

using TOS;
using Mutable;

SAMPLING_RATE_IN_HZ = 256
SAMPLES_PER_WINDOW  = 512 //(2*SAMPLING_RATE_IN_HZ)
//NUM_CHANNELS        = 22;
NUM_CHANNELS        = 1;
NUM_FEATURES = 3;

// This one takes 100ms for one chan as opposed to 700ms for eugene.ws
// It takes 8.6 seconds for 22 chans though.  The code bloat is
// probably hurting (that's a 3mb executable).  Oh wait, no it's just
// the O2/O3 difference.  Because in O2 only 1chan takes 388ms --
// almost 4 times as long.

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

zip_bufsize = 1

/*
 This is the beginnings of a statically-allocated version of eugene.ws
*/
svmKernelPar = 1.00
threshold    = 0.1
consWindows = 3 // number of consecutive windows of detections;

// detector values
fun SVMOutput(svmVectors, svmCoeffs, svmBias, svmKernelPar, arr)  {
  using Array;
  using Mutable;

  //  diff_norm_squared :: (Array Float, Array Float) -> Float;
  fun diff_norm_squared(a, b) {
    acc = ref(0);
    //    println("diff_norm_squared lengths"++a.length++" "++b.length);
    for i = 0 to a.length-1 {
      x = logF(a[i]);
      y = b[i];
/*       println(i ++ ": " ++ y ++ ", "++ x); */
      acc := acc + (x-y) * (x-y);
    };
    acc
  };

  // there are 30 vectors, we need to diff norm squared with a different one each time
  ySVM = ref(svmBias);
  for i = 0 to svmVectors.length - 1 {
    norm :: Float = diff_norm_squared(arr, svmVectors[i]);
    denom = svmKernelPar * arr.length.gint;
    ySVM := ySVM + svmCoeffs[i] * expF((0-norm)/denom);
  };
  ySVM
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

// zipN is redefined here so as to be in scope of fifostatic.ws:
myZipN :: (Int, List (Stream a)) -> Stream (Array a);
fun myZipN(bufsize, slist) {
  using List;
  len = slist`List:length;
  outputBuf = Array:makeUNSAFE(len);
  iterate (ind, elem) in unionList(slist) {
    state { bufs = Array:build(len, fun(_) FIFO:make(bufsize)) }
    using FIFO;
//println("  Enqueuing in "++ind++" currently has "++bufs[ind]`FIFO:elements);
    enqueue(bufs[ind], elem);
    if Array:andmap(fun(q) not(empty(q)), bufs)
      then {
      for i = 0 to len-1 {
	outputBuf[i] := dequeue(bufs[i]);
      };

      emit outputBuf;
    }
  }
}

fun FlattenZip(winsize, strmlst) {
  using Array;
  buf = make(winsize, 0);
  iterate arr in myZipN(zip_bufsize, strmlst) {
    for k = 0 to winsize / arr[0].length - 1 {
      temp = arr[k];
      for i = 0 to temp.length - 1 {
	buf[k*temp.length+i] := temp[i];
      }
    }
    emit buf;
  }
}

AddOddAndEven :: (Int, Stream (Array Float), Stream (Array Float)) -> Stream (Array Float);
fun AddOddAndEven(winsize, s1,s2) {
  //    assert_eq("AddOddAndEven", first.width, second.width);
  using Array;
  buf = make(winsize, 0);
  //iterate arr in myZipN(zip_bufsize, [s1,s2]) {   
  iterate arr in zipN(zip_bufsize, [s1,s2]) {   
    state { _stored_value = 0; }
    first = arr[0];
    second = arr[1];
    //print("    first len "++first.length++" whereas winsize= "++winsize++" "); 
    for i = 0 to first.length - 1 {
      buf[i] := first[i] + _stored_value;
      _stored_value := second[i]; // we don't add the last odd guy, but store
    };
    emit buf;
  }
}

fun GenericGet(offset, winsize, strm) {
  arr = Array:make(winsize, 0);
  iterate seg in strm {
    for i = 0 to winsize - 1 {
      //	println("generic get "++i++" which maps to "++(i*2)+offset++" of "++winsize);
      arr[i] := seg[(i*2)+offset];
    };
    emit arr;
  }
}

fun GetOdd (winsize, strm) GenericGet(1, winsize, strm);
fun GetEven(winsize, strm) GenericGet(0, winsize, strm);

fun myRound(val)
{
	roundF(val*1000)/1000;
}

// implementation of an FIR filter using convolution 
// you have to provide an array of coefficients 
FIRFilter :: (Int, Array Float, Stream (Array Float)) -> Stream (Array Float);
fun FIRFilter(bufsize, filter_coeff, strm) {
    using Array;
    nCoeff = filter_coeff.length;
    
    _flipped_filter_coeff =  // array of _filter_coefficients
      build(nCoeff, fun(i) filter_coeff[nCoeff-1-i]);

    // remembers the previous points needed for convolution
    _memory = FIFO:make(nCoeff);
    for i = 1 to nCoeff-1 { FIFO:enqueue(_memory, 0.0) };

    //println("Calling firfilter with coeffs: "++filter_coeff++"\n");

    outputBuf = make(bufsize,0);

    iterate buf in strm {
      Array:fill(outputBuf,0);

      //println("input: "++ buf);
      for j = 0 to buf.length - 1 {
        //inspect$ _memory;

        // add the first element of the input buffer into the array
	//print ("memory: ");   
        FIFO:enqueue(_memory, buf[j]);
        for i = 0 to nCoeff-1 {
	  outputBuf[j] := outputBuf[j] + 
	   _flipped_filter_coeff[i] * FIFO:peek(_memory, i);
	  //print (" "++myRound(FIFO:peek(_memory,i))++", ");  
        };
	//println("  output: "++myRound(outputBuf[j]));  

	FIFO:dequeue(_memory);
      };

      //print(" sending out: "++ outputBuf++"\n");

      emit outputBuf;
      //print(" after emit: "++ outputBuf++"\n");
    }
}

fun MagWithScale(scale, stm) {
  fun sum(acc,n) (acc + absF(n)/scale);
  smap(fun(seg) Array:fold(sum, 0, seg), stm)
}

hHigh_Even = #[-0.2304, -0.6309, 0.1870, -0.0329];            
hHigh_Odd  = #[0.7148, -0.0280, 0.0308, -0.0106];

HighFreqFilter :: (Int, Stream (Array Float)) -> Stream (Array Float);
fun HighFreqFilter(winsize, input) {
  evenSignal = GetEven(winsize / 2, input);
  oddSignal  = GetOdd(winsize / 2, input);

  // now filter
  highFreqEven = FIRFilter(winsize / 2 , hHigh_Even, evenSignal);
  highFreqOdd  = FIRFilter(winsize / 2, hHigh_Odd, oddSignal);

  // now recombine
  AddOddAndEven(winsize / 2, highFreqEven, highFreqOdd);
}

// Filter coefficients
hLow_Even = #[-0.0106, 0.0308, -0.0280, 0.7148];
hLow_Odd  = #[0.0329, -0.1870, 0.6309, 0.2304];

LowFreqFilter :: (Int, Stream (Array Float)) -> Stream (Array Float);
fun LowFreqFilter(winsize, input) {
  evenSignal = GetEven(winsize / 2, input);
  oddSignal  = GetOdd (winsize / 2, input);

  // now filter
  lowFreqEven = FIRFilter(winsize / 2, hLow_Even, evenSignal);
  lowFreqOdd  = FIRFilter(winsize / 2, hLow_Odd,  oddSignal);

  //lowFreqEven = evenSignal;
  //lowFreqOdd = oddSignal;

  // now recombine them
  //AddOddAndEven(winsize / 2, snoop("REAL FLTRED: ", lowFreqEven), lowFreqOdd);
  AddOddAndEven(winsize / 2, lowFreqEven, lowFreqOdd);
}

filterGains = #[1.4142, 1.8684, 2.6412, 3.7352, 5.2818, 7.4668, 10.5596, 11.3137];

fun GetFeatures(winsize, input) {
  lowFreq1 = LowFreqFilter(winsize, input);
  lowFreq2 = LowFreqFilter(winsize / 2, lowFreq1);
  lowFreq3 = LowFreqFilter(winsize / 4, lowFreq2);

  highFreq4 = HighFreqFilter(winsize / 8, lowFreq3); // we want this one
  lowFreq4  = LowFreqFilter(winsize / 8, lowFreq3); 
  level4    = MagWithScale(filterGains[3], highFreq4);

  highFreq5 = HighFreqFilter(winsize / 16, lowFreq4); // and this one 
  lowFreq5  = LowFreqFilter(winsize / 16, lowFreq4); 
  level5    = MagWithScale(filterGains[4], highFreq5); 

  highFreq6 = HighFreqFilter(winsize / 32, lowFreq5); // and this one
  // lowFreq6 = LowFreqFilter(lowFreq5); 
  level6    = MagWithScale(filterGains[5], highFreq6);
/*   println(level6); */

  zipN(zip_bufsize, [level4, level5, level6]);
}


fun castToFloat(winsize, stm) {
  floats = Array:make(winsize, 0.0);
  iterate arr in stm {
    for i = 0 to arr.Array:length - 1 {
      floats[i] := (cast_num(arr[i]) :: Float);
    };
    emit floats;
  }
}

fun process_channel(winsize, stm) {
  casted = castToFloat(winsize, stm);
  filter_results = GetFeatures(winsize, casted);
  filter_results
}

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

    
namespace Node {
  
  // For running on the PC:
  prefix = "patient36_file16/";
  //sensor = smap(toArray, (readFile(prefix++"FT10-T8-short.txt", "mode: binary", timer(2.0)) :: Stream Int16).window(winsize));

  inputs :: List (Stream (Array Float));
  __inputs = {
    prefix = "patient36_file16/";
    //postfix = "-short.txt";
    postfix = ".txt";
    ticktock = Server:timer(SAMPLING_RATE_IN_HZ);
    map(fun(ch) smap(int16ToFloat, 
	             (readFile(prefix++ch++postfix,  "mode: binary", ticktock)
	              :: Stream Int16)) .arrwindow(winsize),
        List:reverse(List:prefix(channelNames, NUM_CHANNELS)))
  }

  // This is just a dummy datasource for java:
  //inputs = [COUNTUP(0).arrwindow(winsize)];
  inputs = [smap(fun(_) Array:build(winsize, fun(i) Float!i), timer$1)];

  // For running on Telos:
  //sensor = read_telos_audio(winsize, 1000) // 1 khz  

  // This is statically allocated, do a big for loop?
  filtered = map(fun(s) process_channel(winsize, s), inputs);

/*   filtered = GetFeatures(winsize, hHigh_Odd, cast); */
  flat = FlattenZip(NUM_CHANNELS*NUM_FEATURES, filtered);
  //main = List:fold1(merge,filtered)

 svmStrm = smap(fun(arr) SVMOutput(svmVectors, svmCoeffs, svmBias, svmKernelPar, arr), flat)

}


//main = castToFloat(winsize, Node:inputs.head);

//main = LowFreqFilter(winsize, castToFloat(winsize, Node:inputs.head))

//main = AddOddAndEven(winsize/2,
//                     GetEven(winsize/2, castToFloat(winsize, Node:inputs.head)),
//                     GetOdd(winsize/2, castToFloat(winsize, Node:inputs.head)))

//main = FIRFilter(winsize, hLow_Even, GetEven $ Node:inputs.head);
//main = FIRFilter(winsize, hLow_Even, Node:inputs.head);


//main = Node:inputs.head
//main = Node:filtered.head
//main = Node:flat
main = Node:svmStrm
