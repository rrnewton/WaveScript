include "stdlib.ws"
include "fifostatic.ws"
/* include "coeffs.ws"  */
include "fix_fft.ws"

using TOS;
using Mutable;

zip_bufsize = 1

/*
 This is the beginnings of a statically-allocated version of eugene.ws
*/
/* svmKernelPar = 1.00 */
/* threshold    = 0.1 */
/* consWindows = 3 // number of consecutive windows of detections; */

/* // detector values */
/* fun SVMOutput(svmVectors, svmCoeffs, svmBias, svmKernelPar, arr)  { */
/*   using Array; */
/*   using Mutable; */

/*   //  diff_norm_squared :: (Array Float, Array Float) -> Float; */
/*   fun diff_norm_squared(a, b) { */
/*     acc = ref(0); */
/*     //    println("diff_norm_squared lengths"++a.length++" "++b.length); */
/*     for i = 0 to a.length-1 { */
/*       x = logF(a[i]); */
/*       y = b[i]; */
/* /\*       println(i ++ ": " ++ y ++ ", "++ x); *\/ */
/*       acc := acc + (x-y) * (x-y); */
/*     }; */
/*     acc */
/*   }; */

/*   // there are 30 vectors, we need to diff norm squared with a different one each time */
/*   ySVM = ref(svmBias); */
/*   for i = 0 to svmVectors.length - 1 { */
/*     norm :: Float = diff_norm_squared(arr, svmVectors[i]); */
/*     denom = svmKernelPar * arr.length.gint; */
/*     ySVM := ySVM + svmCoeffs[i] * expF((0-norm)/denom); */
/*   }; */
/*   ySVM */
/* } */

/* fun BinaryClassify(threshold, consWins, strm) { */
/*   using FIFO; */
/*   iterate ySVM in strm { */
/*     state {  */
/*       detect = { tmp = make(consWins); */
/*                  for i = 1 to consWins { tmp.enqueue(false) };   */
/*    	         tmp } */
/*     } */
/*     detect.dequeue(); */
/*     if ySVM > threshold then { */
/*       detect.enqueue(true); */
/*       // if both detections were true, then we trigger */
/*       // we can do this more intelligently, but just saving previous info */
/*       emit FIFO:andmap(fun(x) x, detect); */
/*     } else { */
/*       detect.enqueue(false); */
/*       emit false; */
/*     } */
/*   } */
/* } */


// ============================================================
// Stream / Signal operators

// zipN is redefined here so as to be in scope of fifostatic.ws:
myZipN :: (Int, List (Stream a)) -> Stream (Array a);
fun myZipN(bufsize, slist) {
  using List;
  len = slist`List:length;
  outputBuf = Array:makeUNSAFE(len);
  // Trying this out here:
  bufs = Array:build(len, fun(_) FIFO:make(bufsize));
  iterate (ind, elem) in unionList(slist) {
    //state { bufs = Array:build(len, fun(_) FIFO:make(bufsize)) }
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
  iterate arr in zipN(zip_bufsize, strmlst) {
    for k = 0 to winsize / arr[0].length - 1 {
      temp = arr[k];
      for i = 0 to temp.length - 1 {
	buf[k*temp.length+i] := temp[i];
      }
    }
    emit buf;
  }
}

/* AddOddAndEven :: (Int, Stream (Array Float), Stream (Array Float)) -> Stream (Array Float); */
AddOddAndEven :: (Int, Stream (Array Int16), Stream (Array Int16)) -> Stream (Array Int16);
fun AddOddAndEven(winsize, s1,s2) {
  //    assert_eq("AddOddAndEven", first.width, second.width);
  using Array;
  buf = make(winsize, 0);
  iterate arr in myZipN(zip_bufsize, [s1,s2]) {   
    state { _stored_value = 0; }
    first = arr[0];
    second = arr[1];
/*     print(first.length); */
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


// implementation of an FIR filter using convolution 
// you have to provide an array of coefficients 
FIRFilter :: (Int, Array Int16, Stream (Array Int16)) -> Stream (Array Int16);
fun FIRFilter(bufsize, filter_coeff, strm) {
    using Array;
    nCoeff = filter_coeff.length;
    
    _flipped_filter_coeff =  // array of _filter_coefficients
      build(nCoeff, fun(i) filter_coeff[nCoeff-1-i]);

    // remembers the previous points needed for convolution
    _memory = FIFO:make(nCoeff);
    for i = 1 to nCoeff-1 { FIFO:enqueue(_memory, 0) };

    outputBuf = make(bufsize,0);

    iterate buf in strm {
      for j = 0 to buf.length - 1 {
        //inspect$ _memory;

        // add the first element of the input buffer into the array
/*         FIFO:enqueue(_memory, buf[j]); */


        for i = 0 to nCoeff-1 {
/* 	  outputBuf[j] := outputBuf[j] +  */
/* 	   _flipped_filter_coeff[i] * FIFO:peek(_memory, i); */
	  outputBuf[j] := outputBuf[j] +
	  FIX_MPY(_flipped_filter_coeff[i], FIFO:peek(_memory, i));
        };

 

/* 	FIFO:dequeue(_memory); */
      };
      emit outputBuf;
    }
}

fun MagWithScale(scale, stm) {
  fun sum(acc,n) (acc + absI16(n)/scale);
  smap(fun(seg) Array:fold(sum, 0, seg), stm)
}

/* hHigh_Even = #[-0.2304, -0.6309, 0.1870, -0.0329];             */
/* hHigh_Odd  = #[0.7148, -0.0280, 0.0308, -0.0106]; */
hHigh_Even = #[-7550, -20673, 6127, -1079];
hHigh_Odd = #[23421,   -918, 1009, -348];

HighFreqFilter :: (Int, Stream (Array Int16)) -> Stream (Array Int16);
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
/* hLow_Even = #[-0.0106, 0.0308, -0.0280, 0.7148]; */
/* hLow_Odd  = #[0.0329, -0.1870, 0.6309, 0.2304]; */
hLow_Even = #[-348, 1009, -918, 23421];
hLow_Odd = #[1078, -6128, 20672, 7549];

LowFreqFilter :: (Int, Stream (Array Int16)) -> Stream (Array Int16);
fun LowFreqFilter(winsize, input) {
  evenSignal = GetEven(winsize / 2, input);
  oddSignal  = GetOdd (winsize / 2, input);

  // now filter
  lowFreqEven = FIRFilter(winsize / 2, hLow_Even, evenSignal);
  lowFreqOdd  = FIRFilter(winsize / 2, hLow_Odd,  oddSignal);

  // now recombine them
  AddOddAndEven(winsize / 2, lowFreqEven, lowFreqOdd);
}

//filterGains = #[1.4142, 1.8684, 2.6412, 3.7352, 5.2818, 7.4668, 10.5596, 11.3137];
filterGains = #[46339, 61221, 86544, 122391, 173068, 244664, 346006, 370716];

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

  //zipN(zip_bufsize, [level4, level5, level6]);
  myZipN(zip_bufsize, [level4, level5, level6]);
  // TEMP TEMP FIXME:  HACKING AROUND:
  //level6
  //lowFreq1
}

/*
fun castToFloat(winsize, stm) {
  floats = Array:make(winsize, 0.0);
  iterate arr in stm {
    for i = 0 to arr.Array:length - 1 {
      floats[i] := (cast_num(arr[i]) :: Float);
    };
    emit floats;
  }
}
*/

fun process_channel(winsize, stm) {
/*   casted = castToFloat(winsize, stm); */
  filter_results = GetFeatures(winsize, stm);
  filter_results
}
    
namespace Node {

  // DANGER TOGGLING THIS FOR EXPERIMENTS:
  //THERATE = 256.0 / 400.0;  // Accurante "realtime" rate.
  THERATE = 1.0 / 15.0; // Every 10 seconds...
  winsize = 8;
  //winsize = 32;
  //winsize = 256;

  // input winsize
  //winsize = 512;

  NUM_CHANNELS = 1;
  NUM_FEATURES = 3;
  // For running on the PC:
  prefix = "patient36_file16/";
  //sensor = smap(toArray, (readFile(prefix++"FT10-T8-short.txt", "mode: binary", timer(2.0)) :: Stream Int16).window(winsize));

  // A dummy datasource for java:
  //sensor = [COUNTUP(0).arrwindow(winsize)];
  //sensor = smap(fun(_) Array:build(winsize, fun(i) Int16!i), timer$1);
  outbuf = Array:build(winsize, fun(i) Int16!i);
  sensor = iterate _ in timer$ THERATE { led1Toggle(); emit outbuf };

  // For running on Telos:
  //sensor = read_telos_audio(winsize, 1000) // 1 khz  

  // This is statically allocated, do a big for loop?
  filtered = map(fun(s) process_channel(winsize, s), [sensor]);

/*   filtered = GetFeatures(winsize, hHigh_Odd, cast); */
  // flat = FlattenZip(NUM_CHANNELS*NUM_FEATURES, filtered);
  //main = List:fold1(merge,filtered)

/*  svmStrm = smap(fun(arr) SVMOutput(svmVectors, svmCoeffs, svmBias, svmKernelPar, arr), flat) */

}


//main = castToFloat(winsize, Node:inputs.head);



//main = AddOddAndEven(winsize/2,
//                     GetEven(winsize/2, castToFloat(winsize, Node:inputs.head)),
//                     GetOdd(winsize/2, castToFloat(winsize, Node:inputs.head)))




//ret = FIRFilter(Node:winsize, hLow_Even, Node:sensor);
//ret = LowFreqFilter(Node:winsize, Node:sensor);

ret = MagWithScale(filterGains[3],
        FIRFilter(Node:winsize, hLow_Even, GetEven(Node:winsize, Node:sensor)));

//main = iterate _ in Node:filtered.head { emit 389 }
//main = Node:flat
//main = Node:sensor

main = iterate _ in ret { emit 389 }

