include "stdlib.ws"
include "fifostatic.ws"
//include "coeffs.ws" 

using TOS;
using Mutable;

zip_bufsize = 1

/*
 This is the beginnings of a statically-allocated version of eugene.ws
*/

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

AddOddAndEven :: (Int, Stream (Array Float), Stream (Array Float)) -> Stream (Array Float);
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
      arr[i] := seg[(i*2)+offset];
    };
    emit arr;
  }
}

fun GetOdd (winsize, strm) GenericGet(1, winsize, strm);
fun GetEven(winsize, strm) GenericGet(0, winsize, strm);


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

    outputBuf = make(bufsize,0);

    iterate buf in strm {
      for j = 0 to buf.length - 1 {
        //inspect$ _memory;

        // add the first element of the input buffer into the array
        FIFO:enqueue(_memory, buf[j]);
        for i = 0 to nCoeff-1 {
	  outputBuf[j] := outputBuf[j] + 
	   _flipped_filter_coeff[i] * FIFO:peek(_memory, i);
        };

	FIFO:dequeue(_memory);
      };
      emit outputBuf;
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
  evenSignal = GetEven(winsize, input);
  oddSignal  = GetOdd(winsize, input);

  // now filter
  highFreqEven = FIRFilter(winsize, hHigh_Even, evenSignal);
  highFreqOdd  = FIRFilter(winsize, hHigh_Odd, oddSignal);

  // now recombine
  AddOddAndEven(winsize / 2, highFreqEven, highFreqOdd);
}

// Filter coefficients
hLow_Even = #[-0.0106, 0.0308, -0.0280, 0.7148];
hLow_Odd  = #[0.0329, -0.1870, 0.6309, 0.2304];

LowFreqFilter :: (Int, Stream (Array Float)) -> Stream (Array Float);
fun LowFreqFilter(winsize, input) {
  evenSignal = GetEven(winsize, input);
  oddSignal  = GetOdd (winsize, input);

  // now filter
  lowFreqEven = FIRFilter(winsize, hLow_Even, evenSignal);
  lowFreqOdd  = FIRFilter(winsize, hLow_Odd,  oddSignal);

  // now recombine them
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
/*   filter_results = GetFeatures(winsize, casted);   */
/*   filter_results */
}
    
namespace Node {
  
  // input winsoze
  winsize = 512;
  NUM_CHANNELS = 1;
  NUM_FEATURES = 3;
  // For running on the PC:
  prefix = "patient36_file16/";
  sensor = smap(toArray, (readFile(prefix++"FP1-F7.txt", "mode: binary", 
				   Server:timer(2.0)) :: Stream Int16).window(winsize));

  // For running on Telos:
  //sensor = read_telos_audio(winsize, 1000) // 1 khz  

  // This is statically allocated, do a big for loop?
  filtered = map(fun(s) process_channel(winsize, s), [sensor]);

/*   filtered = GetFeatures(winsize, hHigh_Odd, cast); */
  flat = FlattenZip(NUM_CHANNELS*NUM_FEATURES, filtered);
  main = List:fold1(merge,filtered)
}

