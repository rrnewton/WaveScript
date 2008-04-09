include "stdlib.ws"
include "fifostatic.ws"
//include "coeffs.ws" 

using TOS;

/*
 This is the beginnings of a statically-allocated version of eugene.ws
*/

/* fun GenericGet(offset, strm) { */
/*   using Array; */
/*   buf = make(winsize, 0); */
/*   for j = 0 to buf.length - 1 { */
    
/*   } */
/*   emit buf; */

/* } */


AddOddAndEven :: (Stream (Array Float), Stream (Array Float)) -> Stream (Array Float);
fun AddOddAndEven(s1,s2) {
  using Array;
  
  _stored_value = 0;
  //    assert_eq("AddOddAndEven", first.width, second.width);
  buf = make(winsize, 0);
  iterate arr in zipN(zip_bufsize, [s1,s2]) {    
    first = arr[0];
    second = arr[1];
    for i = 0 to first.width - 1 {
      buf[i] := first[[i]] + _stored_value;
      _stored_value := second[[i]]; // we don't add the last odd guy, but store
    };
    emit buf;
  }

// implementation of an FIR filter using convolution 
// you have to provide an array of coefficients 
FIRFilter :: (Array Float, Stream (Array Float)) -> Stream (Array Float);
fun FIRFilter(filter_coeff, strm) {
    using Array;
    nCoeff = filter_coeff.length;
    
    _flipped_filter_coeff =  // array of _filter_coefficients
      build(nCoeff, fun(i) filter_coeff[nCoeff-1-i]);

    // remembers the previous points needed for convolution
    _memory = FIFO:make(nCoeff);
    for i = 1 to nCoeff-1 { FIFO:enqueue(_memory, 0.0) };

    outputBuf = make(winsize,0);

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


hHigh_Odd  = #[0.7148, -0.0280, 0.0308, -0.0106 ]

namespace Node {
  
  // input winsoze
  winsize = 512;

  // For running on the PC:
  prefix = "patient36_file16/";
  sensor = smap(toArray, (readFile(prefix++"FP1-F7.txt", "mode: binary", Server:timer(2.0)) :: Stream Int16).window(winsize))

  // For running on Telos:
  //sensor = read_telos_audio(winsize, 1000) // 1 khz  

  // Working in floating point atm:
  // This is statically allocated:

  floats = Array:make(winsize, 0.0);

  cast = iterate arr in sensor {
    for i = 0 to arr.Array:length - 1 {
      floats[i] := (cast_num(arr[i]) :: Float);
    };
    emit floats;
  }

  filtered = FIRFilter(hHigh_Odd, cast);
}

main = Node:filtered;

