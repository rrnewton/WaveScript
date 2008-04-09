include "stdlib.ws"
include "fifostatic.ws"

using TOS;

/*
 This is the beginnings of a statically-allocated version of eugene.ws
*/


winsize = 512

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
	// 		print ("memory: ");   
        for i = 0 to nCoeff-1 {
	  outputBuf[j] := outputBuf[j] + 
	   _flipped_filter_coeff[i] * FIFO:peek(_memory, i);
	  //	  	  print (i++": "++myRound(FIFO:peek(_memory,i))++", ");  
        };
	// 	println("");  
	// 	println("output: "++myRound(outputBuf[j]));  

	FIFO:dequeue(_memory);
      };
      // 	println("END"); 
	
      emit outputBuf;
    }
}


hHigh_Odd  = #[0.7148, -0.0280, 0.0308, -0.0106 ]

namespace Node {

  // For running on the PC:
  sensor = smap(toArray, (readFile("profile.dat", "mode: binary", Server:timer(2.0)) :: Stream Int16).window(winsize))
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

