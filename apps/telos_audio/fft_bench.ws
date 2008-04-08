

include "stdlib.ws"
include "fix_fft.ws"

using TOS;
using Array;


// Let's do a 512 pt fft:
levels = 9;
arrsize = 2 ^ levels;

inp     = make(arrsize,11);
imag    = make(arrsize,0);

fun sfft(src) {
  iterate arr in src {
    for i = 0 to arrsize-1 { imag[i] := 0 };
    fix_fft(arr, imag, levels, false);
    emit arr;
  }
}

//==============================================================================

namespace Node {

  src = iterate arr in read_telos_audio(arrsize, arrsize.gint) // 512 hz
  {
    // Make it signed int16s:
    for i = 0 to arrsize - 1 {
      inp[i] := (cast_num(arr[i]) :: Int16);
    };
    emit arr;
  };
  /*
  ffts = 
     sfft $ 
     sfft $ 
     sfft $ 
        src
  */
} // End node

//main = Node:ffts

