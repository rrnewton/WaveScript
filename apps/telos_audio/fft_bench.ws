

include "stdlib.ws"
include "fix_fft.ws"

// Hmm... what is the meaning of the first argument?
levels = 4;
arrsize = 2 ^ levels;

fun sfft(src) {
  using Array;
  inp     = make(arrsize,11);
  imag    = make(arrsize,0);
  for i = 0 to arrsize-1 { imag[i] := 0 };
  cast = iterate arr in src {
    // Make it signed int16s:
    for i = 0 to arrsize - 1 {
      inp[i] := (cast_num(arr[i]) :: Int16);
    };
    emit inp;
  };
  iterate arr in cast {
    fix_fft(arr, imag, levels, false);
    emit ();
  }
}

//==============================================================================

using TOS;

namespace Node {
  src = read_telos_audio(arrsize, 1000); // 1khz
  ffts = sfft $ src
} // End node

main = Node:ffts

