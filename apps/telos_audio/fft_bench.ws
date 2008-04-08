

include "stdlib.ws"
include "fix_fft.ws"

using TOS;
using Array;


// Let's do a 512 pt fft:
levels = 9;
arrsize = 2 ^ levels;

inp     = make(arrsize,11);
imag    = make(arrsize,22);

fun sfft(stm) {
  iterate arr in stm {
    for i = 0 to arrsize-1 { imag[i] := 0 };
    fix_fft(arr, imag, levels, false);
    emit arr;
  }
}

//==============================================================================



namespace Node {
  file = { 
    raw = (readFile("profile.dat", "mode: binary", Server:timer(2.0)) :: Stream Int16);
    smap(toArray, raw.window(arrsize)); 
  };

  sensor = iterate arr in read_telos_audio(arrsize, arrsize.gint) // 512 hz
  {
    // Make it signed int16s:
    for i = 0 to arrsize - 1 {
      inp[i] := (cast_num(arr[i]) :: Int16);
    };
    emit inp;
  };

  src = IFPROFILE(file, sensor);

  ffts = 
     sfft $ 
        src

} // End node

main = iterate _ in Node:ffts {
  emit 1;
}

