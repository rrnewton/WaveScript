
// Having some odd problems with fft_bench.ws
// Hacking things a bit to get *something* going.

include "stdlib.ws"
include "fix_fft.ws"

using TOS;
using Array;


// Let's do a 512 pt fft:
levels = 9;
arrsize = 2 ^ levels;

inp     = make(arrsize,11);

imag    = make(arrsize,22);


fixlib = GETENV("REGIMENTD") ++ "/lib/fix_fft.c";
theCcode = inline_C("#include \""++ fixlib ++"\"","");
cfft = (foreign("fix_fft", []) :: (Array FftInt, Array FftInt, Int, Bool) -> Int);



//==============================================================================


namespace Node {
  file = { 
    raw = (readFile("profile.dat", "mode: binary", Server:timer(1000.0)) :: Stream Int16);
    smap(toArray, raw.window(arrsize)); 
  };

  sensor = iterate arr in read_telos_audio(arrsize, 1000) // 512 hz
  {
    // Make it signed int16s:
    for i = 0 to arrsize - 1 {
      inp[i] := (cast_num(arr[i]) :: Int16);
    };
    emit inp;
  };

  src = IFPROFILE(file, sensor);
  
  prep = smap(fun(arr) {
    //for i = 0 to arrsize-1 { imag[i] := 0 };
    (arr,1,2,3)
  } ,src);

  fft1 = iterate (arr,x,y,z) in prep {
    fix_fft(arr, imag, levels, false);  
    emit (arr,x,y);
  };

  fft2 = iterate (arr,x,y) in fft1 {
    //fix_fft(arr, imag, levels, true);
    sum = Mutable:ref(0);
    for i = 1 to 1000 { 
      for j = 1 to 300 {
        sum += 1;
      }
    }
    emit (arr,x);
  };


  fft3 = iterate (arr,x) in fft2 {
    //fix_fft(arr, imag, levels, true);
    sum = Mutable:ref(0);
    for i = 1 to 1000 { 
      for j = 1 to 600 {
        sum += 1;
      }
    }
    emit arr;
  };


  /*
  ffts = 
     sfft $ 
        prep

  ffts = iterate arr in src {
    emit 3;
  }
  */


} // End node


main = iterate _ in Node:fft3  { emit 1; } 
