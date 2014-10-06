

include "stdlib.ws"
include "fix_fft.ws"

using TOS;
using Array;


// Let's do a 512 pt fft:
levels = 9;
arrsize = 2 ^ levels;

inp     = make(arrsize,11);

imag    = make(arrsize,22);


fixlib = GETENV("WAVESCRIPTD") ++ "/lib/fix_fft.c";
theCcode = inline_C("#include \""++ fixlib ++"\"","");
cfft = (foreign("fix_fft", []) :: (Array FftInt, Array FftInt, Int, Bool) -> Int);



// The INLINE C CODE CAUSES A VERY STRANGE OPERATOR DUPLICATION:
fun sfft(stm) {
  iterate (arr,img) in stm //.merge(theCcode)
  {
    //cfft(arr, img, levels, false);  
    fix_fft(arr, img, levels, false);    
    emit (arr,img);
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
  
  prep = smap(fun(arr) {
    for i = 0 to arrsize-1 { imag[i] := 0 };
    (arr, imag)
  } ,src);


  fft1 = iterate (arr,img) in prep {
    //fix_fft(arr, img, levels, false);    
    emit (arr,img);
  };

  fft2 = iterate (arr,img) in prep {
    //fix_fft(arr, img, levels, true);
    emit (arr,img);
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


main = iterate _ in Node:fft2  { emit 1; } 
