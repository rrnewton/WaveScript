
DEBUG = false;
DEBUGSYNC = DEBUG;


include "stdlib.ws";
include "matrix.ws";

//======================================================================



fun fft_filter(s, low, high, win) {

  rw = rewindow(s, 512, 256);
  hn = myhanning(rw);
  f = fft(hn);
 
  filt = iterate(freq in f) {
    arr = Array:make(freq.width, 0.0+0.0i);
    for i = 0 to freq.width {
      if (i < low || i > high) then {
        arr[i] := freq[[i]];
      }
    }
    emit(toSigseg(arr, 0, freq.timebase));
  };

  td = zip2segs(ifft(filt), rw);
  
  combine = iterate((f, orig) in td) {
    state {
      arr = Array:null;
    }

    if (arr == Array:null) then {
      arr := Array:make(orig.width / 2, 0.0);
    };

    for i = 0 to Array:length(arr) {
      arr[i] := arr[i] + f[[i]];
    };

    emit(toSigseg(arr, orig.start, orig.timebase));

    for i = 0 to Array:length(arr) {
      arr[i] := f[[i + Array:length(arr)]];
    }
  }; 
}



// Main query:


chans = (readFile("~/potholes/3-27/drive.txt", "window: 512")
          :: Stream (int16 * int16 * int16));

sm = stream_map;

x = window(sm(fun((a,_,_)) int16ToFloat(a), chans), 512);
z = window(sm(fun((_,a,_)) int16ToFloat(a), chans), 512);
y = window(sm(fun((_,_,a)) int16ToFloat(a), chans), 512);

z2 = gnuplot_sigseg_stream(z);
z3 = fft_filter(z2,1,13,512);

BASE <- gnuplot_sigseg_stream(z3);
