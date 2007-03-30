

include "stdlib.ws";
//include "matrix.ws";

//======================================================================



fun fft_filter(s, filter) {

  rw = rewindow(s, Array:length(filter)*2, Array:length(filter));

  rw2 = iterate (w in rw) {
    print("item size = " ++ w.width ++ "\n");
    emit(w);
  };

  hn = myhanning(rw2);

  filt = iterate(h in hn) {
    print("pre-fft size = " ++ h.width ++ "\n");
    freq = fft(h);
    emit(toSigseg(Array:build(Array:length(filter), fun(i) freq[[i]] * filter[i]), 
                  freq.start, freq.timebase));
    print("filt size = " ++ Array:length(filter) ++ "\n");
  };
 
  tdwin = iterate(f in filt) { 
    print("pre-ifft size = " ++ f.width ++ "\n");
    emit(ifft(f)); 
    print("post-ifft size = " ++ f.width ++ "\n");
  };
  td = zip2segs(tdwin, rw);
  
  iterate((f, orig) in td) {
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
  }
}

fun notch_filter(size, low, high) {
  arr = Array:make(size, 0.0+0.0i);
  for i = low to high {
    arr[i] := gint(1);
  };
  print("filter size = " ++ Array:length(arr) ++ "\n");
  arr
}

// Main query:


chans = (readFile("~/potholes/3-27/drive.txt", "window: 512")
          :: Stream (int16 * int16 * int16));

sm = stream_map;

x = window(sm(fun((a,_,_)) int16ToFloat(a), chans), 512);
z = window(sm(fun((_,a,_)) int16ToFloat(a), chans), 512);
y = window(sm(fun((_,_,a)) int16ToFloat(a), chans), 512);

z2 = gnuplot_sigseg_stream(z);

z3 = fft_filter(z2,notch_filter(256,1,20));

BASE <- gnuplot_sigseg_stream(z3);
