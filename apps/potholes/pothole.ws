
include "stdlib.ws";
//include "matrix.ws";

//======================================================================



fun fft_filter(s, filter) {

  rw = rewindow(s, Array:length(filter)*2 - 2, Array:length(filter));

  rw2 = iterate (w in rw) {
    print("item size = " ++ w.width ++ "\n");
    emit(w);
  };

  hn = myhanning(rw2);


  filt = iterate(h in hn) {
    println("size before fft " ++ h`width);
    freq = fft(h);
    println(" after " ++ freq`width);
    emit(toSigseg(Array:build(Array:length(filter), fun(i) freq[[i]] * filter[i]), 
                  freq.start, freq.timebase));
    print("filt size = " ++ Array:length(filter) ++ "\n");
  };

  tdwin = iterate f in filt { 
    println("size before ifft " ++ f`width);
    println("  after " ++ f`ifft`width);
    emit(ifft(f)); 
  };

  td = zip2segs(tdwin, rw);
  
  iterate((f, orig) in td) {
    state { arr = Array:null }

    if arr == Array:null 
    then arr := Array:make(orig.width / 2, 0.0);
    for i = 0 to Array:length(arr) - 1 {
      arr[i] := arr[i] + f[[i]];
    };

    emit(toSigseg(arr, orig.start, orig.timebase));

    for i = 0 to Array:length(arr) - 1 {
      arr[i] := f[[i + Array:length(arr)]];
    }
  }
}

notch_filter :: (Int, Int, Int) -> Array Complex;
fun notch_filter(size, low, high) {
  Array:build(size, 
    fun (i) {
      if i >= low && i <= high 
      then 1.0+0.0i // Can't use gint here because we're in the meta phase!
      else 0.0+0.0i
    })
}


sm = stream_map;

// ============================================================
// Main query:

chans = (readFile("~/potholes/3-27/drive.txt", "")
          :: Stream (int16 * int16 * int16));

x = window(sm(fun((a,_,_)) int16ToFloat(a), chans), 512);
z = window(sm(fun((_,a,_)) int16ToFloat(a), chans), 512);
y = window(sm(fun((_,_,a)) int16ToFloat(a), chans), 512);

//z2 = gnuplot_sigseg_stream(z);

z3 = fft_filter(z,notch_filter(257,1,20));

//z3 = fft_filter(z,Array:make(257, 0.0+0.0i));

//BASE <- gnuplot_sigseg_stream(z3);
//BASE <- CONST(notch_filter(257,1,20));
BASE <- z3;


