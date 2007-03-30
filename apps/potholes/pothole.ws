
include "stdlib.ws";
//include "matrix.ws";

//======================================================================



fun fft_filter(s, filter) {

  rw = rewindow(s, Array:length(filter)*2 - 2, 0-(Array:length(filter)-1));

  rw2 = iterate (w in rw) {
    print("item size = " ++ w.width ++ "\n");
    emit(w);
  };

  filt = iterate(h in rw2) {
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

  td = zip2segs(gnuplot_sigseg_stream(myhanning(tdwin)), rw);
  
  iterate((f, orig) in td) {
    state { arr = Array:null }

    if arr == Array:null 
    then arr := Array:make(orig.width / 2, 0.0);

    print("!! arr 0 == " ++ arr[0] ++ " last arr " ++ arr[Array:length(arr) - 1] ++ "\n");
    print("!! seg 0 == " ++ f[[0]] ++ "\n");

    for i = 0 to Array:length(arr) - 1 {
      arr[i] := arr[i] + f[[i]];
print("@@@ " ++ i ++ " " ++ arr[i] ++ "\n");
    };

    print("emitting seg start = " ++ orig.start ++ ", arr 0 = " ++ arr[0] ++ ","
           ++ f[[0]] ++ " last arr " ++ arr[Array:length(arr) - 1] ++ "\n");
    emit(toSigseg(arr, orig.start, orig.timebase));

    for i = 0 to Array:length(arr) - 1 {
      arr[i] := f[[i + Array:length(arr)]];
    }

    print("?? arr 0 == " ++ arr[0] ++ " last arr " ++ arr[Array:length(arr) - 1] ++ "\n");
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

z2 = gnuplot_sigseg_stream(z);

z3 = fft_filter(z2,notch_filter(257,1,5));

BASE <- unionList([gnuplot_sigseg_stream(rewindow(z3, 4096, 0)),gnuplot_sigseg_stream(rewindow(z, 4096, 0))]);



