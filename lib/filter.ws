
include "stdlib.ws";

// Frequency-domain filter operator
fun fft_filter(s, filter) {

  rw = rewindow(s, (Array:length(filter)-1)*2, 0-(Array:length(filter)-1));

  filt = iterate(h in rw) {
    freq = fft(h);
    emit(toSigseg(Array:build(Array:length(filter), fun(i) freq[[i]] * filter[i]), 
                  freq.start, freq.timebase));
  };

  tdwin = iterate f in filt { 
    emit(ifft(f)); 
  };

  td = zip2_sametype(gnuplot_sigseg_stream(myhanning(tdwin)), rw);
  
  iterate((f, orig) in td) {
    state { arr = Array:null }

    if arr == Array:null 
    then arr := Array:make(orig.width / 2, 0.0);

    for i = 0 to Array:length(arr) - 1 {
      arr[i] := 2.0 * (arr[i] + f[[i]]);
print("@@@ " ++ i ++ " " ++ arr[i] ++ "\n");
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

