
include "stdlib.ws";

// Frequency-domain filter operator
fun fft_filter(s, filter) {

  rw = rewindow(s, (Array:length(filter)-1)*2, 0-(Array:length(filter)-1));

  filt = iterate(h in rw) {
    freq = fftR2C(toArray(h));
    emit(Array:build(Array:length(filter), fun(i) freq[i] * filter[i]));
  };

  tdwin = iterate f in filt { 
    emit(toSigseg(ifftC2R(f),0,nulltimebase)); 
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



fun filter_spikes(s, thresh) {
  iterate (v in s) {
    state {
      last = 0.0;
    }  

    arr = Array:build(v.width, fun (i) v[[i]]);

    for i = 0 to v.width-1 {
      if ((arr[i] > thresh) || (arr[i] < 0.0-thresh)) then {
        arr[i] := last; 
      };
      last := arr[i];
    }

    emit(toSigseg(arr, v.start, v.timebase));
  }
}

