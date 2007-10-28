
include "stdlib.ws";

// inverses overlapped hanning windows (sigsegs)
hanning_merge :: (Stream (Sigseg Float)) -> Stream (Sigseg Float);
fun hanning_merge(s) {
  iterate w in s {
    state { arr = Array:null }

    if arr == Array:null 
    then arr := Array:make(w.width / 2, 0.0);

    for i = 0 to arr`Array:length - 1 {
      arr[i] := 2.0 * (arr[i] + w[[i]]);
    };

    emit(toSigseg(arr, w.start, w.timebase));

    for i = 0 to arr`Array:length - 1 {
      arr[i] := w[[i + arr`Array:length]];
    }
  }
}

// Frequency-domain filter operator
fft_filter :: (Stream (Sigseg Float), Array Complex) -> Stream (Sigseg Float);
fun fft_filter(s, filter) {
  using Array;

  rw = rewindow(s, filter`Array:length * 2, 0 - filter`Array:length);

  han = hanning(rw);

  tdfilt = iterate(h in han) {
    harr = toArray(h);
    freq = fftR2C(harr);

    fun multfilt(i) { 
      if (i < filter`Array:length) then {
        freq[i] * filter[i];
      }
      else {
        gint(0); 
      }
    };

    toifft = Array:build(freq`Array:length, multfilt);
    //println("freq len "++freq`Array:length++ " toifft len "++toifft`Array:length);

    tdarr = ifftC2R(toifft);

//println("harr: "++harr);
//println("tdarr: "++tdarr);

    tmp = toSigseg(tdarr, h.start, h.timebase);
    emit tmp;
  };

  hanning_merge(tdfilt)
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

low_pass :: (Int, Int) -> Array Complex;
fun low_pass(size, cutoff) {
  Array:build(size, 
    fun (i) {
      if i <= cutoff 
      then 1.0+0.0i // Can't use gint here because we're in the meta phase!
      else 0.0+0.0i
    })
}

high_pass :: (Int, Int) -> Array Complex;
fun high_pass(size, cutoff) {
  Array:build(size, 
    fun (i) {
      if i >= cutoff 
      then 1.0+0.0i // Can't use gint here because we're in the meta phase!
      else 0.0+0.0i
    })
}


fun psd(s, size) {
  rw = rewindow(s, size*2, 0);
  han = hanning(rw);
  iterate (h in han) {
    freq = fftR2C(toArray(h));
    emit Array:build(size, fun (i) (absC(freq[i])))
  }
}


filter_spikes :: ((Stream (Sigseg t)), Float) -> (Stream (Sigseg t));
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


fun prestream(s, pre_data) {
  iterate (v in s) {
    state {
      done = false;
    }
    if (not(done)) then {
      for i = 0 to Array:length(pre_data)-1 {
	emit(pre_data[i]);
      }
    };
    emit(v);
  }
}


// takes a series of samples
fun gaussian_smoothing(s, points, sigma) {
  rw = rewindow
    (window
     (prestream(s,Array:make
		(points/2,0.0)), points), 
     points, 1-points);

  iterate (w in rw) {
    state {
      hw = gaussian(intToFloat(sigma),points);
    }
    emit(adot(toArray(w),hw));
  }
}
