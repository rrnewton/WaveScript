
include "stdlib.ws";

// inverses overlapped hanning windows
fun hanning_merge(s) {
  iterate w in s {
    state { arr = null }

    if arr == null 
    then arr := make(w.width / 2, 0.0);

    for i = 0 to length(arr) - 1 {
      arr[i] := 2.0 * (arr[i] + w[i]);
    };

    emit(arr);

    for i = 0 to length(arr) - 1 {
      arr[i] := w[i + length(arr)];
    }
  }
}

// Frequency-domain filter operator
fft_filter :: (Stream (Sigseg Float), Array Complex) -> Stream (Sigseg Float);
fun fft_filter(s, filter) {
  using Array;

  rw = rewindow(s, (filter`length - 1) * 2, 0 - (filter`length - 1));

  han = hanning(rw);

  tdfilt = iterate(h in han) {
    freq = fftR2C(toArray(h));
    emit hanning_merge(ifftC2R(build(filter`length, 
                                     fun(i) freq[i] * filter[i])));
  };

  tdfilt2 = zip2_sametype(tdfilt, rw);

  iterate((f, orig) in tdfilt2) {
    emit(toSigseg(arr, orig.start, orig.timebase));
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
