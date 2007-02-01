
DEBUG = false;
DEBUGSYNC = DEBUG;


include "stdlib.ws";
include "matrix.ws";

//======================================================================

// Takes Sigseg Complex
fun marmotscore2(freqs) { 
  result = 
    absC(freqs[[3]] +: 
	 freqs[[4]]);
  if DEBUG then 
   print("\nMarmot Score: "++show(result)++", \nBased on values "
	++ show(freqs[[3]]) ++ " "
	++ show(freqs[[4]]) ++ " \n");
  result
}


/* expects Zip2<SigSeg<float>,float>::Output */
fun detect(scorestrm) {
  // Constants:
  alpha = 0.999;
  hi_thresh = 16;
  startup_init = 300;
  refract_interval = 40;
  max_run_length = 48000;
  samples_padding = 2400;

  iterate((score,win) in scorestrm) {
    state {
      thresh_value = 0.0;
      trigger = false;
      smoothed_mean = 0.0;
      smoothed_var = 0.0;
      _start = 0; 
      trigger_value = 0.0;
      startup = 300;
      refract = 0;                 

      // private
      noise_lock = 0; // stats
    }

    fun reset() {
      thresh_value := 0.0;
      trigger := false;
      smoothed_mean := 0.0;
      smoothed_var := 0.0;
      _start := 0;
      trigger_value := 0.0;
      startup := startup_init;
      refract := 0;
    };

    if DEBUG then 
    print("Detector state: thresh_value " ++show(thresh_value)++ " trigger " ++show(trigger)++ 
	  " smoothed_mean " ++show(smoothed_mean)++ " smoothed_var " ++show(smoothed_var)++ "\n" ++
	  "        _start " ++show(_start)++ " trigger_value " ++show(trigger_value)++ 
	  " startup " ++show(startup)++ " refract " ++show(refract)++ " noise_lock " ++show(noise_lock)++"\n"
	  );
    
    
    /* if we are triggering.. */
    if trigger then {      

      /* check for 'noise lock' */
      if win.end - _start > max_run_length then {
	print("Detection length exceeded maximum of " ++ show(max_run_length)
	      ++", re-estimating noise");
	
	noise_lock := noise_lock + 1;
	reset();
	//goto done; GOTO GOTO GOTO 
      };

      /* over thresh.. set refractory */
      if score > thresh_value then {
	refract := refract_interval;
      } else if refract > 0 then {	
	/* refractory counting down */
	refract := refract - 1;
      }	else {
	/* untriggering! */
	trigger := false;
	emit (true,                       // yes, snapshot
	      _start - samples_padding,     // start sample
	      win.end + samples_padding); // end sample
	if DEBUG then
	print("KEEP message: "++show((true, _start - samples_padding, win.end + samples_padding))++
	      " just processed window "++show(win.start)++":"++show(win.end)++"\n");

	// ADD TIME! // Time(casted->_first.getTimebase()
	_start := 0;
      }
    } else { /* if we are not triggering... */      
      /* compute thresh */
      let thresh = intToFloat(hi_thresh) *. sqrtF(smoothed_var) +. smoothed_mean;

      if DEBUG then 
        print("Thresh to beat: "++show(thresh)++ ", Current Score: "++show(score)++"\n");

      /* over thresh and not in startup period (noise est period) */
      if startup == 0 && score > thresh then {
	if DEBUG then print("Switching trigger to ON state.\n");
	trigger := true;
	refract := refract_interval;
	thresh_value := thresh;
	_start := win.start;
	trigger_value := score;
      }	else {
	/* otherwise, update the smoothing filters */
	smoothed_mean := score *. (1.0 -. alpha) +. smoothed_mean *. alpha;
	delt = score -. smoothed_mean;
	smoothed_var := (delt *. delt) *. (1.0 -. alpha) +. smoothed_var *. alpha;
      };
	
      /* count down the startup phase */
      if startup > 0 then startup := startup - 1;
      
      /* ok, we can free from sync */
      /* rrn: here we lamely clear from the beginning of time. */
      /* but this seems to assume that the sample numbers start at zero?? */
      emit (false, 0, max(0, win.end - samples_padding));
      if DEBUG then 
      print("DISCARD message: "++show((false, 0, max(0, win.end - samples_padding)))++
	    " just processed window "++show(win.start)++":"++show(win.end)++"\n");
      
    }
  }
}


/*

fun ssmap(f) {
  fun (seg) {
    toSigseg((amap(f))(to_array(seg)), seg.start, seg.end, seg.timebase)
  }
}

fun ssfold(f) {
  fun (seg) {
    x = seg[[0]];
    for i = 1 to seg.width - 1 {
      x := f(x,seg[[i]]);
    };
    x
  }
}

fun afold(f) {
  fun (arr) {
    x = arr[0];
    for i = 1 to arr.length - 1 {
      x := f(x,arr[i]);
    };
    x
  }
}

fun ssmap2(f) {
  fun (seg1, seg2) {
    a = makeArray(seg1.width, seg1[[0]]);
    for i = 0 to seg1.width - 1 {
      a[i] := f(seg1[[i]], seg2[[i]]);
    };
    toSigseg(a, seg1.start, seg1.end, seg1.timebase)
  }
}

fun zeroesf(size) {
  makeArray(size, 0.0)
}

// this version returns an array
fun onesf(size) {
  makeArray(size, 1.0);
}

fun matrix(rows, cols, init) {
  a = makeArray(rows, makeArray(cols, init));
  for i = 1 to rows {
    a[i] := makeArray(cols, init);
  };
  a
}

fun mset(matrix, row, col, val) {
  row = matrix[row];
  row[col] := val;
  val
}

fun mget(matrix, row, col) {
  row = matrix[row];
  row[col]
}

fun sortseg(seg) {
  // really want a struct here!! value and index
  ival = matrix(seg.width, 2, 0.0);
  for i = 0 to seg.width - 1 {
    mset(ival, i, 0, seg[[i]]);
    mset(ival, i, 1, i);
  }
  qsort(ival, (fun (i,j) {mget(ival,i,0) < mget(ival,j,0)}));
  toSigseg(ival, seg.start, seg.end, seg.timebase);
}

fun Vmax(v) {
  max = 0.0;
  ind = (0-1);
  for i = 0 to v.length {
    if (ind < 0 || max < v[i]) then {
      ind := i;
      max := v[i];
    }
  };
  (max,ind)
}

fun MRowEval(m,row,f) {
  for i = 0 to row-1 {
    mset(m,Q,i,f(i));
  }
}

fun array_iterate_index(a,f) {
  new_a = makeArray(a.length, a[0]);
  for i = 0 to a.length-1 {
    new_a[i] := f(a[i],i);
  };
  new_a
}

*/

// NEED:
// qsort
// ComplexI
// expc
// atan2
// MInv


fftArray :: Array Float -> Array Complex;
fun fftArray(arr) {
  to_array(fft(toSigseg(arr, 0, arr.length, nulltimebase)))
}


FarFieldDOA :: (Stream (List (Sigseg t)), Array (Array Float)) -> Stream (Array (Array t)); 
fun FarFieldDOA(synced, sensors)
{	
  Nsens = m_rows(sensors);
  r = makeArray(Nsens, 0.0);
  theta = makeArray(Nsens, 0.0);
  
  /* compute r and theta for each sensor relative to sensor 0 as origin */
  for i = 1 to Nsens {
    r[i] := sqrtF(sqr(m_get(sensors,i,0) - m_get(sensors,0,0)) +
		  sqr(m_get(sensors,i,0) - m_get(sensors,0,1)) +
		  sqr(m_get(sensors,i,2) - m_get(sensors,0,2)));
    theta[i] := atan2(m_get(sensors,i,1) - m_get(sensors,0,1), 
		      m_get(sensors,i,0) - m_get(sensors,0,0));
  };

  // convert the output of sync to a matrix.. yes this kind of sucks 
  matrix_in = stream_map(list_of_segs_to_matrix, synced);

  // ok, i guess we do one big iterate.. 
  result = iterate (m_in in matrix_in) {
    //fft the sync'd data 
    ffts = m_rowmap(fftArray, m_in);

    // compute psds
    psds = m_map(absC, ffts);

    // compute norms
    norms = m_rowmap_scalar(asum, psds);
     emit norms;

/*
    // normalize
    nffts = ffts;
    for i = 0 to nffts.length - 1 {
      for j = 0 to nffts[i].length - 1 {
        nffts[i]
amap_inplace ((f,n) in (zip2(ffts,norms))) {
    emit((ssmap2((/:)))(f,n))
*/

  };
  result
/*


  // compute psds
  psds = smap(amap(ssmap(cnorm)), ffts);

  // compute norms
  norms = smap(amap(ssfold((+.))), psds);
  
  // normalize
  nffts = iterate ((f,n) in (zip2(ffts,norms))) {
    emit((ssmap2((/:)))(f,n))
  };
  
  // sum powers 
  power = smap(afold(ssmap2((+.))), psds);

  // sort
  sorted = smap(sortseg, power);

  // doa
  doa = iterate ((f,sel) in (zip2(nffts, sorted))) {

    Ngrd = 360;
    NSrc = 5;
    Ndat = sorted.width;
    MaxIter = 5;
    
    if (NSrc == 1) then { MaxIter := 1; };

    // gridmap maps polar grid entry to radians
    gridmap = makeArray(Ngrd, 0);
    for i = 0 to Ngrd-1 {
      gridmap[i] := (i * 360.0 /. Ngrd) * M_PI / 180.0; 
    };

    Jmet = matrix(NSrc,Ngrd,0.0);

    // T is the maximum direction grid value
    T = onesf(NSrc);
    Tbefore = zeroesf(NSrc);

    // phase delays for each source
    delay = matrix(Nsens, NSrc, 0.0);

    // steering matrix
    D = matrix(NSrc, N_SENSORS, 0.0);
    
    fun doDelay(P, grid) {
      - r[P] * cos(gridmap[grid] - theta[P])
    };

    // 5 iterations
    for iter = 1 to MaxIter {
      for Q = 1 to NSrc {
	
	J = makeArray(Ngrd, 0.0);
	
	// compute delay for other sources
	for L = 1 to NSrc {
	  if L != Q then {
	    for P = 1 to Nsens-1 {
	      mset(delay,P,L,doDelay(P, T[L]));
	    }
	  }
	};
	
	for I = 0 to Ngrd-1 {
	  
	  // compute delay for this direction
	  for P = 1 to Nsens-1 {
	    mset(delay,P,Q,doDelay(P,I));
	  };

	  // Sum over highest power frequecies
	  for K = 0 to Nsel-1 {

	    // calculate the steering matrix
	    for L = 0 to NSrc-1 {
	      if (iter > 1 || L <= Q) then {
		mset(D,0,L,1.0);
		for P = 1 to Nsens-1 {
		  tmp = sel[[K]];
		  mset(D,P,L, expc(-ComplexI *: 2 *: M_PI *:
				   tmp[1] *: mget(delay,P,L) /: Ndat));
		}
	      }
	    };

	    // get col vector from matrix, except its an array of sigseg :(
	    X = (amap_nd(fun(elt){tmp = sel[[K]]; elt[[tmp[1]]]}), f);

	    Post = MMult(D, MInv(D));
	    J[I] := J[I] + real(MMult(MTrans(X), MMult(P, X)));
	  }
	};

	let (maxJ, maxJind) = Vmax(J);
	T[Q] := maxJind;
        MRowEval(Jmet,Q,(fun(i)(J[i] /. maxJ)));
      };

      // stopping condition, break out of loop?
      max_change = (afold(max), array_iterate_index(T, (fun(val,i){abs(Tbefore[i] -. gridmap[val])})));
      //or, in matlab... max(abs(Tbefore-Trad)) 
      
      if (max_change < 0.001) then {
        break;
      };

      for i = 0 to Tbefore.length - 1 {
	Tbefore[i] := gridmap[T[i]] 
      }
    }
  };

  doa  
*/
}



//========================================
// Main query:

ch1 = audio(0, 4096, 0);
ch2 = audio(1, 4096, 0);
ch3 = audio(2, 4096, 0);
ch4 = audio(3, 4096, 0);

// 96 samples are ignored between each 32 used:
rw1 = rewindow(ch1, 32, 96); 

//hn = smap(hanning, rw1);
hn = myhanning(rw1);

freq = stream_map(fft, hn);
wscores = iterate (w in freq) { emit(marmotscore2(w), w); }

detections = detect(wscores);

d2 = iterate (d in detections) { 
  let (flag,_,_) = d;
  if flag then print("detected at "++show(d)++"\n"); 
  emit d; 
};

synced = syncN(d2, [ch1, ch2, ch3, ch4]);

/* define array geometry */
sensors = list_to_matrix([[ 0.4,-0.4,-0.4],
			  [ 0.4, 0.4, 0.4],
			  [-0.4, 0.4,-0.4],
			  [-0.4,-0.4, 0.4]]);

doas = FarFieldDOA(synced, sensors);

BASE <- doas;
