
DEBUG = false
DEBUGSYNC = DEBUG 

include "stdlib.ws"


//======================================================================

// Takes Sigseg Complex
fun marmotscore(freqs) { 
  st = freqs.start;
  result = 
    cnorm(freqs[[st + 4]] +: 
	  freqs[[st + 5]] +:
	  freqs[[st + 6]] +:
	  freqs[[st + 7]]);
  if DEBUG then 
   print("\nMarmot Score: "++show(result)++", \nBased on values "
	++ show(freqs[[st + 4]]) ++ " "
	++ show(freqs[[st + 5]]) ++ " "
	++ show(freqs[[st + 6]]) ++ " "
	++ show(freqs[[st + 7]]) ++ " \n");
  result
}

/* expects Zip2<SigSeg<float>,float>::Output */
fun detect(scorestrm) {
  // Constants:
  alpha = 0.999;
  hi_thresh = 8;
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
      _start = 0; //start = ??? // SeqNo
      trigger_value = 0.0;
      startup = 300;
      refract = 0;                 

      // private
      noise_lock = 0; // stats
    }

    fun reset() {
      thresh_value := 0;
      trigger := false;
      smoothed_mean := 0.0; // 0; // TYPE INFERENC ERROR -- CHECK SET! CASE FIXME!!!!
      smoothed_var := 0.0;
      _start := 0;
      trigger_value := 0;
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
	trigger := 0;
	emit (true,                       // yes, snapshot
	      _start - samples_padding,     // start sample
	      win.end + samples_padding); // end sample
	// ADD TIME! // Time(casted->_first.getTimebase()
	_start := 0;
      }
    } else { /* if we are not triggering... */      
      /* compute thresh */
      let thresh = int_to_float(hi_thresh) *. sqrtf(smoothed_var) +. smoothed_mean;

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
      emit(false, 0, max(0, win.end - samples_padding));
    }
  }
}


fun amap(f) {
  fun (arr) {
    for i = 0 to arr.length - 1 {
      arr[i] := f(arr[i]);
    }
  }
}

fun amap_nd(f) {
  fun (arr) {
    narr = makeArray(arr.length, arr[0]);
    for i = 0 to arr.length - 1 {
      narr[i] := f(arr[i]);
    };
    narr
  }
}

fun ssmap(f) {
  fun (seg) {
    to_sigseg((amap(f))(to_array(seg)), seg.start, seg.end, seg.timebase)
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
    to_sigseg(a, seg1.start, seg1.end, seg1.timebase)
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
  to_sigseg(ival, seg.start, seg.end, seg.timebase);
}

fun sqrf(x) { x *. x; }

fun Vmax(v) {
  max = 0.0;
  ind = -1;
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


fun MMult(m1,m2) {
  m3 = matrix(m1.length, m2[0].length, mget(m1,0,0));
  for i = 0 to m1.length-1 {
    for j = 0 to m2[0].length-1 {
      // need to know type :( .. what if not float?
      sum = 0.0;
      for k = 0 to m2.length-1 {
	sum := sum +. (mget(m1,i,k) *. mget(m2,k,j));
      };
      mset(m3,i,j,sum)
    }
  };
  m3
}


fun MTrans(m) {
  m2 = matrix(m[0].length, m.length, mget(m,0,0));
  for i = 0 to m.length {
    for j = 0 to m[0].length {
      mset(m2,j,i,mget(m,i,j))
    }
  };
  m2
}


// NEED:
// qsort
// ComplexI
// expc
// atan2
// MInv

fun FarFieldDOA(synced) 
{	
  Nsens = 4;

  sensors = matrix(Nsens, 3, 0.0);
  r = makeArray(Nsens, 0.0);
  theta = makeArray(Nsens, 0.0);
  
  mset(sensors, 0, 0,  0.4);
  mset(sensors, 0, 1, -0.4);
  mset(sensors, 0, 2, -0.4);

  mset(sensors, 1, 0,  0.4);
  mset(sensors, 1, 1,  0.4);
  mset(sensors, 1, 2,  0.4);

  mset(sensors, 2, 0, -0.4);
  mset(sensors, 2, 1,  0.4);
  mset(sensors, 2, 2, -0.4);

  mset(sensors, 3, 0, -0.4);
  mset(sensors, 3, 1, -0.4);
  mset(sensors, 3, 2,  0.4);

  /* compute r and theta for each sensor relative to sensor 0 as origin */
  for i = 1 to 3 {
    r[i] := sqrtf(sqrf(mget(sensors,i,0) -. mget(sensors,0,0)) +.
		  sqrf(mget(sensors,i,0) -. mget(sensors,0,1)) +.
		  sqrf(mget(sensors,i,2) -. mget(sensors,0,2)));
    theta[i] := atan2(mget(sensors,i,1) -. mget(sensors,0,1), mget(sensors,i,0) -. mget(sensors,0,0));
  };

  // fft the sync'd 
  ffts = smap(amap(fft), synced);

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

freq = smap(fft, hn);

//wscores = smap(fun(w){(marmotscore(w), w)}, freq);
wscores = iterate (w in freq) { emit(marmotscore(w), w); }

detections = detect(wscores);

synced = syncN(detections, [ch1, ch2, ch3, ch4]);

doas = FarFieldDOA(synced);

BASE <- synced;
