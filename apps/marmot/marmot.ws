
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



fftArray :: Array Float -> Array Complex;
fun fftArray(arr) {
  to_array(fft(toSigseg(arr, 0, arr.length, nulltimebase)))
}


//FarFieldDOA :: (Stream (List (Sigseg t)), Array (Array Float)) -> Stream (Array (Array t)); 
fun FarFieldDOA(synced, sensors)
{	
  // parameters:
  Ngrd = 360;           // Ngrd is the number of grid points to compute DOA for.
  NSrc = 5;             // maximum number of sources to consider
  Nsel = 30;            // number of frequency bins to consider
  MaxIter = 5;          // maximum number of ot
  if (NSrc == 1) then { MaxIter := 1; };

  Nsens = m_rows(sensors);
  r = makeArray(Nsens, 0.0);
  theta = makeArray(Nsens, 0.0);
  
  /* compute r and theta for each sensor relative to sensor 0 as origin */
  for i = 1 to Nsens-1 {
    r[i] := sqrtF(sqr(m_get(sensors,i,0) - m_get(sensors,0,0)) +
		  sqr(m_get(sensors,i,0) - m_get(sensors,0,1)) +
		  sqr(m_get(sensors,i,2) - m_get(sensors,0,2)));
    theta[i] := atan2(m_get(sensors,i,1) - m_get(sensors,0,1), 
		      m_get(sensors,i,0) - m_get(sensors,0,0));
  };

  // gridmap maps polar grid entry to radians
  gridmap = makeArray(Ngrd, gint(0));
  for i = 0 to Ngrd-1 {
    gridmap[i] := (i * 360.0 / Ngrd) * PI / 180.0; 
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

    // normalize
    fun normalize(row,i) { amult_scalar(row,gint(0)/floatToComplex(norms[i])) };
    nffts = m_rowmap_index(normalize, ffts);

    // sum powers values across channels 
    power = makeArray((m_in[0]).length(), gint(0));
    for i = 0 to m_in.length-1 {
      for j = 0 to (m_in[i]).length-1 {
        power[j] := power[j] + m_get(m_in,i,j);
      }
    };

    // create index vector
    power_index = makeArray((m_in[0]).length, 0);
    for i = 0 to power_index.length-1 { power_index[i] := i; };

    // sort index vector and power vector
    fun swap(i,j) { 
      tmp1 = power_index[i]; 
      power_index[i] := power_index[j];
      power_index[j] := tmp1; 
      tmp2 = power[i]; 
      power[i] := power[j];
      power[j] := tmp1; 
    };
    fun cmp(i,j) { (power[i] - power[j]) };
    sort(swap,cmp,power.length);

    // length of the fft
    Ndat = power.width;  

    // This will be the output DOA likelihoods
    Jmet = matrix(NSrc,Ngrd,0.0);

    // T is the maximum direction grid value
    T = a_ones(NSrc);
    Tbefore = a_zeroes(NSrc);
    Trad = gint(0);

    // phase delays for each source
    delay = matrix(Nsens, NSrc, 0.0);

    // steering matrix
    D = matrix(NSrc, Nsens, 0.0);

    fun doDelay(P, grid) {
      0.0 - r[P] * cos(gridmap[grid] - theta[P])
    };

    // 5 iterations
    for iter = 1 to MaxIter {
      for Q = 0 to NSrc-1 {
	
	J = makeArray(Ngrd, 0.0);
	
	// compute delay for other sources
	for L = 0 to NSrc-1 {
	  if L != Q then {
	    for P = 1 to Nsens-1 {
	      m_set(delay,P,L,doDelay(P, T[L]));
	    }
	  }
	};
	
	for I = 0 to Ngrd-1 {
	  
	  // compute delay for this direction
	  for P = 1 to Nsens-1 {
	    m_set(delay,P,Q,doDelay(P,I));
	  };

	  // Sum over highest power frequecies
	  for K = 0 to Nsel-1 {

	    // calculate the steering matrix
	    for L = 0 to NSrc-1 {
	      if (iter > 1 || L <= Q) then {
		m_set(D,0,L,1.0);
		for P = 1 to Nsens-1 {
		  m_set(D,P,L, floatToComplex(E)^
                   (0.0+1.0i * floatToComplex(-2.0 * PI *
		   gint(power_index[K]+1) * m_get(delay,P,L) / gint(Ndat))));
		}
	      }
	    };

	    // get col vector from matrix, except its an array of sigseg :(
	    X = m_col(nffts,power_index[K]);

	    Post = m_mult(D, m_inv(D));
	    J[I] := J[I] + realpart(m_mult(m_trans(X), m_mult(Post, X)));
	  }
	};

	let (maxJ, maxJind) = a_max(J);
	T[Q] := maxJind;
        Trad[Q] := (T[Q]-1.0)*2.0*PI/gint(Ngrd);
        
	for K = 0 to m_cols(Jmet) {
          m_set(Jmet,Q,K,(J[K] / maxJ));
	}
      };

      // stopping condition, break out of loop?
      diffs = makeArray(Trad.length, gint(0));
      for K = 0 to Trad.length { diffs[K] := absF(Tbefore[K]-Trad[K]) };
      let (max_change,_) = a_max(diffs);

      //or, in matlab... that would just be 'max(abs(Tbefore-Trad))'!!!
      
      if (max_change < 0.001) then {
        break;
      };

      for i = 0 to Tbefore.length - 1 {
	Tbefore[i] := gridmap[T[i]] 
      }
    };
 
    emit(Jmet);

  };
  result

}

sm = stream_map;

//========================================
// Main query:

flag = GETENV("WSARCH") == "ENSBox";
//flag = true;
//marmotfile = "/archive/4/marmots/brief.raw";
//marmotfile = "/archive/4/marmots/real_100.raw";
marmotfile = 
  if FILE_EXISTS("15min_marmot_sample.raw") then "15min_marmot_sample.raw" else
  if FILE_EXISTS("3min_marmot_sample.raw") then "3min_marmot_sample.raw" else
  if FILE_EXISTS("6sec_marmot_sample.raw") then "6sec_marmot_sample.raw" else
  if FILE_EXISTS("~/archive/4/marmots/brief.raw") then "~/archive/4/marmots/brief.raw" else
  wserror("Couldn't find sample marmot data, run the download scripts to get some.\n");

chans = (dataFile(marmotfile, "binary", 24000, 0)
	 :: Stream (Int * Int * Int * Int));

ch1 = if flag then ENSBoxAudio(0,4096,0,24000) else window(sm(fun((a,_,_,_)) intToFloat(a), chans), 4096);
ch2 = if flag then ENSBoxAudio(1,4096,0,24000) else window(sm(fun((_,b,_,_)) intToFloat(b), chans), 4096);
ch3 = if flag then ENSBoxAudio(2,4096,0,24000) else window(sm(fun((_,_,c,_)) intToFloat(c), chans), 4096);
ch4 = if flag then ENSBoxAudio(3,4096,0,24000) else window(sm(fun((_,_,_,d)) intToFloat(d), chans), 4096);

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
//BASE <- unionList([window(sm(fun((a,_,_,_)) intToFloat(a), chans), 1),
//		   audio(0,1,0,44000)]);
