

/*


[2007.11.14] Some numbers from faith:  For 10 tuples.

 OrigHandopt: 1s
 LewisVer:    14.9 / 13.6
 RyanVEr:     10.65
 RyanWithOpt: 3.3s

*/


DEBUG = false;
DEBUGSYNC = DEBUG;

include "stdlib.ws";
include "filter.ws";

// Trying this fft:
//fix_fft :: (Array Int16 * Array Int16 * Int16 * Int16)
//         = foreign("fix_fft", ["fix_fft.c"]);

//marmfft = fix_fft
marmfft = memoized_fftR2C
fun sigseg_marmfft  (ss) toSigseg(ss `toArray `marmfft,    ss.start, ss.timebase)

// Takes Sigseg Complex
fun marmotscore2(freqs) { 
  result = 
    absC(freqs[[3]] +: 
	 freqs[[4]]);
  if DEBUG then 
   log(1 ,"\nMarmot Score: "++show(result)++", \nBased on values "
	 ++ show(freqs[[3]]) ++ " "
	 ++ show(freqs[[4]]) ++ " \n");
  result
}

//detect :: Stream (Float * Sigseg any) -> Stream (Bool * Int * Int64);
//detect :: Stream (Float * Sigseg any) -> Stream (Bool * Int64 * Int64);
//detect :: Stream (Float * Sigseg 'any) -> Stream (Bool * #erk * Int64);
detect :: Stream (Float * Int64 * Int64) -> Stream (Bool * Int64 * Int64);
fun detect(scorestrm) {
  // Constants:
  alpha = 0.999;
  hi_thresh = 16;
  startup_init = 300;
  refract_interval = 40;
  max_run_length = 48000;
  samples_padding = 2400`to64;
  
  iterate((score,st,en) in scorestrm) {
    state {
      thresh_value = 0.0;
      trigger = false;
      smoothed_mean = 0.0;
      smoothed_var = 0.0;
      _start = 0`to64; 
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
      _start := 0`to64;
      trigger_value := 0.0;
      startup := startup_init;
      refract := 0;
    };

    if DEBUG then 
    log(1,"Detector state: thresh_value " ++show(thresh_value)++ " trigger " ++show(trigger)++ 
	  " smoothed_mean " ++show(smoothed_mean)++ " smoothed_var " ++show(smoothed_var)++ "\n" ++
	  "        _start " ++show(_start)++ " trigger_value " ++show(trigger_value)++ 
	  " startup " ++show(startup)++ " refract " ++show(refract)++ " noise_lock " ++show(noise_lock)++"\n"
	  );
    
    
    /* if we are triggering.. */
    if trigger then {      

      /* check for 'noise lock' */
      if int64ToInt(en - _start) > max_run_length then {
	log(1, "Detection length exceeded maximum of " ++ show(max_run_length)
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
	
	/* emit power of 2 */
	p = en + samples_padding - _start;
	p2 = Mutable:ref(1`to64);
	// RRN: GETTING RID OF FOR/BREAK:
	/*	for i = 0 to 24 {
	  if (p2 >= p) then break;
	  p2 := p2 * 2;
	  }*/

	//	ind = Mutable:ref(0);
	//        while i <= 24 && p2 < p {
        while p2 < p {
          p2 := p2 * 2`to64;
	  //	  i := i + 1;  // Is this necessary?
	};

	emit (true,                               // yes, snapshot
	      _start - samples_padding,           // start sample
	      _start - samples_padding + p2 - 1`to64); // end sample
	if DEBUG then
	 log(1,"KEEP message: "++show((true, _start - samples_padding, en + samples_padding))++
	      " just processed window "++show(st)++":"++show(en)++"\n");

	// ADD TIME! // Time(casted->_first.getTimebase()
	_start := 0`to64;
      }
    } else { /* if we are not triggering... */      
      /* compute thresh */
      let thresh = i2f(hi_thresh) *. sqrtF(smoothed_var) +. smoothed_mean;

      if DEBUG then 
        log(1,"Thresh to beat: "++show(thresh)++ ", Current Score: "++show(score)++"\n");

      /* over thresh and not in startup period (noise est period) */
      if startup == 0 && score > thresh then {
	if DEBUG then log(1,"Switching trigger to ON state.\n");
	trigger := true;
	refract := refract_interval;
	thresh_value := thresh;
	_start := st;
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
      emit (false, 0`to64, max(0`to64, st - samples_padding - 1`to64));
      if DEBUG then 
      log(1, "DISCARD message: "++show((false, 0, max(0`to64, en - samples_padding)))++
	    " just processed window "++show(st)++":"++show(en)++"\n");
      
    }
  }
}



// ================================================================================


// RRN:
// Adding some combinators which we will target with the rewrite rules.

fun SSS_toArray(sss)   iterate x in sss { emit toArray(x) }
fun SSS_fromArray(sss) 
  iterate arr in sss { 
    state { stamp :: Int64 = 0 }
    emit toSigseg(arr, stamp, nulltimebase);
    stamp += intToInt64$ Array:length(arr);
  }

fun SA_fft(sa) 
  iterate arr in sa {
    emit marmfft(arr);
  }

fun SA_ifft(sa) 
  iterate arr in sa {
    emit ifftC2R(arr);
  }

// fft on a stream of sigsegs.  Could be primitive.
fun SSS_fft(sss) {
/*   SSS_fromArray $  */
/*   SA_fft $  */
/*   SSS_toArray $  */
/*     sss */
  smap(sigseg_marmfft, sss)
}

fun SSS_ifft(sss) {
/*   SSS_fromArray $  */
/*   SA_ifft $  */
/*   SSS_toArray $  */
/*     sss */
  smap(sigseg_ifftC2R, sss)
}

fun high_pass(size, cutoff, input) {
  using Curry;

  fun clearlow(arr) {
    //arr2 = Array:copy(arr);
    arr    
  };

  SSS_fromArray   $ 
   smap(clearlow) $
    SSS_toArray   $ 
     SSS_ifft     $ 
      input
}



toFreq:: (Int, Stream (Sigseg Float)) ->  Stream (Sigseg Complex);
fun toFreq(size, sss) {
   SSS_fft $
   hanning $ 
   rewindow(sss, size*2, 0 - size)
}


fromFreq :: (Int, Stream (Sigseg Complex)) ->  Stream (Sigseg Float);
fun fromFreq(size, sss) {
   hanning_merge $
   SSS_ifft $
     //   rewindow(sss, sizeg, 0 - size)
     sss
}

raw_bandpass :: (Int, Int, Int, Stream (Sigseg Complex)) ->  Stream (Sigseg Complex);
fun raw_bandpass(size, lo, hi, strm) {
   fun filt(ss) {
     len = ss`width;
     arr = ss`toArray;
     newarr = Array:make(len, 0);
     Array:blit(newarr, lo, arr, lo, hi-lo+1);
     toSigseg(newarr,ss`start, ss`timebase);
   };
  smap(filt, strm)
}

bandpass :: (Int, Int, Int, Stream (Sigseg Float)) ->  Stream (Sigseg Float);
fun bandpass(size, lo, hi, strm) {
   fromFreq(size,
     raw_bandpass(size,lo,hi, 
       toFreq(size, strm)))
}


  fun reduce(ss) {
    //println("   Reducing ss: "++ss);
    (Array:fold(fun(acc, x) acc + absC(x), 0.0, toArray(ss)), 
     ss`start, ss`end)
  };


//SSS_psd                                  :: (Int, Stream (Sigseg Float)) -> Stream Float;
fun SSS_psd(size, s) {
  Curry:smap(reduce) $ 
  toFreq(size, s)
}


fun oneshot(size, s) {
  Curry:smap(reduce) $ 
  raw_bandpass(16, 3, 4, 
       toFreq(size, s))
}




// RRN: Modifying this to make it amenable to rewrite opts.
/*detector :: (Stream (Sigseg Int16) * Stream (Sigseg Int16) * Stream (Sigseg Int16) * Stream (Sigseg Int16))
  -> Stream Detection;*/
fun detector((ch1i,ch2i,ch3i,ch4i)) {

  sfloats = deep_stream_map(int16ToFloat, ch1i);

  /* ULTRACRAP IMPLEMENTATION */
  // highpass and lowpass to simulate bandpass

  ver1 = {
    hi = fft_filter(sfloats, low_pass_coefs(16,4));
    lo = fft_filter(hi, high_pass_coefs(16,3));
    filtered = lo;
    // now compute psd
    psds = psd(filtered, 16);
    wscores = iterate p in psds {    
      emit( (Array:fold((+), 0.0, toArray(p)), p.start, p.end) )
    };
    wscores
  };
  ver2 = {
    filt = bandpass(16, 3, 4, sfloats);
    wscores = SSS_psd(16, filt);
    wscores
  };
  ver3 = oneshot(16,sfloats);
 
  wscores = if GETENV("HANDOPT_FFTIFFT")=="" then ver2 else ver3;
  //wscores = ver2;

  /* SEMICRAP IMPLEMENTATION -- doesnt work but for comparison */
  //psds = psd(sfloats, 16);

  // sum the psd 

  detections = detect(wscores);

  d2 = iterate (d in detections) {
    let (flag,_,_) = d;
    if flag then log(LOG_TIMING,"Detection at "++show(d)++
	", "++vxp_buffer_time_remaining()++" seconds in buffer");
    emit d;
  };

  synced_ints = syncN_no_delete(d2, [ch1i, ch2i, ch3i, ch4i]);

  // EVILHACKS:
  sparsify(1000, wscores)
}




// This puts the pieces together to make a detector that goes from 4
// audio channels all the way to detections.
lewis_detector :: (Stream (Sigseg Int16) * Stream (Sigseg Int16) * Stream (Sigseg Int16) * Stream (Sigseg Int16))
         -> Stream Detection;
fun lewis_detector((ch1i,ch2i,ch3i,ch4i)) {

  sfloats = deep_stream_map(int16ToFloat, ch1i);

  /* ULTRACRAP IMPLEMENTATION */
  // highpass and lowpass to simulate bandpass
  filtered = fft_filter(
               fft_filter(sfloats, low_pass_coefs(16,4)),
                                   high_pass_coefs(16,3));

  // now compute psd
  psds = psd(filtered, 16);

  /* SEMICRAP IMPLEMENTATION -- doesnt work but for comparison */
  //psds = psd(sfloats, 16);

  // sum the psd 
  wscores = iterate p in psds {
    emit( (Array:fold((+), 0.0, toArray(p)), p.start, p.end) )
  };

  detections = detect(wscores);

  d2 = iterate (d in detections) {
    let (flag,_,_) = d;
    if flag then log(LOG_TIMING,"Detection at "++show(d)++
	", "++vxp_buffer_time_remaining()++" seconds in buffer");
    emit d;
  };

  synced_ints = syncN_no_delete(d2, [ch1i, ch2i, ch3i, ch4i]);
  synced_ints
}

