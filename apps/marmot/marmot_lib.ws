
DEBUG = false

// Defines: marmotscore, detect

//======================================================================

// Takes Sigseg Complex
fun marmotscore(freqs) { 
  result = 
    cnorm(freqs[[4]] +: 
	  freqs[[5]] +:
	  freqs[[6]] +:
	  freqs[[7]]);
  if DEBUG then 
   print("\nMarmot Score: "++show(result)++", \nBased on values "
	++ show(freqs[[4]]) ++ " "
	++ show(freqs[[5]]) ++ " "
	++ show(freqs[[6]]) ++ " "
	++ show(freqs[[7]]) ++ " \n");
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
	if DEBUG then
	print("KEEP message: "++show((true, _start - samples_padding, win.end + samples_padding))++
	      " just processed window "++show(win.start)++":"++show(win.end)++"\n");

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
      emit (false, 0, max(0, win.end - samples_padding));
      if DEBUG then 
      print("DISCARD message: "++show((false, 0, max(0, win.end - samples_padding)))++
	    " just processed window "++show(win.start)++":"++show(win.end)++"\n");
      
    }
  }
}

