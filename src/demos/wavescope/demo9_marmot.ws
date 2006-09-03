
//======================================================================
// "Library" routines:

fun syncN (ctrl, strms) {
  _ctrl = iterate((b,s,e) in ctrl) { emit (b,s,e, nullseg); };
  f = fun(s) { iterate(win in s) { emit (false,0,0, win); }; };
  _strms = map(f, strms);  
  slist = _ctrl :: _strms;  

  iterate((ind, tup) in unionList(slist)) {
    state {
      accs = makeArray(slist.listLength - 1, nullseg);
      requests = [];
    }
    let (flag, strt, en, seg) = tup;
    // Process the new data:
    if ind == 0 // It's the ctrl signal.
    then requests := append(requests, [(flag,strt,en)])
    else accs[ind-1] := joinsegs(accs[ind-1], seg);        
    // Now we see if we can process the next request.
    if requests == []
    then {} // Can't do anything yet...
    else {
      let (fl, st, en) = requests.head;
      allready = true;
      for i = 0 to accs.length - 1 {
	if (accs[i] == nullseg ||
	    accs[i].start > st ||
	    accs[i].end < en)
	then allready := false;
      };     	
      if allready then 
      {
	size = en - st + 1; // Start/end is inclusive.
	output = [];
	for i = 0 to accs.length - 1 {
	  output := subseg(accs[i], st, size) :: output;
	};
	emit(reverse(output));
	// Destroy the output portions and remove the serviced request:
	for j = 0 to accs.length - 1 {
	  accs[j] := subseg(accs[j], st + size, accs[j].width - size);
	};
	requests := requests.tail;
      }
    }
  }
}

fun rewindow(sig, newwidth, step) {
  if step > newwidth
  then wserror("rewindow won't allow the creation of non-contiguous output streams")
  else iterate (w in sig) {
    state { acc = nullseg; }
    acc := joinsegs(acc, w);
    for i = 1 to w.width {
      if acc.width > newwidth
      then {emit subseg(acc, acc.start, newwidth);
	    acc := subseg(acc, acc.start + step, acc.width - step)}
      else break;
    }
  }
}

//======================================================================

// Takes Sigseg Complex
fun marmotscore(freqs) { 
  st = freqs.start;
  cnorm(freqs[[st + 4]] +: 
	freqs[[st + 5]] +:
	freqs[[st + 6]] +:
	freqs[[st + 7]]);
}

/* expects Zip2<SigSeg<float>,float>::Output */
fun detect(scorestrm) {
  iterate((score,win) in scorestrm) {
    state {
      alpha = 0.999;
      hi_thresh = 8;
      startup_init = 300;
      refract_interval = 40;
      max_run_length = 48000;
      samples_padding = 2400;

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
    
    // float, float, float -> float
    //    fun UpdateEWMA( new_value, *state, alpha) {
    //      return *state = new_value * (1.0-alpha) + *state * alpha;
    //    }    
    
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

      /* over thresh and not in startup period (noise est period) */
      if startup == 0 && score > thresh then {
	trigger := true;
	refract := refract_interval;
	thresh_value := thresh;
	_start := win.start;
	trigger_value := score;
      }	else {
	/* otherwise, update the smoothing filters */
	// TODO: 
	//UpdateEWMA(score, &(smoothed_mean), alpha);
	//UpdateEWMA(sqrf(score-smoothed_mean), &(smoothed_var), alpha);
      };
	
      /* count down the startup phase */
      if startup > 0 then startup := startup - 1;
      
      /* ok, we can free from sync */
      // TODO:
      //emit(sync_ctrl(Time(), casted->_first.endTime() - samples_padding, false));
      //emit(false, ??, win.end - samples_padding);
    }
  }
}

/*

    
      /* if we are not triggering... */
      else {

	/* compute thresh */
	float thresh = hi_thresh*sqrtf(smoothed_var) + smoothed_mean;

	/* over thresh and not in startup period (noise est period) */
	if ((startup == 0) && score > thresh) {
	  trigger = 1;
	  refract = refract_interval;
	  thresh_value = thresh;
	  start = casted->_first.start();
	  trigger_value = score;
	}
	
	/* otherwise, update the smoothing filters */
	else {
	  UpdateEWMA(score, &(smoothed_mean), alpha);
	  UpdateEWMA(sqrf(score-smoothed_mean), &(smoothed_var), alpha);
	}
	
	/* count down the startup phase */
	if (startup) startup--;    

	/* ok, we can free from sync */
	emit(sync_ctrl(Time(), casted->_first.endTime() - samples_padding, false));
      }
      
    done:
      delete casted;
    }
    
    return false;
  }
};
*/


//========================================
// Main query:

ch1 = audio(0, 128, 0);
ch2 = audio(1, 128, 0);
ch3 = audio(2, 128, 0);
ch4 = audio(3, 128, 0);

rw1 = rewindow(ch1, 32, 32);
hn = smap(hanning, rw1);
freq = smap(fft, hn);

//wscores = smap(fun(w){(marmotscore(w), w)}, freq);
wscores = iterate (w in freq) { emit(marmotscore(w), w); }

detections = detect(wscores);

/*
  iterate (pr in wscores) {
    state { counter = 0; }
    let (sc,w) = pr;
    
    print("  SCORE: " ++show(sc)++ "\n");
    
    emit(true, counter, counter+9);
    counter := counter + 10;
  };
*/

//synced = syncN(detections, [ch1, ch2, ch3, ch4]);

// Currently the output is CRUDE, and incomplete.  It basically just
// gives you synced windows so that you know syncN is working.  The
// detector's not implemented, marmotscore is not implemented, and
// hanning windows are not currently implemented either.
BASE <- wscores;








//======================================================================
//SCRATCH:

// UNFINISHED:
/* fun sync4 (ctrl, s1, s2, s3, s4) { */
/*   // A lame sort of manual union type.  Pad all streams out with all fields: */
/*   _ctrl = iterate((b,s,e) in ctrl) { emit (b,s,e, nullseg); }; */
/*   _s1   = iterate(win in s1) { emit (false,0,0, win); }; */
/*   _s2   = iterate(win in s2) { emit (false,0,0, win); };   */

/*   // Now it's homogenously typed. */
/*   slist = [ _ctrl, _s1 , _s2];   */

/*   iterate((ind, tup) in unionList(slist)) { */
/*     state { */
/*       acc1 = nullseg; */
/*       acc2 = nullseg; */
/*       acc3 = nullseg; */
/*       acc4 = nullseg; */
/*       requests = []; */
/*     } */
/*     let (flag, strt, en, seg) = tup; */

/*     // Process the new data: */
/*     if ind == 0 // It's the ctrl signal. */
/*     then requests := append(requests, [(flag,strt,en)]) */
/*     else if ind == 1 */
/*     then acc1 := joinsegs(acc1, seg) */
/*     else acc2 := joinsegs(acc2, seg); */
    
/*     // Now we see if we can process the next request.      */
/*     if requests == [] */
/*     then {} */
/*     else { */
/*       let (fl, st, en) = requests.head; */
/*       if (acc1 != nullseg  &&  	  acc2 != nullseg   &&    acc3 != nullseg  &&  	  acc4 != nullseg && */
/* 	  acc1.start <= st && 	  acc2.start <= st  &&    acc3.start <= st &&     acc4.start <= st &&  */
/* 	  acc1.end >= en   &&	  acc2.end >= en    &&	  acc3.end >= en   &&	  acc4.end >= en) */
/*       then { */

/* 	size = en - st + 1; // Start & end are inclusive. */
/* 	emit (subseg(acc1, st, size), */
/* 	      subseg(acc2, st, size), */
/* 	      subseg(acc3, st, size), */
/* 	      subseg(acc4, st, size)); */
/* 	acc1 := subseg(acc1, st + size, acc1.width - size); */
/* 	acc2 := subseg(acc2, st + size, acc2.width - size); */

/* 	// The request is serviced. */
/* 	requests := requests.tail; */
/*       } */
/*     } */
/*   } */
/* } */
