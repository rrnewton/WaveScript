
// This is a hacked up version of the first phase of the marmot app.
// The marmot app proper is separate from this and is going in the apps/ 
// folder.

DEBUG = false
DEBUGSYNC = DEBUG

//======================================================================
// "Library" routines:

// Constant:
M_PI = 3.141592653589793;

fun syncN (ctrl, strms) {
  _ctrl = iterate((b,s,e) in ctrl) { emit (b,s,e, nullseg); };
  f = fun(s) { iterate(win in s) { emit (false,0,0, win); }; };
  _strms = map(f, strms);
  slist = _ctrl : _strms;

  if DEBUGSYNC 
    then print("Syncing N streams (including ctrl stream): " ++ show(slist.listLength) ++ "\n");

  iterate((ind, tup) in unionList(slist)) {
    state {
      accs = makeArray(slist.listLength - 1, nullseg);
      requests = [];
    }

    if DEBUGSYNC then {
      print("SyncN  Current ACCS: ");
      for i = 0 to accs.length - 1 {
	if accs[i] == nullseg
	then print("null  ")
	else print(show(accs[i].start) ++ ":" ++ show(accs[i].end) ++ "  ");
      };
      print("\n");
    };

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
      }
      if allready then {
	if fl then {
	  if DEBUGSYNC 
	  then print("SyncN: Output segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	  size = en - st + 1; // Start,end are inclusive.
	  output = [];
	  for i = 0 to accs.length - 1 {
	    output := subseg(accs[i], st, size) : output;
	  }
	  emit(reverse(output));
	} else if DEBUGSYNC then
	  print("SyncN: Discarding segment: " ++ show(st) ++ ":" ++ show(en) ++  "\n");

	// Destroy the discarded portions and remove the serviced request:
	for j = 0 to accs.length - 1 {
	  // We don't check "st".  We allow "destroy messages" to kill already killed time segments.
	  accs[j] := subseg(accs[j], en + 1, accs[j].end - en);
	};
	requests := requests.tail;
      }
    }
  }
}

// This version is enhanced to allow large steps that result in gaps in the output streams.
//   GAP is the space *between* sampled strips, negative for overlap!
fun rewindow(sig, newwidth, gap) {
  feed = newwidth + gap;

  if (gap <= (0 - newwidth))
    then wserror("rewindow cannot step backwards: width "++ show(newwidth) ++" gap "++show(gap))
    else 
     
   iterate (win in sig) {
    state { 
      acc = nullseg; 
      // This bool helps to handle an output streams with gaps.
      // We have to states, true means we're to "output" a gap next,
      // false means we're to output a sigseg next.
      need_feed = false;
    }

    acc := joinsegs(acc, win);
    //print("Acc "++show(acc.start)++":"++show(acc.end)++" need_feed "++show(need_feed)++"\n");

    for i = 1 to win.width {
      if need_feed then {
	if acc.width > gap // here we discard a segment:
	then {acc := subseg(acc, acc.start + gap, acc.width - gap);
	      need_feed := false; }
	else break;
      } else {
	if acc.width > newwidth
	then {emit subseg(acc, acc.start, newwidth);
	      if gap > 0 
	      then { 
		acc := subseg(acc, acc.start + newwidth, acc.width - newwidth);
		need_feed := true; 
	      } else acc := subseg(acc, acc.start + feed, acc.width - feed);
	} else break;	
      }
    }
  }
}


// RRN: This has the problem that the hanning coefficient is ZERO at
// the first and last element in the window.  These represent wasted samples.
// myhanning : Sigseg Float -> Sigseg Float;
fun myhanning (strm) {
  iterate(win in strm) {
    state{ 
      _lastLen = 0;
      _hanning = nullarr;
    }

    if _lastLen != win.width then {
      _lastLen := win.width;
      _hanning := makeArray(_lastLen, 0.0);
      // Refil the hanning window:
      for i = 0 to _lastLen - 1 {
	//print("LASTLEN: "++show(int_to_float(_lastLen-1))++"\n");
	_hanning[i] := 0.5 *. (1.0 -. cos(2.0 *. M_PI *. int_to_float(i) /. int_to_float(_lastLen-1)));
	// RRN: This would fix the zeroed fenceposts:
	//_hanning[i] := 0.5 *. (1.0 -. cos(2.0 *. M_PI *. int_to_float(i+1) /. int_to_float(_lastLen+1)));
      }
    };

    /* alloc buffer */
    buf = makeArray(_lastLen, 0.0);
    for i = 0 to _lastLen - 1 {
      buf[i] := _hanning[i] *. win[[i]];
    }
    
    //print("\nWIN: "++ show(win)++"\n");
    //print("\nHAN: "++ show(_hanning)++"\n");
    //print("\nBUF: "++ show(buf)++"\n");

    emit to_sigseg(buf, win.start, win.end, win.timebase);
  }
}

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
      thresh_value := 0.0;
      trigger := false;
      smoothed_mean := 0.0; // 0; // TYPE INFERENC ERROR -- CHECK SET! CASE FIXME!!!!
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
	      ++", re-estimating noise\n");
	
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

//========================================
// Main query:

ch1 = audio(0, 4096, 0);
ch2 = audio(1, 4096, 0);
ch3 = audio(2, 4096, 0);
ch4 = audio(3, 4096, 0);

outwidth=100;
dummydetections = iterate(w in ch1) {
  state { 
    pos = 0; 
    flag = false; 
  }
  emit(flag, pos, pos + outwidth - 1);
  pos := pos + outwidth;
  flag := if flag then false else true;
};

// 96 samples are ignored between each 32 used:
rw1 = rewindow(ch1, 32, 96); 

//hn = smap(hanning, rw1);
hn = myhanning(rw1);

freq = smap(fft, hn);

//wscores = smap(fun(w){(marmotscore(w), w)}, freq);
wscores = iterate (w in freq) { emit (marmotscore(w), w); }

detections = detect(wscores);

positives = sfilter(fun((b,_,_)) b, detections)
		   
synced = syncN(detections, [ch1, ch2, ch3, ch4]);
//synced = syncN(dummydetections, [ch1, ch2, ch3, ch4]);

// [2006.09.04] RRN: Currently it doesn't ever detect a marmot.
// If you try to do the real syncN, it will process the whole without outputing anything.

//BASE <- positives;
//BASE <- detections;
BASE <- synced;
