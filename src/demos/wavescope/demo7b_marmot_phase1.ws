
// This is a hacked up version of the first phase of the marmot app.
// The marmot app proper is separate from this and is going in the apps/ 
// folder.

// [2007.03.05]
// Darn, runs in wsc but produces this as its first output:
//   WSOUT: (true, -2400, 2416)
// In "ws" it produces this:
//   

// [2007.03.22] Runs in wscaml, 100ms to process 1.2 mb file (6 sec).
// (600ms for ws.opt, 
/// 2700ms for wsc, 1200ms w/ -j 1, 253ms with constraining to one processor)


// [2007.03.23]
// Outfitted the caml version with bigarrays.  Tested on 3.6mb file (3x 6sec).
//   wscaml: 120ms
//   wsc   : 750ms    (with --at_once -j 1, and loading down the other cpu.)

// [2007.03.26] Now all backends are working with block-reading, let's
// do one final test on this file.  
//  Testing on faith, with 24 seconds of data (4x 6sec), 4.6 mb.
//  Loading one processor to make it uniprocessor.
//
//  Tuple-at-a-time reading:
//   ws    : 2100 ms
//   wsc   : 1000 ms 
//   wscaml: 144 ms
//
//  Blocked sigseg reading:
//   ws    : 1050 ms
//   wsc   : 550 ms
//   wscaml: 150 ms

//  handwritten c++: 530 ms,
//    time ./Marmot-SMSegList  -j 1 --at_once
//  (and this is *with* syncing, but does it output the data anywhere?)

//
// Notes, the caml version is using double precision floats/complex
// numbers.  The c++ version is still using single precision.
// The caml version is also using copy-always... so rewindow is expensive.

// [2007.03.28] Adapted the sigseg_seglist version to bigarray.
// Improves the performance (blocked reading) to 134 ms.  Presumably
// because rewindow is more efficent.


// [2007.06.29] Just got the MLton backend working on this.
// It only has a copy-always storage manager, but lets see how it does.
// Using the scheduler reported numbers for wsc, and whole process
// (user) times for mlton/caml.
// Note, also: wsc now uses the improved array representation.

//  Blocked sigseg reading:
//   ws.opt  : 948 ms
//   wsc     : 320-360 ms (513 real)
//   wscaml  : 164 ms (165 real)
//   wsmlton : 152 ms (178 real)  (Copy Always!)

// Ah... good to see, with the current scheduler (rev 1495) wsc
// actually does as well with both processors... at least for block
// reading.

// Just ran on ARM (1.1 mb input)
//   MLton:  real  1.273s  user 1.150s
//   wsc  :  real  4.519s  user 3.780s


// Hmm... my first shot at sigseg_seglist for mlton made performance worse.


//======================================================================



DEBUG = false
DEBUGSYNC = DEBUG 

//======================================================================
// "Library" routines:

fun window(S, len) 
  iterate(x in S) {
    state{ 
      arr = Array:null;
      ind = 0; 
      startsamp = 0`gint;
    }
    if ind == 0 then arr := Array:make(len, x);
    arr[ind] := x;
    ind := ind + 1;
    if ind == len
    then {
      emit toSigseg(arr, startsamp, nulltimebase);
      ind := 0;
      arr := Array:make(len, x); 
      startsamp := startsamp + len`gint;
    }
  };


// Constant:
M_PI = 3.141592653589793;

syncN       :: (List (Stream (Sigseg Float)), Stream (Bool * Int64 * Int64)) -> Stream (List (Sigseg Float));
fun syncN (strms, ctrl) {
  let _ctrl = iterate((b,s,e) in ctrl) { emit (b,(s::Int64),(e::Int64), (nullseg :: Sigseg Float)); };
  let f = fun(s) { iterate(win :: Sigseg Float in s) { 
                   emit (false,0`intToInt64, 0`intToInt64, (win :: Sigseg Float)); }; };
  let _strms = map(f, strms);

  let slist :: List (Stream (Bool * Int64 * Int64 * Sigseg t)) = _ctrl ::: _strms;
  
  // Side effect not allowed in iterate:
  //print("Syncing N streams: " ++ show(slist.List:length) ++ "\n");

  iterate((ind, tup) in unionList(slist)) {
    state {
      accs = Array:make(slist.List:length - 1, nullseg);
      requests = [];
    }
    print("  Current ACCS: ");
    for ii = 0 to accs.Array:length - 1 {
      if accs[ii] == nullseg
      then print("null  ")
      else print(show(accs[ii].start) ++ ":" ++ show(accs[ii].end) ++ "  ");
    };
    print("\n");

    let (flag, _strt, _en, seg) = tup;
    strt :: Int64 = _strt;
    en :: Int64 = _en;    

    // Process the new data:
    if ind == 0 // It's the ctrl signal.
    then requests := append(requests, [(flag,strt,en)])
    else accs[ind-1] := joinsegs(accs[ind-1], seg);
        
    // Now we see if we can process the next request.
    if requests == []
    then {} // Can't do anything yet...
    else {
      let (fl, st, en) = requests.head;
      let allready = 
	// This should be andmap, not fold:
	Array:fold(fun (bool, seg)
		   (bool               &&
		    not(seg == nullseg) &&
		    not(seg.start > st) &&
		    not(seg.end < en)),
		   true, accs);
     	
      if allready then {
	if fl then {
	  print("  Spit out segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	  size = int64ToInt(en - st) + 1; // Start,end are inclusive.

  	  emit List:map(fun (seg) subseg(seg,st,size), Array:toList(accs))

	} else 
	  print(" Discarding segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");

	// Destroy the discarded portions and remove the serviced request:
	for j = 0 to accs.Array:length - 1 {
	  // We don't check "st".  We allow "destroy messages" to kill already killed time segments.
	  ss :: Sigseg t = accs[j];
	  foo :: Int64 = end(ss);
	  accs[j] := subseg(accs[j], en + 1`intToInt64, int64ToInt(foo - en));
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
     
   iterate win in sig {
    state { 
      acc = nullseg; 
      // This bool helps to handle an output streams with gaps.
      // We have to states, true means we're to "output" a gap next,
      // false means we're to output a sigseg next.
      need_feed = false;
      go = true;
    }

    acc := joinsegs(acc, win);
    //print("Acc "++show(acc.start)++":"++show(acc.end)++" need_feed "++show(need_feed)++"\n");
    
    go := true;
    while go {
      //print("acc width "++ acc.width ++ " GO "++ go ++"\n");
      if need_feed then {
	if acc.width > gap // here we discard a segment:
	then {acc := subseg(acc, acc.start + gap`gint, acc.width - gap);
	      need_feed := false; }
	else go := false
      } else {
	if acc.width > newwidth
	then {emit subseg(acc, acc.start, newwidth);
	      if gap > 0 
	      then { 
		acc := subseg(acc, acc.start + newwidth`gint, acc.width - newwidth);
		need_feed := true; 
	      } else acc := subseg(acc, acc.start + feed`gint, acc.width - feed);
	} else go := false;	
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
      _hanning = Array:null;
    }

    if _lastLen != win.width then {
      _lastLen := win.width;
      _hanning := Array:make(_lastLen, 0.0);
      // Refil the hanning window:
      for i = 0 to _lastLen - 1 {
	//print("LASTLEN: "++show(intToFloat(_lastLen-1))++"\n");
	_hanning[i] := 0.5 *. (1.0 -. cos(2.0 *. M_PI *. intToFloat(i) /. intToFloat(_lastLen-1)));
	// RRN: This would fix the zeroed fenceposts:
	//_hanning[i] := 0.5 *. (1.0 -. cos(2.0 *. M_PI *. intToFloat(i+1) /. intToFloat(_lastLen+1)));
      }
    };

    /* alloc buffer */
    buf = Array:make(_lastLen, 0.0);
    for i = 0 to _lastLen - 1 {
      buf[i] := _hanning[i] *. win[[i]];
    }
    
    //print("\nWIN: "++ show(win)++"\n");
    //print("\nHAN: "++ show(_hanning)++"\n");
    //print("\nBUF: "++ show(buf)++"\n");

    emit toSigseg(buf, win.start, win.timebase);
  }
}

//======================================================================

// Takes Sigseg Complex
fun marmotscore(freqs) { 
  result = 
    absC(freqs[[4]] +: 
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
  max_run_length = 48000`intToInt64;
  samples_padding = 2400`intToInt64;

  iterate(rec in scorestrm) {
    state {
      thresh_value = 0.0;
      trigger = false;
      smoothed_mean = 0.0;
      smoothed_var = 0.0;
      _start :: Int64 = 0`gint; 
      trigger_value = 0.0;
      startup = 300;
      refract = 0;                 

      // private
      noise_lock = 0; // stats
      yay = false;
      shouldhitby = 100000`gint;
    }

    fun reset() {
      thresh_value := 0.0;
      trigger := false;
      smoothed_mean := 0.0; // 0; // TYPE INFERENC ERROR -- CHECK SET! CASE FIXME!!!!
      smoothed_var := 0.0;
      _start := 0 `gint;
      trigger_value := 0.0;
      startup := startup_init;
      refract := 0;
    };

    if not(yay) && rec.RAW.start > shouldhitby
    then wserror("Got to sample "++shouldhitby++" and didn't find a marmot detection!  "++
		 "Are you running on the normal datafile in the demos dir?");

    if DEBUG then 
    print("Detector state: thresh_value " ++show(thresh_value)++ " trigger " ++show(trigger)++ 
	  " smoothed_mean " ++show(smoothed_mean)++ " smoothed_var " ++show(smoothed_var)++ "\n" ++
	  "        _start " ++show(_start)++ " trigger_value " ++show(trigger_value)++ 
	  " startup " ++show(startup)++ " refract " ++show(refract)++ " noise_lock " ++show(noise_lock)++"\n"
	  );
    
    
    /* if we are triggering.. */
    if trigger then {      

      /* check for 'noise lock' */
      if rec.RAW.end - _start > max_run_length then {
	print("Detection length exceeded maximum of " ++ show(max_run_length)
	      ++", re-estimating noise\n");
	
	noise_lock := noise_lock + 1;
	reset();
	//goto done; GOTO GOTO GOTO 
      };

      /* over thresh.. set refractory */
      if rec.SCORE > thresh_value then {
	refract := refract_interval;
      } else if refract > 0 then {	
	/* refractory counting down */
	refract := refract - 1;
      }	else {
	/* untriggering! */
	trigger := false;

	yay := true;
	tmp :: Int64 = rec.RAW.end;

	/*
	emit (true,                            // yes, snapshot
	      _start - samples_padding,   // start sample
	      tmp + samples_padding); // end sample
	*/
	// HACK PAD IT FOR TESTALL_DEMOS:
	emit (Fired=true, Start= 0, End= 0`gint);


	if DEBUG then
	print("KEEP message: "++show((true, _start - samples_padding, rec.RAW.end + samples_padding))++
	      " just processed window "++show(rec.RAW.start)++":"++show(rec.RAW.end)++"\n");

	// ADD TIME! // Time(casted->_first.getTimebase()
	_start := 0`gint;
      }
    } else { /* if we are not triggering... */      
      /* compute thresh */
      let thresh = intToFloat(hi_thresh) *. sqrtF(smoothed_var) +. smoothed_mean;

      if DEBUG then 
        print("Thresh to beat: "++show(thresh)++ ", Current Score: "++show(rec.SCORE)++"\n");

      /* over thresh and not in startup period (noise est period) */
      if startup == 0 && rec.SCORE > thresh then {
	if DEBUG then print("Switching trigger to ON state.\n");
	trigger := true;
	refract := refract_interval;
	thresh_value := thresh;
	_start := rec.RAW.start;
	trigger_value := rec.SCORE;
      }	else {
	/* otherwise, update the smoothing filters */
	smoothed_mean := rec.SCORE *. (1.0 -. alpha) +. smoothed_mean *. alpha;
	delt = rec.SCORE -. smoothed_mean;
	smoothed_var := (delt *. delt) *. (1.0 -. alpha) +. smoothed_var *. alpha;
      };
	
      /* count down the startup phase */
      if startup > 0 then startup := startup - 1;
      
      /* ok, we can free from sync */
      /* rrn: here we lamely clear from the beginning of time. */
      /* but this seems to assume that the sample numbers start at zero?? */
      emit (Fired= false, Start= 0, End= max(0`gint, rec.RAW.end - samples_padding));
      if DEBUG then 
      print("DISCARD message: "++show((false, 0, max(0`gint, rec.RAW.end - samples_padding)))++
	    " just processed window "++show(rec.RAW.start)++":"++show(rec.RAW.end)++"\n");
      
    }
  }
}

stream_filter :: (t -> Bool, Stream t) -> Stream t;
fun stream_filter(f,s) {
  iterate x in s {
    if f(x) then emit x
    //   else print("Discarded: "++ x ++"\n")
  }
}

// This doesn't create a shared structure:
fun deep_smap(f,S) {
  iterate w in S {
    output = Array:build(w`width, fun (i) f(w[[i]]));
    emit toSigseg(output, w`start, w`timebase);
  }
}

//========================================
// Main query:

// Two ways to read the data:

/*
chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) :: Stream (Int16 * Int16 * Int16 * Int16));
_ch1 = window(iterate((a,_,_,_) in chans){ emit int16ToFloat(a) }, 4096);
_ch2 = window(iterate((_,b,_,_) in chans){ emit int16ToFloat(b) }, 4096);
ch3 = window(iterate((_,_,c,_) in chans){ emit int16ToFloat(c) }, 4096);
ch4 = window(iterate((_,_,_,d) in chans){ emit int16ToFloat(d) }, 4096);
*/

// [2007.03.24] Using the new "readFile" we can read sigsegs directly:
fun toFl(S) { deep_smap(int16ToFloat, S) }
fn = "6sec_marmot_sample.raw";
config = "mode: binary  window: 4096  skipbytes: 6 ";
driver = timer(24000.0 / 4096.0);
_ch1 = toFl((readFile(fn, config,                driver) :: Stream (Sigseg Int16)));
_ch2 = toFl((readFile(fn, config ++ "offset: 2", driver) :: Stream (Sigseg Int16)));
ch3  = toFl((readFile(fn, config ++ "offset: 4", driver) :: Stream (Sigseg Int16)));
ch4  = toFl((readFile(fn, config ++ "offset: 6", driver) :: Stream (Sigseg Int16)));


ch1 = _ch1; ch2 = _ch2;
//ch1 = gnuplot_sigseg_stream(_ch1);  ch2 = gnuplot_sigseg_stream(_ch2);

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

  fun sigseg_fftR2C (ss) toSigseg(ss`toArray`fftR2C,  ss.start, ss.timebase)
freq = iterate(x in hn) { emit (FREQ= sigseg_fftR2C(x), RAW= x) };

//wscores = smap(fun(w){(marmotscore(w), w)}, freq);
wscores = iterate r in freq { emit (r | ~FREQ, SCORE= marmotscore(r.FREQ)); }

detections = detect(wscores);


positives = stream_filter(fun(r) r.Fired, detections)
		   
//synced = syncN(detections, [ch1, ch2, ch3, ch4]);
//synced = syncN(dummydetections, [ch1, ch2, ch3, ch4]);

// [2006.09.04] RRN: Currently it doesn't ever detect a marmot.
// If you try to do the real syncN, it will process the whole without outputing anything.

// [2007.08.09] DETECTIONS IS BROKEN.  FOR SOME ODD REASON THE 64BIT SAMPLE NUMBERS BROKE THE DECTOR.
// DISABLING FOR NOW BECAUSE IT'S NOT A PRIORITY.
main = 
//synced
//positives
detections
//wscores
// iterate(w in hn){emit w[[16]]}
// iterate(w in rw1){emit w[[16]]}
//unionList([ch1,ch2])
//iterate(w in ch1){emit w`width}
//ch1
//iterate w in ch1 { emit w[[100]] }
//iterate w in (readFile(fn, "mode: binary  rate: 24000  window: 4096  skipbytes: 6 ") :: Stream (Sigseg Int16)) { emit w[[100]] }



//timer(3.0)
//iterate(w in _ch1) { print("test\n"); emit w[[0]] }
//iterate(w in rw1) { print("test\n"); emit w[[0]] }



//iterate(w in freq){emit w[[15]]}

//freq
//iterate(w in freq){emit (w.start, w.end)}

//wscores
//iterate((sc,w) in wscores) {emit sc}

;
