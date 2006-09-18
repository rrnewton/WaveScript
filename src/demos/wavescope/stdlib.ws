
// Defines: 
//   M_PI
//   syncN, rewindow, myhanning


// Constant:
M_PI = 3.141592653589793;

//======================================================================
// "Library" routines:


fun syncN (ctrl, strms) {
  _ctrl = iterate((b,s,e) in ctrl) { emit (b,s,e, nullseg); };
  f = fun(s) { iterate(win in s) { emit (false,0,0, win); }; };
  _strms = map(f, strms);  
  slist = _ctrl :: _strms;  

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
	    output := subseg(accs[i], st, size) :: output;
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
      buf[i] := _hanning[i] *. win[[win.start + i]];
    }
    
    //print("\nWIN: "++ show(win)++"\n");
    //print("\nHAN: "++ show(_hanning)++"\n");
    //print("\nBUF: "++ show(buf)++"\n");

    emit to_sigseg(buf, win.start, win.end, win.timebase);
  }
}
