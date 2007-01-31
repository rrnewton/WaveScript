
// Defines: 
//   M_PI
//   syncN, rewindow, myhanning

// NEED:
// qsort, Complexl, expc, atan2, MInv...

// Constant:
PI   = 3.141592653589793;
PIO2 = PI/2.0;
 
//======================================================================
// Library POD (plain old data) functions:

fun println(s) {
  print(s);
  print("\n");
};

// For completeness we include these restrictions of their generic counterparts:
//fun intToFloat     (i::Int)   toFloat(i) 
//fun intToComplex   (i::Int)   toComplex(i) 
//fun floatToComplex (f::Float) toComplex(f) 

//======================================================================
// "Library" stream constructors:

syncN :: (Stream (Bool * Int * Int),  List (Stream (Sigseg t))) 
         -> Stream (List (Sigseg t));
syncN = if IS_SIM then __syncN else
 fun (ctrl, strms) {
   DEBUGSYNC = true; // Activate to debug the below code:

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
    
    // Debugging helper functions:
    fun printaccs() {
      for i = 0 to accs.length - 1 {
	if accs[i] == nullseg
	then print("null  ")
	else print(show(accs[i].start) ++ ":" ++ show(accs[i].end) ++ "  ");
      }
    };
    fun printwidths(){
      for i = 0 to accs.length - 1 {
	if accs[i] == nullseg
	then print("0   ")
	else print(show(accs[i].width) ++ " ");
      }
    };

    //if DEBUGSYNC then { print("SyncN  Current ACCS: "); printaccs(); print("\n") };
    //if DEBUGSYNC then { print("SyncN  ACC widths: "); printwidths(); print("\n") };
    if DEBUGSYNC then { print("SyncN ACCS: "); printaccs(); print("    "); printwidths(); print("\n") };

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
	    (fl && accs[i].start > st) || // This only matters if we're retaining it.
	    accs[i].end < en)
	then { 
	  allready := false;
	  if DEBUGSYNC 
	  then print("  Not all ready: "
		     ++ show(accs[i] == nullseg) ++ " "
		     ++ show(fl && accs[i].start > st) ++ " "
		     ++ show(accs[i].end < en) ++ "\n");
	}
      }
      // The data is ready on all buffers, now it's time to either discard or output it.
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
	} else {
	  if DEBUGSYNC then
	  print("SyncN: Discarding segment: " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	};
	// In either case, destroy the finished portions and remove the serviced request:
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
rewindow :: (Stream(Sigseg t), Int, Int) -> Stream(Sigseg t);
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
myhanning :: Stream (Sigseg Float) -> Stream (Sigseg Float);
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
	//print("LASTLEN: "++show(intToFloat(_lastLen-1))++"\n");
	_hanning[i] := 0.5 *. (1.0 -. cos(2.0 *. PI *. intToFloat(i) /. intToFloat(_lastLen-1)));
	// RRN: This would fix the zeroed fenceposts:
	//_hanning[i] := 0.5 *. (1.0 -. cos(2.0 *. M_PI *. intToFloat(i+1) /. intToFloat(_lastLen+1)));
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

    emit toSigseg(buf, win.start, win.end, win.timebase);
  }
}


//======================================================================
// Higher order routines which should have built-in support at some point.


/*   sigseg_foreach(f,ss)  */
/*   foreachi ((i,x) in ss) { } */

/* fun sigseg_foreach(f,ss) { */
/*   for i = 0 to ss.width-1 { */
/*     f(ss[[i]]); */
/*   } */
/* } */
/* fun sigseg_foreachi(f,ss) { */
/*   for i = 0 to ss.width-1 { */
/*     f(i, ss[[i]]); */
/*   } */
/* } */


// This doesn't create a shared structure:
deep_stream_map :: ((a -> b), Stream (Sigseg a)) -> Stream (Sigseg b);
fun deep_stream_map(f,sss) {
  iterate(ss in sss) {    
    first = f(ss[[0]]);
    output = makeArray(ss.width, first);
    for i = 1 to ss.width - 1 {
      output[i] := f(ss[[i]]);
    }
    emit toSigseg(output, ss.start, ss.end, ss.timebase);
  }
}

// UNFINISHED:
// Assumes in-order but possibly overlapping
//deep_stream_map2 :: ((a -> b), Stream (Sigseg a)) -> Stream (Sigseg b);
fun deep_stream_map2(f,sss) {
  iterate(ss in sss) {
    state { 
      pos = 0;
      lastout = nullseg;
    }
    first = f(ss[[0]]);
    output = makeArray(ss.width, first);
    if pos > ss.start then
      for i = 0 to pos - ss.start - 1 {
	// Copy old result:
	output[i] := lastout[[i + (ss.start - lastout.start)]];
      }	     
  };
  strm = rewindow(sss,100,0);
}

stream_map :: (a -> b, Stream a) -> Stream b;
fun stream_map(f,s) {
  iterate (x in s) {
    emit f(x);
  }
}

stream_filter :: (t -> Bool, Stream t) -> Stream t;
fun stream_filter(f,s) {
  iterate (x in s) {
    if f(x) then emit x
  }
}

stream_iterate :: ((inp, st) -> (List out * st), st, Stream inp) -> Stream out;
fun stream_iterate(f,z,s) {
  iterate (x in s) {
    state { sigma = z }
    let (ls,sig2) = f(x,sigma);
    sigma := sig2;
    // list_foreach(fun(t) emit t, ls);
    for i = 0 to ls.listLength-1 {
      emit listRef(ls,i);
    }
  }
}

//======================================================================
// Useful aliases:

i2f = intToFloat;
i2c = intToComplex;
f2i = floatToInt;
f2c = floatToComplex;
c2i = complexToInt;
c2f = complexToFloat;

/* Some additional math functions */

fun sqr(x) { x*x }

fun atan2(arg1,arg2) {
  if (arg1+arg2) == arg1 then {
    if arg1 >= 0.0 then PIO2
    else -1.0*PIO2
  }
  else {
    tmp = atan(arg1/arg2);
    if arg2 < 0.0 then {
      if tmp <= 0.0 then (tmp + PI)
      else (tmp - PI)
    }
    else tmp
  }
}

/* test1 = stream_map(fun(w) w[[0]], audio(0,1024,0)); */
/* test2 = stream_filter(fun (n) n > 300.0, test1); */
/* test3 = stream_iterate(fun (x,st) ([x +. st, 5.0, 6.0], st +. 100.0), */
/* 		       0.0, test2); */
/* test4 = deep_stream_map(fun(x) x /. 100.0, audio(0,10,0)) */
/* BASE <- test4; */

//BASE <- audio(0,10,0);

