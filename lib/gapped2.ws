
// A library of stream primitives that handle gapped streams.
// For now gaps are represented by nullsegs.


include "stdlib.ws";

// A gap contoins start and width.
uniontype GappedStream t = Gap (Int64 * Int) | Seg (Sigseg t);

DEBUGGAPPED = false;

namespace Gapped {

  namespace Internal {

    // HAVE TO LOOK FOR FENCEPOST ERRORS:
    fun rewindow(sig, newwidth, gap) {
      feed = gint(newwidth + gap);

      if (gap <= (0 - newwidth))
      then wserror("rewindow cannot step backwards: width "++ show(newwidth) ++" gap "++show(gap))
      else if gap >= 0 then 

	// POSITIVE GAP
        iterate sum in sig {
          state {
	    acc = nullseg;
	    // This is the next sample that's actually requested.
	    nextpos = -1 `to64;
          }

	  fun try_output () {
	    //println("Trying output, nextpos "++nextpos++" acc "++acc);
	    while acc`start <= nextpos && nextpos+gint(newwidth) <= acc`start+gint(acc`width) {
	      if DEBUGGAPPED then println("Emitting chunk: "++nextpos++ " width "++newwidth);
	      if DEBUGGAPPED then println("From acc starting: "++acc`start++" width "++acc`width);
 	      cut = subseg(acc, nextpos, newwidth);
	      //println("Here's the cut: "++cut);
	      emit cut;
	      nextpos += feed;
	    };
	    // When we're done, let's trim anything we'll never need from the acc:
	    // That is, anything already emitted.
	    if acc`start < nextpos then 
	    if acc`end < nextpos then 
  	      acc := nullseg
	    else {
	      remaining = (acc`width - from64(nextpos - acc`start));
	      if DEBUGGAPPED then println("Cutting acc down from width "++acc`width++" to "++ remaining);
	      acc := subseg(acc, nextpos, remaining);
	    }
	  };

	  case sum {
	    Gap(tup): {
	      let (st,wid) = tup;
	      // If the nextpos is in the gap, then that's bad, we reset.
	      if nextpos >= st && nextpos < st + wid`gint 
	      then {	          
		  if DEBUGGAPPED then println("Gapped:rewindow - RESET");
		  acc := nullseg;
		  nextpos := -1 `gint;
	      }
  	      else {}
	    }
            Seg(win):
	      // If we get a gap, we do nothing.
	      if win == nullseg 
	      then wserror("Gapped:rewindow - even gapped streams shouldn't contain nullsegs")
	      else {
		// If it's the first time, just set the state.
		if nextpos == -1`gint then {
		  if DEBUGGAPPED then println("Gapped:rewindow - first time running");
		  acc := win;
		  nextpos := win`start;
		}
		// If the acc is null ...
		else if acc == nullseg then {
	          acc := win
		}
                // If we get what we're expecting, that's fine.
		else {
		  assert_eq("Rewindow:gapped got incorrect input stream", acc`end + 1`gint, win`start);
		  if DEBUGGAPPED then println("Gapped:rewindow - normal join");
		  acc := joinsegs(acc, win);		  
		};

		// In any of the above cases we try to output:
		try_output();
	      }
	  }
      }
    else
      // NEGATIVE GAP
      iterate _ in sig {
      wserror("Gapped:rewindow -- Not handling overlapping windows yet.");
    }
  }
 }


// This takes a stream of sigsegs any gaps in the stream will be
// replaced with null sigsegs.
//markgaps :: Stream t -> GappedStream t;
fun markgaps(stm) {
  iterate w in stm {
    state {
      next = to64(-1);
    }
    if w == nullseg then wserror("Gapped:markgaps -- should not have nullsegs in input stream")
    else if w`start > next && (next != to64(-1))
    then emit Gap((next, from64(w`start - next)));
    emit Seg(w);
    next := w`end + gint(1);
    }
  }

// We add the gaps back in separately.
fun rewindow(sig,newwidth,gap)
  markgaps(Internal:rewindow(sig,newwidth,gap))


// This is an internal helper that can be parameterized in two ways to
// form "syncN" and "syncN_no_delete".
Internal:syncN_aux = 
fun (ctrl, strms, del) {
   DEBUGSYNC = false; // Activate to debug the below code:

   ENABLEWARNINGS = false;
   WARNSKEW = 60000; // threshold for warning that an accumulator has grown to big.  Should be user input.

  _ctrl = iterate((b,s,e) in ctrl) { emit ((b,s,e, nullseg) :: (Bool * Int64 * Int64 * Sigseg any)); };
  f = fun(s) { iterate(win in s) { emit (false,0`gint,0`gint, win); }; };
  _strms = map(f, strms);  
  slist = _ctrl ::: _strms;  

   //  if DEBUGSYNC then print("Syncing N streams (including ctrl stream): " ++ show(slist`List:length) ++ "\n");

  iterate((ind, tup) in unionList(slist)) {
    state {
      accs = Array:make(slist`List:length - 1, nullseg);
      requests = [];
    }
    
    // Debugging helper functions:
    fun printaccs() {
      for i = 0 to accs`Array:length - 1 {
	if accs[i]`width == 0
	then print("null  ")
	else print(show(accs[i] `start) ++ ":" ++ show(accs[i] `end) ++ "  ");
      }
    };
    fun printwidths(){
      for i = 0 to accs`Array:length - 1 {
	if accs[i]`width == 0
	then print("0   ")
	else print(show(accs[i]`width) ++ " ");
      }
    };

    //if DEBUGSYNC then { print("SyncN  Current ACCS: "); printaccs(); print("\n") };
    //if DEBUGSYNC then { print("SyncN  ACC widths: "); printwidths(); print("\n") };
    if DEBUGSYNC then 
    { print("SyncN ACCS: "); printaccs(); print("    "); printwidths(); print("  tag value "++show(ind)); print("\n") };

    // First do a "skew" check to detect when one accumulator has gotten to big. 
    for i = 0 to Array:length(accs)-1 {
      if ENABLEWARNINGS then      
      if width(accs[i]) > WARNSKEW
      then {
        print("WARNING: skewed sync, acc sizes: ");
	for j = 0 to Array:length(accs)-1 {
	  print( accs[j]`width ++ " ");
	};
	//	print("\n");
	if requests == [] 
	then print(" no requests.\n")
        else print(" waiting for "++ requests`head ++"\n");
      }
    };


    let (flag, strt, en, seg) = tup;
    
    // ========================================
    // Process the new data:
    if ind == 0 // It's the ctrl signal.
    then requests := append(requests, [(flag,strt,en)])
    else accs[ind-1] := joinsegs(accs[ind-1], seg);        

    // ========================================
    // Now we see if we can process the next request.
    if requests == []
    then {} // Can't do anything yet...
    else {
      let (fl, st, en) = requests`head;

      allready =
	Array:andmap(
	 fun (seg)
	 if (seg == nullseg         ||
	     (fl && seg`start > st) || // This only matters if we're retaining it.
	     seg`end < en)
	   then { 		       
	     if DEBUGSYNC then {
	       if (seg == nullseg) then
		 println("  Not all ready: NULL")
	       else
		 println("  Not all ready: "
			  ++ show(fl && seg`start > st) ++ " "
			  ++ show(seg`end < en) ++ " " 
	                  ++ ((st,en,seg`start,seg`end)));
	     };
             
             if (seg != nullseg && fl && seg`start > st) then {
               println("Sync has a request that can never be filled:");
	       println("Accumulator has "++seg`start++" to "++
		       seg`end++", but request is for "++
		       st++" to "++en++".");
	       wserror("Impossible sync request");
	     };

	     false }
  	   else true,
	 accs);

      // The data is ready on all buffers, now it's time to either discard or output it.
      if allready then {
	if fl then {
	  if DEBUGSYNC 
	  then print("SyncN: Output segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	  size = int64ToInt(en - st) + 1; // Start,end are inclusive.
  	  emit List:map(fun (seg) subseg(seg,st,size), Array:toList(accs))
	} else {
	  if DEBUGSYNC then
	  print("SyncN: Discarding segment: " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	};

	if (del || not(fl)) then {
	  // In either case, destroy the finished portions and remove the serviced request:
	  for j = 0 to accs`Array:length - 1 {
	    // We don't check "st".  We allow "destroy messages" to kill already killed time segments.
	    // [2007.07.01] JUST FIXED A BUG, We previously were trying to subseg data that wasn't there.
	    killuntil = max(accs[j]`start, en + 1`gint); // Make sure we don't try to subseg before the start.
	    //killuntil = en+1; // This was broken...	    
	    accs[j] := subseg(accs[j], killuntil, int64ToInt(accs[j]`end - killuntil) + 1);
	  };
	};
	requests := requests`tail;
      }
    }
  }
}


syncN_no_delete = fun (ctrl, strms) { Internal:syncN_aux(ctrl, strms, false) }



} // End namespace






src = COUNTUP(30);
s1 = window(src, 10);
g = iterate x in s1 {
  state { b = false }
  b := not(b);
  if b then emit x;
}

using Gapped;
//BASE <- markgaps(g)
//BASE <- rewindow(markgaps(g), 5, 0)
//BASE <- Gapped:Internal:rewindow(markgaps(s1), 100, 50);
//BASE <- rewindow(markgaps(s1), 100, -50);
//BASE <- markgaps(s1);

BASE <- rewindow(markgaps(s1), 100, 50);



