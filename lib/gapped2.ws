
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
      /*
      acc := joinsegs(acc, win);
      wid = acc`width - owed_gap;
      // NEGATIVE GAP
      number_ready = 
	if wid < newwidth then 0 else (wid - newwidth) / feed;

      for i = 0 to number_ready-1 {
	emit subseg(acc, owed_gap + i*feed, newwidth)
      };
      acc := subseg(acc, owed_gap + number_ready * feed, ??);      
      */
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





/*
        // Send out the remainder even though it may not be the right size.
	emit acc;
	if acc != nullseg then emit nullseg;
        acc := nullseg;
	owed_gap :=
*/



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







  /*

     
   iterate (win in sig) {
    state { 
      acc = nullseg; 
      // Sometimes we still owe some "gap" samples from last time:
      owed_gap = 0;
      //      go = false; // Temp 
    }

    // IF NOT NULLSEG
    {
      acc := joinsegs(acc, win);
      wid = acc`width - owed_gap;
      // NEGATIVE GAP
      number_ready = 
	if wid < newwidth then 0 else (wid - newwidth) / feed;

      for i = 0 to number_ready-1 {
	emit subseg(acc, owed_gap + i*feed, newwidth)
      };
      acc := subseg(acc, owed_gap + number_ready * feed, ??);      
    }






   // This is INEFFICIENT!  We don't need to do this many subseg operations:
   go := true;
   while go {
     if need_feed then {
       if acc`width > gap // here we discard a segment:
       then {acc := subseg(acc, acc`start + gap`intToInt64, acc`width - gap);
	     need_feed := false; }
       else go := false
      } else {
	if acc`width > newwidth
	then {emit subseg(acc, acc`start, newwidth);
	      if gap > 0 
	      then { 
		acc := subseg(acc, acc`start + newwidth`intToInt64, acc`width - newwidth);
		need_feed := true; 
	      } else acc := subseg(acc, acc`start + feed`intToInt64, acc`width - feed);
	} else go := false
      }
   }
  }
}

s1 = window(timer(3.0), 10);

BASE <- rewindow(s1, 5, 0)


  */
