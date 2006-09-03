
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


fun marmotscore(w) { 3.8 }






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
wscores = iterate (w in freq) { emit(marmotscore(w), w); };

detections = 
  iterate (pr in wscores) {
    state { counter = 0; }
    let (sc,w) = pr;
    emit(true, counter, counter+9);
    counter := counter + 10;
  };

synced = syncN(detections, [ch1, ch2, ch3, ch4]);

// Currently the output is CRUDE, and incomplete.  It basically just
// gives you synced windows so that you know syncN is working.  The
// detector's not implemented, marmotscore is not implemented, and
// hanning windows are not currently implemented either.
BASE <- synced;
