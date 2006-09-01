

fun marmotscore(w) { 3.8 }

fun rewindow(sig, newwidth, step) {
  if step >= newwidth
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


fun sync4 (ctrl, s1, s2, s3, s4) {
  // A lame sort of manual union type.  Pad all streams out with all fields:
  _ctrl = iterate((b,s,e) in ctrl) { emit (b,s,e, nullseg); };
  _s1   = iterate(win in s1) { emit (false,0,0, win); };
  _s2   = iterate(win in s2) { emit (false,0,0, win); };  

  // Now it's homogenously typed.
  slist = [ _ctrl, _s1 , _s2];  

  iterate((ind, tup) in unionList(slist)) {
    state {
      acc1 = nullseg;
      acc2 = nullseg;
      requests = [];
    }
    let (flag, strt, en, seg) = tup;

    // Process the new data:
    if ind == 0 // It's the ctrl signal.
    then requests := append(requests, [(flag,strt,en)])
    else if ind == 1
    then acc1 := joinsegs(acc1, seg)
    else acc2 := joinsegs(acc2, seg);
    
    // Now we see if we can process the next request.     
    if requests == []
    then {}
    else {
      let (fl, st, en) = requests.head;
      if (acc1 != nullseg  &&  	  acc2 != nullseg   &&    acc3 != nullseg  &&  	  acc4 != nullseg &&
	  acc1.start <= st && 	  acc2.start <= st  &&    acc3.start <= st &&     acc4.start <= st && 
	  acc1.end >= en   &&	  acc2.end >= en    &&	  acc3.end >= en   &&	  acc4.end >= en)
      then {

	size = en - st + 1; // Start & end are inclusive.
	emit (subseg(acc1, st, size),
	      subseg(acc2, st, size),
	      subseg(acc3, st, size),
	      subseg(acc4, st, size));
	acc1 := subseg(acc1, st + size, acc1.width - size);
	acc2 := subseg(acc2, st + size, acc2.width - size);

	// The request is serviced.
	requests := requests.tail;
      }
    }
  }
}






//========================================
// Main query:

ch1 = audio(0, 4096, 0);
ch2 = audio(1, 4096, 0);
ch3 = audio(2, 4096, 0);
ch4 = audio(3, 4096, 0);

