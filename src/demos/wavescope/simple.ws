

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
      startsamp := startsamp + len`intToInt64;
    }
  };

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
      go = false; // Temp 
    }

    acc := joinsegs(acc, win);
    //print("Acc "++show(acc`start)++":"++show(acc`end)++" need_feed "++show(need_feed)++"\n");

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


s0 = timer(3.0);

s1 = window(s0, 30);

s2 = rewindow(s1, 11, 0);

BASE <- iterate x in s2 {
  print("woot\n");
  print(x);
  print("\n");
  emit x;
}
