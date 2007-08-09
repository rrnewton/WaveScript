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

//======================================================================

fun syncN (strms, ctrl) {
  let _ctrl = iterate((b,s,e) in ctrl) { emit (b,s,e, (nullseg :: Sigseg Float)); };
  let f = fun(s) { iterate(win :: Sigseg Float in s) { 
                   emit (false,0,0, (win :: Sigseg Float)); }; };
  let _strms = map(f, strms);

  let slist = _ctrl ::: _strms;
  
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
      let allready = 
	// This should be andmap, not fold:
	Array:fold(fun (bool, seg)
		   (bool               &&
		    not(seg == nullseg) &&
		    not(seg.start > st`intToInt64) &&
		    not(seg.end   < en`intToInt64)),
		   true, accs);
     	
      if allready then {
	if fl then {
	  print("  Spit out segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	  size = en - st + 1; // Start,end are inclusive.

  	  emit List:map(fun (seg) subseg(seg, intToInt64(st), size), Array:toList(accs))

	} else 
	  print(" Discarding segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");

	// Destroy the discarded portions and remove the serviced request:
	for j = 0 to accs.Array:length - 1 {
	  // We don't check "st".  We allow "destroy messages" to kill already killed time segments.
	  accs[j] := subseg(accs[j], intToInt64(en + 1), accs[j].end`int64ToInt - en);
	};
	requests := requests.tail;
      }
    }
  }
}


//======================================================================
// QUERY:

//// Our current implementation of unionList works poorly for uneven
//// input and output window sizes.  The problem is that the
//// "requests" are currently fixed to come at the same rate as the
//// new data.

chans = (readFile("6sec_marmot_sample.raw", "mode: binary", timer(44000.0))
         :: Stream (Int16 * Int16 * Int16 * Int16));
ch1 = window(iterate((a,_,_,_) in chans){ emit int16ToFloat(a) }, 128);
ch2 = window(iterate((_,b,_,_) in chans){ emit int16ToFloat(b) }, 128);

outwidth = 100;

ctrl = iterate(w in ch1) {
  state { 
    pos = 0; 
    flag = false; 
  }
  emit(flag, pos, pos + outwidth - 1);
  pos := pos + outwidth;
  flag := if flag then false else true;
};

BASE <- syncN([ch1, ch2], ctrl);


//======================================================================

/* fun sync2 (s1, s2, ctrl) { */
/*   state { */
/*     acc1 = nullseg; */
/*     acc2 = nullseg; */
/*   }     */

/*   fun handle_exn(appreslt) { */
/*     case appreslt of */
/*       | AppendSuccess win -> win */
/*       | AppendNoncontig   -> error "..."  */
/*       | AppendDiffseries  -> error "..." */
/*   } */
/*   fun prune(win, t1, t2) { */
/*     TODO.... */
/*       // TODO: error handling for failed prune. */
      
/*   } */
/*   case zip3(s1,s2,ctrl) of */
/*     | First3  w1 -> acc1 := handle_exn( append(acc1, w1)); */
/*     | Second3 w2 -> acc2 := handle_exn( append(acc2, w2)); */
/*     | Third3 <t1, t2, flag> -> */
/* 	if (flag) { */
/* 	  // TODO: error handling for if this range is not available on both input streams: */
/* 	  emit <subref(acc1,t1,t2), subref(acc2,t1,t2)>; */
/* 	}  */
/*         acc1 = prune(acc1,t1,t2); */
/* 	acc2 = prune(acc2,t1,t2); */
