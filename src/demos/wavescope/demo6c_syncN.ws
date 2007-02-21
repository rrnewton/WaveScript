
//======================================================================

fun syncN (strms, ctrl) {
  let _ctrl = iterate((b,s,e) in ctrl) { emit (b,s,e, nullseg); };
  let f = fun(s) { iterate(win in s) { emit (false,0,0, win); }; };
  let _strms = map(f, strms);

  let slist = _ctrl : _strms;
  
  // Side effect not allowed in iterate:
  //print("Syncing N streams: " ++ show(slist.listLength) ++ "\n");

  iterate((ind, tup) in unionList(slist)) {
    state {
      accs = makeArray(slist.listLength - 1, nullseg);
      requests = [];
    }
    print("  Current ACCS: ");
    for ii = 0 to accs.length - 1 {
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
      let allready = true;
      for i = 0 to accs.length - 1 {
	if (accs[i] == nullseg ||
	    accs[i].start > st ||
	    accs[i].end < en)
	then allready := false;
      };
     	
      if allready then {
	if fl then {
	  print("  Spit out segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	  size = en - st + 1; // Start,end are inclusive.
	  output = [];
	  for i = 0 to accs.length - 1 {
	    output := subseg(accs[i], st, size) : output;
	  }
	  emit(reverse(output));
	} else 
	  print(" Discarding segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");

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


//======================================================================
// QUERY:

//// Our current implementation of unionList works poorly for uneven
//// input and output window sizes.  The problem is that the
//// "requests" are currently fixed to come at the same rate as the
//// new data.

chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) :: Stream (Int * Int * Int * Int));
ch1 = window(iterate((a,_,_,_) in chans){ emit intToFloat(a) }, 128);
ch2 = window(iterate((_,b,_,_) in chans){ emit intToFloat(b) }, 128);

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
