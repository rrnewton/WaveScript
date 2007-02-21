 
/*
type Maybe a = None | Some a
type Union3 (a, b, c) = Firstof3  a 
                      | Secondof3 b
                      | Thirdof3  c

iterate (x in S) {
  case x of
    | Firstof3 n -> f(n)
    | other -> other
}

*/

sync2 :: ((Stream (Bool * Int * Int)), 
          (Stream (Sigseg Float)), 
          (Stream (Sigseg Float)))
     ->   (Stream (Sigseg Float * Sigseg Float));
fun sync2 (ctrl, s1, s2) {
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
    
    if (acc1 != nullseg) then  print("  Acc1: " ++ show(acc1.start) ++ ":" ++ show(acc1.end) ++ "\n");
    if (acc2 != nullseg) then  print("  Acc2: " ++ show(acc2.start) ++ ":" ++ show(acc2.end) ++ "\n");
    
    // Now we see if we can process the next request.
    if requests == []
    then {}
    else {
      let (fl, st, en) = requests.head;
      if (acc1 != nullseg &&  	  acc2 != nullseg &&
	  acc1.start <= st && 	  acc2.start <= st &&
	  acc1.end >= en &&	  acc2.end >= en)
	then {
	print("  Spit out segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	size = en - st + 1; // Start/end is inclusive.
	emit (subseg(acc1, st, size),
	      subseg(acc2, st, size));
	acc1 := subseg(acc1, st + size, acc1.width - size);
	acc2 := subseg(acc2, st + size, acc2.width - size);

	// The request is serviced.
	requests := requests.tail;
      }
    }

    //    emit(nullseg, nullseg);
  }
}


//======================================================================
// QUERY:

//// Our current implementation of unionList works poorly for uneven
//// input and output window sizes.  The problem is that the
//// "requests" are currently fixed to come at the same rate as the
//// new data.

// run ./get_sample_data first
chans = (dataFile("6sec_marmot_sample.raw", "binary", 44000, 0) :: Stream (Int * Int * Int * Int));
ch1 = window(iterate((a,_,_,_) in chans){ emit intToFloat(a) }, 128);
ch2 = window(iterate((_,b,_,_) in chans){ emit intToFloat(b) }, 128);

outwidth = 100;

ctrl = iterate(w in ch1) {
  state { pos = 0; }    
  emit(true, pos, pos + outwidth - 1);
  pos := pos + outwidth;
};

BASE <- sync2(ctrl, ch1, ch2);


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












//======================================================================
//SCRATCH:

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
