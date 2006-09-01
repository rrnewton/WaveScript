

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


fun sync2 (s1, s2, ctrl) {
  state {
    acc1 = nullseg;
    acc2 = nullseg;
  }
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



}




//========================================
// Main query:

ch1 = audio(0, 4096, 0);
ch2 = audio(1, 4096, 0);
ch3 = audio(2, 4096, 0);
ch4 = audio(3, 4096, 0);

