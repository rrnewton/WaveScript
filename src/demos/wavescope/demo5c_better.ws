
// UNFINISHED

BASE <- rewindow((readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) 
                 :: Stream (Sigseg Int)), 10, 100);



/* // This version is enhanced to allow large steps that result in gaps in the output streams. */
/* fun rewindow(sig, newwidth, step) { */

/*   fun feed() { */
/*       /\* try feeding forward *\/ */
/*       if (_acc.length() < _feed) goto done; */
/*       _acc.subseg(_feed, _acc.length() - _feed); */
/*       _need_feed = false; */
/*       goto again;   */
/*   } */

/*   fun again() { */
/*       /\* terminate if acc less than window *\/ */
/*       if (_acc.length() < _width) goto done; */
/*       { */
/* 	SigSeg<float> sub = subseg(_acc, 0, _width); */
/* 	emit(sub); */
/*       } */
/*       _need_feed = true; */
/*       //goto feed; // inf loop */
/*   } */

/*   if (step <= 0) */
/*     then wserror("rewindow expets step>=0, received: "++show(step)) */
/*     else  */
     
/*    iterate (win in sig) { */
/*     state {  */
/*       acc = nullseg;  */
/*       // This bool helps to handle an output streams with gaps. */
/*       need_feed = false; */
/*     } */

/*     if !contiguous(acc, win) { */
/*       /\* rrn: this is a dangerous policy, allows poorly formed input: *\/ */
/*       /\* could try to emit remainder with padding.. *\/       */
/*       acc := nullseg; */
/*     } */
    
/*     acc := joinsegs(acc, win); */

/*     /\* didn't feed yet? *\/ */
/*     //if (_need_feed) goto feed; */
/*     //else fallthrough to again. */



/* /\*     for i = 1 to w.width { *\/ */
/* /\*       if acc.width > newwidth *\/ */
/* /\*       then {emit subseg(acc, acc.start, newwidth); *\/ */
/* /\* 	    acc := subseg(acc, acc.start + step, acc.width - step)} *\/ */
/* /\*       else break; *\/ */
/* /\*     } *\/ */

/*   }; */
/* } */
