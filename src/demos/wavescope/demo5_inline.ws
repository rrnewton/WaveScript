


//rewindow : (Signal 'a, int, int) -> Signal 'a;
// rewindow takes a Signal of Sigsegs, a new window-width, and a new step/feed/slide:
/* fun rewindow(s, newwidth, step) { */
/*   if step >= newwidth */
/*   then error("rewindow won't allow the creation of non-contiguous output streams") */
/*   else iterate (w in s) { */
/*    state { acc = nullseg; } */
/*    acc := joinsegs(acc, w); */
/*    // We do this entirely with index numbers, no abstract Time objects are used: */
/*    for i = 1 to w.width { */
/*      if acc.width > newwidth */
/*      then {emit subseg(acc, 0, newwidth); */
/* 	   acc := subseg(acc, step, acc.width - step)} */
/*      else break; */
/* }}} */


//s1 = rewindow(audioFile("./countup.raw", 4096, 0), 1024, 512);
