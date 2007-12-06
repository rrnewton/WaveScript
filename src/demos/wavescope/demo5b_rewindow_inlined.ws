

// TODO: Enhance this version of rewindow so that it can handle gaps in the output stream.
fun rewindow(sig, newwidth, step) 
  if step > newwidth
  then wserror("rewindow won't allow the creation of non-contiguous output streams")
  else iterate w in sig {
    state { acc = nullseg; }
    acc := joinsegs(acc, w);
    
    while acc.width > newwidth { 
      emit subseg(acc, acc.start, newwidth);
      acc := subseg(acc, acc.start + step`intToInt64, acc.width - step)
    }
  };



main = rewindow((readFile("./countup.raw", "mode: binary  window: 4096", timer(10.0)) 
		  :: Stream (Sigseg Int16)), 1024, 512); 
