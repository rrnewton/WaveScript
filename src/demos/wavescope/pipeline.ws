
NORMALMEAN = 0.329;
NORMALSTD  = 0.1;
LEAKMEAN   = 0.215;
LEAKSTD    = 0.034;
// FIXME: what are MIN and MAX?

fun rewindow(sig, newwidth, step)
{
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
  };
}


//source = doubleFile("./pipeline1.data", 600, 0);
source = audio(0, 600, 0);

rw = rewindow(source, 8192, 500);


pbox = iterate (w in rw)
{
   print("Rewindow : " ++ show(w) ++ "\n");
}

/*
wlt = iterate (w in rw)
{
   // FIXME: do wavelet(4, true)
}

tpk1 = iterate (w in wlt)
{
   // FIXME: do Trimpeak(MIN)
}

filter1 = iterate (w in tpk1)
{
   // FIXME: do PeakTrimFilter()
}

tpk2 = iterate (w in filter1)
{
   // FIXME: do Trimpeak(MAX)
}

filter2 = iterate (w in tpk2)
{
   // FIXME: do PeakTrimFilter()
}

tpk3 = iterate (w in filter2)
{
   // FIXME: do Trimpeak(MIN)
}

filter3 = iterate (w in tpk3)
{
   // FIXME: do PeakTrimFilter()
}

tpk4 = iterate (w in filter3)
{
   // FIXME: do Trimpeak(MAX)
}

detect = iterate(w in zip2(tpk1, tpk4))
{
   // FIXME: do LeakDetect(NORMALMEAN, NORMALSTD, LEAKMEAN, LEAKSTD)
}

iterate (w in detect)
{
   // FIXME: do PrintBox(" LeakDetect ")
}
*/


//BASE <- detect;
BASE <- rw;

