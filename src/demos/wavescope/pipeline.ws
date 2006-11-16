
NORMALMEAN = 0.329;
NORMALSTD  = 0.1;
LEAKMEAN   = 0.215;
LEAKSTD    = 0.034;
E          = 2.7182818284590452354;

// ASK: array of array syntax; timebase stuff; is there an apply?; are there lambdas?;
//      args to subseg?

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


/*
 *
 */
fun haar_calc(values, outputLevel)
{
   
   valArrs = makeArray(outputLevel, nullarr);
   valArrs[0] := values;
   currLen = values.length;
   
   for i=1 to outputLevel-1
   {
      currLen := currLen / 2;
      valArr_i   = makeArray(currLen, 0.0);
      valArr_im1 = valArrs[i-1];
      for j=0 to currLen-1
      {
         valArr_i[j] := (valArr_im1[j*2] +. valArr_im1[j*2+1]) /. 2.0;
      };
      valArrs[i] := valArr_i;
   };
   last = outputLevel-1;
   

   outCoefs = makeArray(currLen, 0.0);
   valArr_last = valArrs[outputLevel-1];
   for j=0 to currLen-1
   {
      outCoefs[j] := (valArr_last[j*2] -. valArr_last[j*2+1]) /. 2.0;
   };

   outCoefs;
}


/*
 *
 */
//trimpeak : Sigseg Double, (Double, Double -> Bool) -> (Int, 'es)
fun trimpeak(w, comp)
{
   supVal = w[[w.start]];
   supInd = w.start;
   for i=w.start+1 to w.end
   {
      if comp(w[[i]], supVal) then
      {
         supVal := w[[i]];
         supInd := i;
      };
   };

   (supInd, supVal);
}


/*
 *
 */
fun gaussian_likelihood(mean, stddev, peakRatio)
{
   constant = 1.0 /. (stddev *. sqrt(2.0 *. pi));
   exponent = E ^. ( 0.0 -. (peakRatio-.mean)*.(peakRatio-.mean) /. (2.0*.stddev*.stddev) );

   constant *. exponent;
}


source :: Signal (Sigseg Float);
//source = doubleFile("./pipeline1.data", 600, 0);
source = audio(0, 600, 0);

rw :: Signal (Sigseg Float);
rw = rewindow(source, 8192, 500);


pbox = iterate (w in rw)
{
   print("Rewindow : " ++ show(w) ++ "\n");
}


wlt = iterate (w in rw)
{
   // FIXME: do wavelet(outputLevel=4, doScaling=true)
   // scalingFactor = doScaling ? sqrt(pow(2.0, (int)outputLevel)) : 1;
   scalingFactor : Float = 4.0;

   outBuf = haar_calc(to_array(w), 4);
   for i=0 to outBuf.length-1
   {
      outBuf[i] := outBuf[i] *. scalingFactor;
   };
   emit to_sigseg(outBuf, w.start, w.end, w.timebase);
}


tpk1 = iterate (w in wlt)
{
   // FIXME: do Trimpeak(MIN)
   let (minInd, minVal) = trimpeak(w, fun(a,b) { a < b });
   emit (minVal, subseg(w, minInd, w.end - minInd));
}

/*
filter1 = iterate ((m,w) in tpk1)
{
   // FIXME: do PeakTrimFilter()
   emit w;
}


tpk2 = iterate (w in filter1)
{
   maxInd = trimpeak(w, fun(a,b) { a > b });
   emit subseg(w, maxInd, w.end - maxInd); // FIXME: is this right?
}

filter2 = iterate (w in tpk2)
{
   // FIXME: do PeakTrimFilter()
   // FIXME: does this really do nothing?
   emit w;
}


tpk3 = iterate (w in filter2)
{
   // FIXME: do Trimpeak(MIN)
   minInd = trimpeak(w, fun(a,b) { a < b });
   emit subseg(w, minInd, w.end - minInd); // FIXME: is this right?
}

filter3 = iterate (w in tpk3)
{
   // FIXME: do PeakTrimFilter()
   // FIXME: does this really do nothing?
   emit w;
}

tpk4 = iterate (w in filter3)
{
   // FIXME: do Trimpeak(MAX)
   maxInd = trimpeak(w, fun(a,b) { a > b });
   emit subseg(w, maxInd, w.end - maxInd); // FIXME: is this right?
}


detect = iterate(w in zip2(tpk1, tpk4))
{
   // FIXME: do LeakDetect(NORMALMEAN, NORMALSTD, LEAKMEAN, LEAKSTD)
   emit w;
}

iterate (w in detect)
{
   // FIXME: do PrintBox(" LeakDetect ")
}
*/


//BASE <- detect;
BASE <- rw;

