
NORMALMEAN = 0.329;
NORMALSTD  = 0.1;
LEAKMEAN   = 0.215;
LEAKSTD    = 0.034;
E          = 2.7182818284590452354;

// ASK: array of array syntax; timebase stuff; is there an apply?; are there lambdas?;
//      args to subseg?

/*
 *
 */
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
fun zip(s1, s2)
{
   let slist = [s1, s2];
   iterate ((i, x) in unionList(slist))
   {
      state {
         buf1 = []; // using list for poor-man's Maybe type.
         buf2 = [];
      }

      if (i == 0) then
      {
         buf1 := [x]; // Might throw out elemnt.
      }
      else if (i == 1) then
      {
         buf2 := [x];
      }
      else
      {
         wserror("implementation error");
      };

      if (buf1.listLength == 1 && buf2.listLength == 1) then
      {
         emit (buf1.head, buf2.head);
         buf1 := [];
         buf2 := [];
      };
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
   
   for i=1 to outputLevel-2
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
   last = outputLevel-2;
   currLen := currLen / 2;
   
   outCoefs = makeArray(currLen, 0.0);
   valArr_last = valArrs[last];
   for j=0 to currLen-1
   {
      outCoefs[j] := (valArr_last[j*2] -. valArr_last[j*2+1]) /. 2.0;
   };

   outCoefs;
}


/*
 *
 */
fun trimpeak(stream, comp)
{
   iterate (w in stream)
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

      emit (supVal, subseg(w, w.start + supInd, w.end - supInd));
   };
}


/*
 * FIXME: probaly just temporary; in lieu of a built-in absf
 */
absf :: Float -> Float;
fun absf(x)
{
   if (x < 0.0) then 0.0 -. x
   else                     x;
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
   scalingFactor = 4.0;

   outBuf = haar_calc(to_array(w), 4); // FIXME: move this to_array() into haar_calc() !!!
   for i=0 to outBuf.length-1
   {
      outBuf[i] := outBuf[i] *. scalingFactor;
   };


   emit to_sigseg(outBuf, 0, outBuf.length-1, w.timebase);
}

tpk1 :: Signal (Float, Sigseg Float);
tpk1 = trimpeak(wlt, fun(a,b) { a < b });

filter1 = iterate ((m,w) in tpk1)
{
   // FIXME: do PeakTrimFilter()
   emit w;
}

tpk2 = trimpeak(filter1, fun(a,b) { a > b });


filter2 = iterate ((m,w) in tpk2)
{
   // FIXME: do PeakTrimFilter()
   emit w;
}


tpk3 = trimpeak(filter2, fun(a,b) { a < b });

filter3 = iterate ((m,w) in tpk3)
{
   // FIXME: do PeakTrimFilter()
   emit w;
}


tpk4 = trimpeak(filter3, fun(a,b) { a > b });


//blah = unionList([tpk1, tpk4]);

//detect = iterate(((m1,w1), (m2,w2)) in unionList([tpk1, tpk4]))
detect = iterate(((m1,w1), (m2,w2)) in zip(tpk1, tpk4))
{
   // FIXME: do LeakDetect(NORMALMEAN, NORMALSTD, LEAKMEAN, LEAKSTD)
   peakRatio = m1 /. m2;

   print("peakRatio: " ++ show(peakRatio));

   emit(absf(peakRatio) < 0.32);
}

/*

iterate (w in detect)
{
   // FIXME: do PrintBox(" LeakDetect ")
}
*/


BASE <- tpk1;
//BASE <- wlt;
