
// TODO: fix up time bases and we're done!!!!!!!

NORMALMEAN = 0.329;
NORMALSTD  = 0.1;
LEAKMEAN   = 0.215;
LEAKSTD    = 0.034;
E          = 2.7182818284590452354;

pi   = 3.141592653589793;

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
fun rewindow(sig, newwidth, gap) {
  feed = newwidth + gap;

  if (gap <= (0 - newwidth))
    then wserror("rewindow cannot step backwards: width "++ show(newwidth) ++" gap "++show(gap))
    else 
     
   iterate (win in sig) {
    state { 
      acc = nullseg; 
      // This bool helps to handle an output streams with gaps.
      // We have to states, true means we're to "output" a gap next,
      // false means we're to output a sigseg next.
      need_feed = false;
      go = false; // Temp 
    }

    acc := joinsegs(acc, win);
    //print("Acc "++show(acc`start)++":"++show(acc`end)++" need_feed "++show(need_feed)++"\n");

    go := true;
   while go {
     if need_feed then {
       if acc`width > gap // here we discard a segment:
       then {acc := subseg(acc, acc`start + gap, acc`width - gap);
	     need_feed := false; }
       else go := false
      } else {
	if acc`width > newwidth
	then {emit subseg(acc, acc`start, newwidth);
	      if gap > 0 
	      then { 
		acc := subseg(acc, acc`start + newwidth, acc`width - newwidth);
		need_feed := true; 
	      } else acc := subseg(acc, acc`start + feed, acc`width - feed);
	} else go := false
      }
   }
  }
}
*/

/*
 * Using PRIMITIVE version for now:
 */
fun zip2(s1, s2)
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

      if (buf1`List:length == 1 && buf2`List:length == 1) then
      {
         emit (buf1`head, buf2`head);
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
   
   valArrs = Array:make(outputLevel, Array:null);
   valArrs[0] := values;
   currLen = Mutable:ref(values`Array:length);
   
   for i=1 to outputLevel-1
   {
      currLen := currLen / 2;
      valArr_i   = Array:make(currLen, 0.0);
      valArr_im1 = valArrs[i-1];
      for j=0 to currLen-1
      {
         valArr_i[j] := (valArr_im1[j*2] +. valArr_im1[j*2+1]) /. 2.0;
      };
      valArrs[i] := valArr_i;
   };
   last = outputLevel-1;
   currLen := currLen / 2;
   
   outCoefs = Array:make(currLen, 0.0);
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
      supVal = Mutable:ref$ w[[w.start]];
      supInd = Mutable:ref(w.start);
      //      print("START FOR\n");
      //for i=w.start+1 to w.end // This is quite wrong.
      for i = 1 to w.width-1 
      {
	//         print("iter "++i++"\n");
         if comp(w[[i]], supVal) then
         {
            supVal := w[[i]];
            supInd := i ;
         };
      };
      //      print("END FOR\n");

      emit (supVal, subseg(w, supInd + w`start, w`width - supInd));
   };
}

/*
 *
 */
fun trimpeakEmpty(stream, comp)
{
   iterate (w in stream)
   {
      supVal = Mutable:ref$ w[[w.start]];
      supInd = Mutable:ref$ w.start;
      for i=w.start+1 to w.end
      {
         if comp(w[[i]], supVal) then
         {
            supVal := w[[i]];
            supInd := i;
         };
      };

      //emit (supVal, subseg(w, supInd, w`width - (supInd-w.start)));
      emit (w.start,supInd,w.end);
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
   constant = 1.0 /. (stddev *. sqrtF(2.0 *. pi));
   exponent = E ^. ( 0.0 -. (peakRatio-.mean)*.(peakRatio-.mean) /. (2.0*.stddev*.stddev) );

   constant *. exponent;
}

source = (readFile("./pipeline1.data", "mode: text  window: 600 ") :: Stream (Sigseg Float))

rw :: Stream (Sigseg Float);
rw = rewindow(source, 8192, 500);
//rw = rewindow(source, 8192, 500-8192);


pbox = iterate (w in rw)
{
   print("Rewindow : " ++ show(w.start) ++ " to " ++ show(w.end) ++ "\n");
}


wlt :: Stream Sigseg Float;
wlt = iterate (w in rw)
{
   // FIXME: do wavelet(outputLevel=4, doScaling=true)
   // scalingFactor = doScaling ? sqrt(pow(2.0, (int)outputLevel)) : 1;
   scalingFactor = 4.0;

   outBuf = haar_calc(toArray(w), 4); // FIXME: move this to_array() into haar_calc() !!!
   for i=0 to outBuf`Array:length-1
   {
      outBuf[i] := outBuf[i] *. scalingFactor;
   };

   emit toSigseg(outBuf, 0, w.timebase);
};

tpk1 :: Stream (Float * Sigseg Float);
tpk1 = trimpeak(wlt, fun(a,b) { a < b });

filter1 :: Stream Sigseg Float;
filter1 = iterate ((m,w) in tpk1)
{
   // FIXME: do PeakTrimFilter()
   //print("peakValue is " ++ show(m));
   emit w;
}

tpk2 = trimpeak(filter1, fun(a,b) { a > b });


filter2 = iterate ((m,w) in tpk2)
{
   // FIXME: do PeakTrimFilter()
   //print("peakValue is " ++ show(m));

   emit w;
}


tpk3 = trimpeak(filter2, fun(a,b) { a < b });

filter3 = iterate ((m,w) in tpk3)
{
   // FIXME: do PeakTrimFilter()
   //print("peakValue is " ++ show(m));

   emit w;
}


tpk4 = trimpeak(filter3, fun(a,b) { a > b });


//blah = unionList([tpk1, tpk4]);

//detect = iterate(((m1,w1), (m2,w2)) in unionList([tpk1, tpk4]))
detect = iterate(((m1,w1), (m2,w2)) in zip2(tpk1, tpk4))
{
   // FIXME: do LeakDetect(NORMALMEAN, NORMALSTD, LEAKMEAN, LEAKSTD)
   peakRatio = m2 /. m1;

   emit(absf(peakRatio) < 0.32);
}
/**/

/*

iterate (w in detect)
{
   // FIXME: do PrintBox(" LeakDetect ")
}
*/

// This prints the raw scores without the 
scores = iterate ((m1,w1), (m2,w2)) in zip2(tpk1, tpk4) {
   // FIXME: do LeakDetect(NORMALMEAN, NORMALSTD, LEAKMEAN, LEAKSTD)
   emit m2 /. m1
}

//BASE <- iterate w in source {emit w`width};
//BASE <- rw;
//BASE <- wlt;
//BASE <- tpk1
//BASE <- filter1
//BASE <- tpk2
BASE <- scores;
//BASE <- detect;

