/***********************************************/
/*   The WaveScript Standard Library.          */
/*                                             */
/* - Ryan Newton & Lewis Girod                 */
/***********************************************/

// This file exports the following bindings.
// Here are their types and some brief documentation.

/// Constants:

const_PI   :: Float;
const_PIO2 :: Float;
const_E    :: Float;

///  Library POD (plain old data) functions: 

println    :: String -> ();

  // Some additional math functions 
sqr        :: #n -> #n;
atan2      :: (Float, Float) -> Float;
deg2rad    :: Float -> Float;
rad2deg    :: Float -> Float;
expF       :: Float -> Float;
expC       :: Complex -> Complex;
gaussian   :: (Float, Int) -> Array Float;

/// Library stream constructors:

   // Should be "stream_fft" for consistency?
fftStream  :: Stream (Array Float) -> Stream (Array Complex);
sigseg_fftC    :: Sigseg Complex -> Sigseg Complex;
sigseg_ifftC   :: Sigseg Complex -> Sigseg Complex;
sigseg_fftR2C  :: Sigseg Float   -> Sigseg Complex;
sigseg_ifftC2R :: Sigseg Complex -> Sigseg Float;

type CtrlStrm = Stream (Bool * Int * Int);
type SegStream t = Stream (Sigseg t);

type S t   = Stream t;
type SS t  = Stream (Sigseg t);
type LSS t = List   (Stream (Sigseg t));
type SLS t = Stream (List   (Sigseg t));

CONST           :: t -> S t;
  // This lets you eavesdrop on a stream while passing all data through:
snoop           :: (a, Stream b) -> Stream b;

zip2_sametype   :: (Stream t, Stream t)           -> Stream (t * t);
zip3_sametype   :: (Stream t, Stream t, Stream t) -> Stream (t * t * t);

//union2          :: (Stream a, Stream b) -> Stream (Union2 a b);

syncN           :: (CtrlStrm, LSS t)       -> SLS t;
syncN_no_delete :: (CtrlStrm, LSS t)       -> SLS t;
thresh_extract  :: (SS  Float, LSS t, Float, Int) -> SLS t;

  // This takes an unwindowed stream and produces a stream of sigsegs:
window          :: (Stream t, Int) -> SS t;

  // Don't change the data, but redo the windowing:
rewindow        :: (SS t, Int, Int) -> SS t;
makeHanning     :: Int      -> Array Float;
  // Taper the edges of the (probably overlapping) windows.
hanning         :: SS Float -> SS Float;

stream_map       :: (a -> b,    Stream a) -> Stream b;
stream_filter    :: (t -> Bool, Stream t) -> Stream t;
stream_iterate   :: ((a, st) -> (List b * st), st, Stream a) -> Stream b;

sigseg_map       :: (a -> b, Sigseg a) -> Sigseg b;
deep_stream_map  :: (a -> b, SS a) -> SS b;
//deep_stream_map2 :: (a -> b, SS b) -> ()


i2f :: Int -> Float;
i2c :: Int -> Complex;
f2i :: Float -> Int;
f2c :: Float -> Complex;
c2i :: Complex -> Int;
c2f :: Complex -> Float;

smap    :: (a -> b)    -> S a -> S b;
sfilter :: (t -> Bool) -> S t -> S t;

Array:fold1     :: ((t, t) -> t, Array t) -> t;
Array:foldRange :: (Int, Int, t, (t, Int) -> t) -> t;
Array:copy      :: Array t -> Array t;

// These aren't at their final names.  They'll be moved into the Array
// namespace or discarded.
amap_inplace    :: (t -> t, Array t) -> Array t;
amap            :: (a -> b, Array a) -> Array b;
afold           :: ((b, a) -> b,  b,  Array a) -> b;
asum            :: Array #n -> #n;
amult_scalar    :: (Array #n, #n) -> Array #n;
amult_scalar_inplace :: (Array #n, #n) -> Array #n;
apairmult       :: (Array #n, Array #n) -> Array #n;
apairsum        :: (Array #n, Array #n) -> Array #n;
adot            :: (Array #n, Array #n) -> #n;
a_max           :: Array #n -> (#n * Int);
a_zeroes        :: Int -> Array #n;
a_ones          :: Int -> Array #n;

  // Side-effecting insertion sort:
sort            :: ((Int, Int) -> (), 
                    (Int, Int) -> Int, 
                    Int) -> ();

// These bindings are PRIVATE.  They are exported (just because we
// don't have a proper module system), but DON'T USE THEM!
   syncN_aux       :: (CtrlStrm, LSS t, Bool) -> SLS t;
		    
// NEED TO ADD:
// qsort, Complexl, expc, atan2, MInv...






/// Here's the IMPLEMENTATIONS:

//======================================================================
// Constant:
const_PI   = 3.141592653589793;
const_PIO2 = const_PI/2.0;
const_E    = 2.718281828459045;

//======================================================================
// Library POD (plain old data) functions:

fun println(s) {
  print(s);
  print("\n");
};


fun sqr(x) { x*x }

fun atan2(arg1,arg2) {
  if (arg1+arg2) == arg1 then {
    if arg1 >= 0.0 then const_PIO2
    else -1.0 * const_PIO2
  }
  else {
    tmp = atan(arg1/arg2);
    if arg2 < 0.0 then {
      if tmp <= 0.0 then (tmp + const_PI)
      else (tmp - const_PI)
    }
    else tmp
  }
}

fun deg2rad(deg) { deg * const_PI / 180.0 }
fun rad2deg(rad) { rad * 180.0 / const_PI }

fun expF(f) { const_E ^. f }
fun expC(c) { floatToComplex(const_E) ^: c }

fun fftStream(s) {
  iterate f in s { emit fftR2C(f) }
}

fun gaussian(sigma,size) {
  mu = intToFloat(size)/2.0;
  coeff = 1.0/(sigma*sqrtF(2.0*const_PI));
  Array:build(size, 
	      fun (i) 
	      coeff*expF(0.0-sqr(intToFloat(i)-mu)/(2.0*sqr(sigma))));
}

// For completeness we include these restrictions of their generic counterparts:
//fun intToFloat     (i::Int)   toFloat(i) 
//fun intToComplex   (i::Int)   toComplex(i) 
//fun floatToComplex (f::Float) toComplex(f) 

// Pretend to keep timebase, but this is wrong:
fun sigseg_fftC   (ss) toSigseg(ss`toArray`fftC,    ss.start, ss.timebase)
fun sigseg_ifftC  (ss) toSigseg(ss`toArray`ifftC,   ss.start, ss.timebase)
fun sigseg_fftR2C (ss) toSigseg(ss`toArray`fftR2C,  ss.start, ss.timebase)
fun sigseg_ifftC2R(ss) toSigseg(ss`toArray`ifftC2R, ss.start, ss.timebase)


//======================================================================
// "Library" stream constructors:

fun CONST(x) 
  iterate _ in timer(1000.0) {
    emit x
  }

fun snoop(str, strm) {
  iterate (x in strm) {
    println( str ++ show(x) );
    emit x;
  }
}

zip2_sametype = fun (s1,s2) {
  slist = [s1,s2];
  iterate((ind, seg) in unionList(slist)) {
    state {
      s1 = [];
      s2 = [];
    }
  
    if (ind == 0) then {
      s1 := List:append(s1,[seg]);
    }
    else if (ind == 1) then {
      s2 := List:append(s2,[seg]);
    }
    else wserror("implementation error: got ind "++ show(ind));

    if (s1 != [] && s2 != []) then {
      emit(List:head(s1), List:head(s2));
      s1 := tail(s1);
      s2 := tail(s2);
    }
  }
}

zip3_sametype = fun (s1,s2,s3) {
  slist = [s1,s2,s3];
  iterate((ind, seg) in unionList(slist)) {
    state {
      s1 = [];
      s2 = [];
      s3 = [];
    }
  
    if (ind == 0) then {
      s1 := List:append(s1,[seg]);
    }
    else if (ind == 1) then {
      s2 := List:append(s2,[seg]);
    }
    else if (ind == 2) then {
      s3 := List:append(s3,[seg]);
    }
    else wserror("implementation error: got ind "++ show(ind));

    if (s1 != [] && s2 != [] && s3 != []) then {
      emit(List:head(s1), List:head(s2), List:head(s3));
      s1 := tail(s1);
      s2 := tail(s2);
      s3 := tail(s3);
    }
  }
}




syncN_aux = 
 //if IS_SIM then __syncN else
fun (ctrl, strms, del) {
   DEBUGSYNC = false; // Activate to debug the below code:

  _ctrl = iterate((b,s,e) in ctrl) { emit (b,s,e, nullseg); };
  f = fun(s) { iterate(win in s) { emit (false,0,0, win); }; };
  _strms = map(f, strms);  
  slist = _ctrl ::: _strms;  

   //  if DEBUGSYNC then print("Syncing N streams (including ctrl stream): " ++ show(slist`List:length) ++ "\n");

  iterate((ind, tup) in unionList(slist)) {
    state {
      accs = Array:make(slist`List:length - 1, nullseg);
      requests = [];
    }
    
    // Debugging helper functions:
    fun printaccs() {
      for i = 0 to accs`Array:length - 1 {
	if accs[i] == nullseg
	then print("null  ")
	else print(show(accs[i] `start) ++ ":" ++ show(accs[i] `end) ++ "  ");
      }
    };
    fun printwidths(){
      for i = 0 to accs`Array:length - 1 {
	if accs[i] == nullseg
	then print("0   ")
	else print(show(accs[i]`width) ++ " ");
      }
    };

    //if DEBUGSYNC then { print("SyncN  Current ACCS: "); printaccs(); print("\n") };
    //if DEBUGSYNC then { print("SyncN  ACC widths: "); printwidths(); print("\n") };
    if DEBUGSYNC then { print("SyncN ACCS: "); printaccs(); print("    "); printwidths(); print("  tag value "++show(ind)); print("\n") };

    let (flag, strt, en, seg) = tup;
    // Process the new data:
    if ind == 0 // It's the ctrl signal.
    then requests := append(requests, [(flag,strt,en)])
    else accs[ind-1] := joinsegs(accs[ind-1], seg);        
    // Now we see if we can process the next request.
    if requests == []
    then {} // Can't do anything yet...
    else {
      let (fl, st, en) = requests`head;

      allready =
	Array:andmap(
	 fun (seg)
	 if (seg == nullseg ||
	       (fl && seg`start > st) || // This only matters if we're retaining it.
	       seg`end < en)
	   then { 		       
	     if DEBUGSYNC then {
	       if (seg == nullseg) then
		 println("  Not all ready: NULL")
	       else
		 println("  Not all ready: "
			  ++ show(fl && seg`start > st) ++ " "
			  ++ show(seg`end < en) ++ " " 
	                  ++ ((st,en,seg`start,seg`end)));
	     };
             
             if (seg != nullseg && fl && seg`start > st) then {
               println("Sync has a request that can never be filled:");
	       println("Accumulator has "++seg`start++" to "++
		       seg`end++", but request is for "++
		       st++" to "++en++".");
	       wserror("Impossible sync request");
	     };

	     false }
  	   else true,
	 accs);

      // The data is ready on all buffers, now it's time to either discard or output it.
      if allready then {
	if fl then {
	  if DEBUGSYNC 
	  then print("SyncN: Output segment!! " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	  size = en - st + 1; // Start,end are inclusive.
  	  emit List:map(fun (seg) subseg(seg,st,size), Array:toList(accs))
	} else {
	  if DEBUGSYNC then
	  print("SyncN: Discarding segment: " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	};

	if (del || not(fl)) then {
	  // In either case, destroy the finished portions and remove the serviced request:
	  for j = 0 to accs`Array:length - 1 {
	    // We don't check "st".  We allow "destroy messages" to kill already killed time segments.
	    accs[j] := subseg(accs[j], en + 1, accs[j]`end - en);
	  };
	};
	requests := requests`tail;
      }
    }
  }
}

syncN = fun (ctrl, strms) { syncN_aux(ctrl, strms, true) }

syncN_no_delete = fun (ctrl, strms) { syncN_aux(ctrl, strms, false) }


fun thresh_extract(search,streamlist,thresh,pad) {

  ctrl = iterate (w in search) {
    state {
      skiptill = pad;
    }

    //println("in thresh, w.start " ++ w.start);

    for i = 0 to w.width-1 {
      if ((w.start+i) > skiptill && absF(w[[i]]) > thresh) then {
        //println("about to emit t, " ++ w.start + i - pad ++ " " ++ w.start + i + pad - 1);
    	emit (true, w.start + i - pad, w.start + i + pad - 1);
        skiptill := w.start + i + pad;
      }
    };
    
    if (w.start+w.width-pad-1 > skiptill) then {
      //println("about to emit f, " ++ w.start + w.width - pad - 1);
      emit (false, 0, w.start + w.width - pad - 1);
    }
  };

  syncN(ctrl,streamlist);
}


fun window(S, len) 
  iterate(x in S) {
    state{ 
      arr = Array:null;
      ind = 0; 
      startsamp = 0;
    }
    if ind == 0 then arr := Array:make(len, x);
    arr[ind] := x;
    ind := ind + 1;
    if ind == len
    then {
      emit toSigseg(arr, startsamp, nulltimebase);
      ind := 0;
      arr := Array:make(len, x); 
      startsamp := startsamp + len;
    }
  };


// This version is enhanced to allow large steps that result in gaps in the output streams.
//   GAP is the space *between* sampled strips, negative for overlap!
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


fun makeHanning(size) {
  Array:build(size, 
	      fun (i) 
	      0.5 * (1.0 - cos(2.0 * const_PI * intToFloat(i) / intToFloat(size-1))))

// RRN: This has the problem that the hanning coefficient is ZERO at
// the first and last element in the window.  These represent wasted samples.
// 0.5 * (1.0 - cos(2.0 * const_PI * intToFloat(i+1) / intToFloat(size+1))))
}

fun hanning (strm) {
  iterate(win in strm) {
    state{ 
      _lastLen = 0;
      _hanning = Array:null;
    }

    if _lastLen != win`width then {
      _lastLen := win`width;
      _hanning := makeHanning(_lastLen);
    };

    /* alloc buffer */
    buf = Array:make(_lastLen, 0.0);
    for i = 0 to _lastLen - 1 {
      buf[i] := _hanning[i] *. win[[i]];
    }
    
    //print("\nWIN: "++ show(win)++"\n");
    //print("\nHAN: "++ show(_hanning)++"\n");
    //print("\nBUF: "++ show(buf)++"\n");

    emit toSigseg(buf, win`start, win`timebase);
  }
}


//======================================================================
// Higher order routines which should have built-in support at some point.

fun stream_map(f,s) {
  iterate (x in s) {
    emit f(x);
  }
}

fun stream_filter(f,s) {
  iterate (x in s) {
    if f(x) then emit x
  }
}

fun stream_iterate(f,z,s) {
  iterate (x in s) {
    state { sigma = z }
    let (ls,sig2) = f(x,sigma);
    sigma := sig2;
    // list_foreach(fun(t) emit t, ls);
    for i = 0 to ls`List:length-1 {
      emit List:ref(ls,i);
    }
  }
}

fun sigseg_map (f, ss) {
  arr = Array:build(ss.width, fun(i) f(ss[[i]]));
  toSigseg(arr, ss.start, ss.timebase)
}

// This doesn't create a shared structure:
fun deep_stream_map(f,sss)
  stream_map( fun(ss) sigseg_map(f,ss), sss);


// UNFINISHED:
// Assumes in-order but possibly overlapping
//deep_stream_map2 :: ((a -> b), Stream (Sigseg a)) -> Stream (Sigseg b);
fun deep_stream_map2(f,sss) {
  iterate(ss in sss) {
    state { 
      pos = 0;
      lastout = nullseg;
    }
    first = f(ss[[0]]);
    output = Array:make(ss`width, first);
    if pos > ss`start then
      for i = 0 to pos - ss`start - 1 {
	// Copy old result:
	output[i] := lastout[[i + (ss`start - lastout`start)]];
      }	     
  };
  strm = rewindow(sss,100,0);
}


//======================================================================
// Useful aliases:

i2f = intToFloat;
i2c = intToComplex;
f2i = floatToInt;
f2c = floatToComplex;
c2i = complexToInt;
c2f = complexToFloat;

// These are the "advanced" versions.  They're curried.
smap    = fun(f) fun(x) stream_map(f,x);
sfilter = fun(f) fun(x) stream_filter(f,x);
//amap = 


//======================================================================
/* array operations */

/* We use this to post-facto add things into the built-in array namespace. */
namespace Array {
  
  fun fold1 (f,arr) {
    if arr == null
    then wserror("Array:fold1 - array must have at least one element!")
    else {
      // There's no efficient way to do this currently.
      // Don't want to copy the array.
      let (_,result) = 
        Array:fold(fun((bit,acc), x) 
		     if bit then (true, f(acc,x)) else (true, acc),
		   (false, arr[0]), arr);
      result
    }
  }

  // This is quite inefficient.  But if it's a useful thing to have it
  // can be made efficient.  Range is inclusive.
  fun foldRange (st, en, zer, f) {
    fold(f, zer, build(en - st + 1, fun(x) x+st))
  }

  fun copy(arr) Array:build(arr`length, fun(i) arr[i]);
  
}

// RRN: NOTE: These should be added to namespace Array:

// Shall we call side effecting versions amapIO ?
fun amap_inplace(f, arr) {
  for i = 0 to arr`Array:length - 1 {
    arr[i] := f(arr[i]);
  }
  arr
}

// TEMP:
amap = Array:map;
afold = Array:fold;

fun asum(arr) { afold((+), gint(0), arr) }
fun amult_scalar(arr,s) { amap(fun (x) x*s, arr) }
fun amult_scalar_inplace(arr,s) {
  fun ms(a) { a*s };
  amap_inplace(ms, arr)
}

fun apairmult(arr1,arr2) {
  Array:build(arr1`Array:length, 
	      fun (i) arr1[i] * arr2[i])
}

fun apairsum(arr1,arr2) {
  Array:build(arr1`Array:length,
              fun (i) arr1[i] + arr2[i])
}


fun adot(arr1,arr2) {
  // rrn: This is pretty unnatural:
  let (_,sum) = Array:fold(fun ((i,acc), x) 
			     (i+1, acc + (x * arr2[i])),
			   (0,gint(0)), arr1);
  sum
}

fun a_max(arr) {
  // TODO: Check for null array!
  let (i,mx,mxi) = 
   Array:fold( fun ((i,mx,mxi), n)
 	        if n > mx 
   	        then (i+1, n,i)
	        else (i+1, mx,mxi),
	       (0,arr[0],0), arr);
  (mx,mxi)
}

fun a_zeroes(len) { Array:make(len, gint(0)) }
fun a_ones(len) { Array:make(len, gint(1)) }

fun sort(swap, cmp, len) {
  for j = 0 to len-1 {
    for i = 0 to len-2 {
      if (cmp(i,i+1) > 0) then {
        swap(i,i+1);
      }
    }
  }
}


/* test1 = stream_map(fun(w) w[[0]], audio(0,1024,0)); */
/* test2 = stream_filter(fun (n) n > 300.0, test1); */
/* test3 = stream_iterate(fun (x,st) ([x +. st, 5.0, 6.0], st +. 100.0), */
/* 		       0.0, test2); */
/* test4 = deep_stream_map(fun(x) x /. 100.0, audio(0,10,0)) */
/* BASE <- test4; */

//BASE <- audio(0,10,0);


// Some standard type definitions.

uniontype Union2 a b       = Oneof2 a | Twoof2 b;
uniontype Union3 a b c     = Oneof3 a | Twoof3 b | Threeof3 c;
uniontype Union4 a b c d   = Oneof4 a | Twoof4 b | Threeof4 c | Fourof4 d;
uniontype Union5 a b c d e = Oneof5 a | Twoof5 b | Threeof5 c | Fourof5 d | Fiveof5 e;


fun union2(s1,s2) 
  merge(stream_map(Oneof2,s1),
	stream_map(Twoof2,s2))

fun union3(s1,s2,s3) 
  merge(stream_map(Oneof3,  s1),
  merge(stream_map(Twoof3,  s2),
        stream_map(Threeof3,s3)))

fun union4(s1,s2,s3,s4) 
  merge(stream_map(Oneof4,   s1),
  merge(stream_map(Twoof4,   s2),
  merge(stream_map(Threeof4, s3),
        stream_map(Fourof4,  s4))))

fun union5(s1,s2,s3,s4,s5) 
  merge(stream_map(Oneof5,   s1),
  merge(stream_map(Twoof5,   s2),
  merge(stream_map(Threeof5, s3),
  merge(stream_map(Fourof5,  s4),
        stream_map(Fiveof5,  s5)))))



/*

union Choose2 (a, b)          = OneOf2 a | TwoOf2 b
union Choose3 (a, b, c)       = OneOf3 a | TwoOf3 b | ThreeOf3 c
union Choose4 (a, b, c, d)    = OneOf4 a | TwoOf4 b | ThreeOf4 c | FourOf4 d
union Choose5 (a, b, c, d, e) = OneOf5 a | TwoOf5 b | ThreeOf5 c | FourOf5 d | FiveOf5 e


*/
