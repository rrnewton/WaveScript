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

println     :: String -> ();
assert      :: (String, Bool) -> ();
assert_eq   :: (String, a,a)  -> ();

fst         :: (a * b) -> a;
snd         :: (a * b) -> b;

  // Some additional math functions 
sqr        :: #n -> #n;
atan2      :: (Float, Float) -> Float;
deg2rad    :: Float -> Float;
rad2deg    :: Float -> Float;
expF       :: Float -> Float;
expC       :: Complex -> Complex;
conjC      :: Complex -> Complex;
gaussian   :: (Float, Int) -> Array Float;
modF       :: (Float, Float) -> Float;

/// Lifted FFT operators:

stream_fftR2C  :: Stream (Array Float) -> Stream (Array Complex);
sigseg_fftC    :: Sigseg Complex -> Sigseg Complex;
sigseg_ifftC   :: Sigseg Complex -> Sigseg Complex;
sigseg_fftR2C  :: Sigseg Float   -> Sigseg Complex;
sigseg_ifftC2R :: Sigseg Complex -> Sigseg Float;


// Extra ADT functions augmenting the compiler builtins.

FIFO:make       ::  Int -> Queue t;
FIFO:empty      ::  Queue t -> Bool;
FIFO:enqueue    :: (Queue t, t) -> ();
FIFO:dequeue    ::  Queue t -> t;

List:filter     :: (a -> Bool, List a) -> List a;
List:map2       :: ((a,b) -> c, List a, List b) -> List c;
List:mapi       :: ((Int,a) -> b, List a) -> List b;
List:foreach    :: (      a -> (), List a) -> ();
List:foreachi   :: ((Int,a) -> (), List a) -> ();
List:fold1      :: ((t, t) -> t, List t) -> t;
List:choplast   :: List t -> (t * List t);

Array:fold1     :: ((t, t) -> t, Array t) -> t;
Array:foldi     :: ((Int, acc, t) -> acc, acc, Array t) -> t;
Array:foldRange :: (Int, Int, t, (t, Int) -> t) -> t;
Array:copy      :: Array t -> Array t;
Array:fill      :: (Array t, t) -> ();
Array:blit      :: (Array t, Int, Array t, Int, Int) -> ();
Array:append    :: (List (Array t)) -> Array t;
Array:sub       :: (Array t, Int, Int) -> Array t;

String:append   :: (String, String) -> String;

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

/// Library stream constructors:

// A flag to the range keep or not, plus start/end sample numbers (inclusive)
type CtrlStrm = Stream (Bool * Int64 * Int64);
type SegStream t = Stream (Sigseg t);

type S t   = Stream t;
type SS t  = Stream (Sigseg t);
type LSS t = List   (Stream (Sigseg t));
type SLS t = Stream (List   (Sigseg t));

CONST           :: t -> S t;
COUNTUP         :: Int -> S Int;

//easyReadFile    :: String -> Stream t;

  // This lets you eavesdrop on a stream while passing all data through:
snoop           :: (a, Stream b) -> Stream b;

zip2_sametype   :: (Stream t, Stream t)           -> Stream (t * t);
zip3_sametype   :: (Stream t, Stream t, Stream t) -> Stream (t * t * t);
zip4_sametype   :: (Int, S t, S t, S t, S t) -> Stream (t * t * t * t);
// Introducing an argument controlling buffer size:
zipN_sametype   :: (Int, List (Stream t)) -> Stream (List t);

//union2          :: (Stream a, Stream b) -> Stream (Union2 a b);

syncN           :: (CtrlStrm, LSS t)       -> SLS t;
syncN_no_delete :: (CtrlStrm, LSS t)       -> SLS t;

// RRN: NOTE: This should be explained or REMOVED:
thresh_extract  :: (SS  Float, LSS t, Float, Int) -> SLS t;

  // This takes an unwindowed stream and produces a stream of sigsegs:
window          :: (Stream t, Int) -> SS t;
dewindow        ::  SS t           -> Stream t;

  // Don't change the data, but redo the windowing:
rewindow        :: (SS t, Int, Int) -> SS t;
deinterleave    :: (Int, Stream t)  -> List (Stream t);
deinterleaveSS  :: (Int, Int, SS t) -> List (SS t);
makeHanning     :: Int      -> Array Float;
  // Taper the edges of the (probably overlapping) windows.
hanning         :: SS Float -> SS Float;

  // Insert explicit nullsegs to represent gaps.
make_gapstream  :: SS t -> SS t;

  // Replay the first element of a stream.
rep_first        :: (Int, Stream a) -> Stream a;

stream_map       :: (a -> b,    Stream a) -> Stream b;
stream_filter    :: (t -> Bool, Stream t) -> Stream t;
stream_iterate   :: ((a, st) -> (List b * st), st, Stream a) -> Stream b;

sigseg_map       :: (a -> b, Sigseg a) -> Sigseg b;
deep_stream_map  :: (a -> b, SS a) -> SS b;
//deep_stream_map2 :: (a -> b, SS b) -> ()



   // shorthands:
i2f :: Int -> Float;
i2d :: Int -> Double;
i2c :: Int -> Complex;
f2i :: Float -> Int;
f2d :: Float -> Double;
f2c :: Float -> Complex;
c2i :: Complex -> Int;
c2f :: Complex -> Float;
c2d :: Complex -> Double;

to64   :: Int -> Int64;
from64 :: Int64 -> Int;

 /// CURRIED versions of functions
// maps, etc

Curry:smap    :: (a -> b)    -> S a -> S b;
Curry:sfilter :: (t -> Bool) -> S t -> S t;



// These bindings are PRIVATE.  They are exported (just because we
// don't have a proper module system), but DON'T USE THEM!
   syncN_aux       :: (CtrlStrm, LSS t, Bool) -> SLS t;
		    
// NEED TO ADD:
// qsort, Complexl, expc, atan2, MInv...





/*=========================================================================================*/
/*                             Here's the _IMPLEMENTATION_                                 */
/*=========================================================================================*/

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

fun fst((a,_)) a
fun snd((_,b)) b

//fun assert(b)      if not(b)    then wserror("Assert failed.");
//fun asserteq(a,b)  if not(a==b) then wserror("Assert failed: "++ a ++" not equal "++ b);

// Named asserts:
fun assert(s,bool)   if not(bool) then wserror("Assert failed in '"++s++"' ");
fun assert_eq(s,a,b) if not(a==b) then wserror("Assert failed in '"++s++"' : "++ a ++" not equal "++ b);

// Some additional math functions.

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
//fun conjC(c) c - (gint(2) * (0.0+1.0i * floatToComplex(imagpart(c))));
fun conjC(c) makeComplex(c`realpart, 0.0 - c`imagpart)

fun modF(f, base) {
  tmp = Mutable:ref(f);
  while tmp > base {
    tmp -= base;
  };
  tmp
}

// This *should* work by caching one or more fftw plans.
fun fftStream(s) {
  iterate f in s { emit fftR2C(f) }
}
// This is the more appropriate name:
stream_fftR2C = fftStream;

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
/* List operations */

namespace List {

  // This is primitive for arrays, but we happen to define it as a
  // library procedure for lists:
  //
  // Unfortunately, with the old static-elaborator, this doesn't work at 
  // meta-program eval time:
  /*
  fun build(n,f) {
    acc = ref([]);
    for i = 0 to n-1 {
      acc := f(i) ::: acc;
    };
    List:reverse(acc);
  }
  */


/*
  fun mapi(f, ls) {
    ind = ref(0);
    ptr = ref(ls);
    acc = ref([]);
    while ptr != [] {
      acc := f(ind, ptr`head) ::: acc;
      ind := ind + 1;
      ptr := ptr ` tail;
    };
    List:reverse(acc)
  }
*/

  // Maybe should be built in...
  fun filter(f, ls) {
    ptr = Mutable:ref(ls);
    acc = Mutable:ref([]);
    while ptr != [] {
      if f(ptr`head)
      then acc := ptr`head ::: acc;
      ptr := ptr ` tail;
    };
    List:reverse(acc)
  }

  fun foreach(f, ls) {
    ptr = Mutable:ref(ls);
    while ptr != [] {
      f(ptr`head);
      ptr := ptr ` tail;
    }
  }
  fun foreachi(f, ls) {
    ind = Mutable:ref(0);
    ptr = Mutable:ref(ls);
    while ptr != [] {
      f(ind, ptr`head);
      ind := ind + 1;
      ptr := ptr ` tail;
    }
  }

  fun mapi(mif, ls) {
    acc = Mutable:ref([]);
    List:foreachi(fun(i,x) acc := mif(i,x) ::: acc, ls);
    List:reverse(acc)
  }

  fun map2(f, ls1, ls2) {
    p1 = Mutable:ref(ls1);
    p2 = Mutable:ref(ls2);
    acc = Mutable:ref([]);
    while p1 != [] {
      if p2 == []
      then wserror("List:map2 arguments of unequal length: "++ls1++" "++ls2++"\n")
      else {
	acc := f(p1`head, p2`head) ::: acc;
	p1 := p1`tail;
	p2 := p2`tail;
      }
    };
    List:reverse(acc)
  }

  // Could define this in either the imperative or functional manner.  
  fun foldi(f, zer, ls) {
    ind = Mutable:ref(0);
    ptr = Mutable:ref(ls);
    acc = Mutable:ref(zer);
    while ptr != [] {
      acc := f(ind, acc, ptr`head);
      ind := ind + 1;
      ptr := ptr ` tail;
    };
    acc
  }
  
  fun fold1 (f,ls) {
    if ls == []
    then wserror("List:fold1 - list must have at least one element!")
    else List:fold(f, ls`head, ls`tail)
  }

  // Truncates the last element of the list, returning that last element and a new list.
  fun choplast(ls) {
    if ls == [] then wserror("List:choplast can't take a null list");
    p1 = Mutable:ref(ls);
    p2 = Mutable:ref(ls`tail);  
    acc = Mutable:ref([]);
    while p2 != [] {
      acc := p1`head ::: acc; 
      p1 := p2;
      p2 := p2`tail;    
    };
    (p1`head, List:reverse(acc))
  }

}


//======================================================================
/* FIFO Queues */


// This is a very inefficient initial implementation.
// Should use circular buffers.
type Queue t = Array (List t);
namespace FIFO {
  fun make(n) Array:make(1,[]);
  fun empty(q) q[0] == [];
  fun enqueue(q,x) q[0] := x ::: q[0];
  fun dequeue(q) {
    let (x,ls) = List:choplast(q[0]);
    q[0] := ls;
    x
  }
}

// [2007.07.29] This array based implementation exposed some problems
// with the meta-evaluation framework.

/*
// Contains start (inclusive) and count
// Because Refs are not first class, we use an array to store the start/count.
type Queue t = (Array t * Array Int);
namespace FIFO {
  fun make(n)   (Array:makeUNSAFE(n), Array:build(2, fun(_) 0));
  fun empty((_,stcnt))  stcnt[1] == 0
  fun enqueue((arr,stcnt), x) {
    len = arr`Array:length;
    if stcnt[1] == len
    then wserror("FIFO:enqueue - queue full!")
    else {
      ind = stcnt[0] + stcnt[1];
      if ind >= len
      then arr[ind-len] := x
      else arr[ind]     := x;
      // Increase the count by one:
      stcnt[1] := stcnt[1] + 1;
    };
  }
  fun dequeue((arr,stcnt)) {
    len = arr`Array:length;
    if stcnt[1] == 0    
    then wserror("FIFO:dequeue - queue empty!")
    else {
      st = stcnt[0];
      if st + 1 == len 
        then stcnt[0] := 0
        else stcnt[0] := st + 1;
      arr[st];
    }
  }
}
*/

//======================================================================
/* Array operations */

/* We use this to post-facto add things into the built-in array namespace. */
namespace Array {

  /*
  fun fromList(ls) {
    arr = Array:makeUNSAFE(ls`List:length);
    p = ref(ls);
    i = ref(0);
    while p != [] {
      arr[i] := p`head;
      p := p`tail;
    };
    arr
    }*/

  // This assumes that there's at least one element in the array and
  // thus doesn't need to be provided with a "neutral element".
  fun fold1 (f,arr) {
    //    if Array:length(arr) == 0
    if arr == Array:null
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

  // Simple wrapper that keeps an extra piece of state to track where
  // we are in the input.  Must be used with fold left!
  fun foldi (f, zer, arr)
    fold(fun ((i,acc), elm) (i+1, f(i,acc,elm)), (0,zer), arr)

  // This is quite inefficient.  But if it's a useful thing to have it
  // can be made efficient.  Range is inclusive.
  fun foldRange (st, en, zer, f) {
    Array:fold(f, zer, Array:build(en - st + 1, fun(x) x+st))
  }

  fun copy(arr) Array:build(arr`Array:length, fun(i) arr[i]);

  fun fill(arr,v) {
    for i = 0 to Array:length(arr) - 1 {
      arr[i] := v;
    }
  }

  // Should maybe call "blockcopy", but that's less fun.
  // Like memcpy... might want to make
  // TODO: add some defense!!  
  fun blit(dst, dstpos, src, srcpos, len) {
    for i = 0 to len - 1 {
      dst[i+dstpos] := src[srcpos+i];      
    }
  }

  // Append a list of arrays.
  fun append(loa) {
    size = List:fold(fun(sz, ar) sz + length(ar), 0, loa);
    newarr = makeUNSAFE(size);
    List:fold(fun (cntr, ar) {
      len = length(ar);
      blit(newarr, cntr, ar, 0, len);
      cntr + len
    }, 0, loa);
    newarr // final result.
  }
  
  // Extract a sub-array.
  fun sub(arr, pos, len)
     build(len, fun(i) arr[pos+i])
  
} // End namespace Array

fun String:append(a,b) a++b

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


//======================================================================
// "Library" stream constructors:

//fun easyReadFile(filename) 
//     readFile(filename, "", timer(1000.0))

fun CONST(x) 
  iterate _ in timer(1000.0) {
    emit x
  }

fun COUNTUP(n)
  iterate _ in timer(1000.0) {
    // Should be Int64:
    state { counter = n }
    emit counter;
    counter += 1;
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

fun zip4_sametype(bufsize, s1,s2,s3,s4) {
  iterate (ind, elem) in unionList([s1,s2,s3,s4]) {
    state { 
      b1 = FIFO:make(bufsize);
      b2 = FIFO:make(bufsize);
      b3 = FIFO:make(bufsize);
      b4 = FIFO:make(bufsize);
    }
    using FIFO;
    if (ind == 0) then enqueue(b1,elem) else
    if (ind == 1) then enqueue(b2,elem) else
    if (ind == 2) then enqueue(b3,elem) else
                       enqueue(b4,elem);
    if not(empty(b1)) && not(empty(b2)) && not(empty(b3)) && not(empty(b4))
    then emit (dequeue(b1), dequeue(b2), dequeue(b3), dequeue(b4))
  }
}

// This currently buffers arbitrarily.
// In the future it should use this bufsize argument to statically
// allocate the buffer.
fun zipN_sametype(bufsize, slist) {
  using List; 
  len = slist`List:length;
  iterate (ind, elem) in unionList(slist) {
    state { bufs = Array:build(len, fun(_) FIFO:make(bufsize)) }
    using FIFO;
    enqueue(bufs[ind], elem);
    if Array:andmap(fun(q) not(empty(q)), bufs)
    then emit List:build(len, fun(i) dequeue(bufs[i]));
  }
}


// This is an internal helper that can be parameterized in two ways to
// form "syncN" and "syncN_no_delete".
syncN_aux = 
fun (ctrl, strms, del) {
   DEBUGSYNC = false; // Activate to debug the below code:

   WARNSKEW = 60000; // threshold for warning that an accumulator has grown to big.  Should be user input.

  _ctrl = iterate((b,s,e) in ctrl) { emit ((b,s,e, nullseg) :: (Bool * Int64 * Int64 * Sigseg any)); };
  f = fun(s) { iterate(win in s) { emit (false,0`gint,0`gint, win); }; };
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
	if accs[i]`width == 0
	then print("null  ")
	else print(show(accs[i] `start) ++ ":" ++ show(accs[i] `end) ++ "  ");
      }
    };
    fun printwidths(){
      for i = 0 to accs`Array:length - 1 {
	if accs[i]`width == 0
	then print("0   ")
	else print(show(accs[i]`width) ++ " ");
      }
    };

    //if DEBUGSYNC then { print("SyncN  Current ACCS: "); printaccs(); print("\n") };
    //if DEBUGSYNC then { print("SyncN  ACC widths: "); printwidths(); print("\n") };
    if DEBUGSYNC then 
    { print("SyncN ACCS: "); printaccs(); print("    "); printwidths(); print("  tag value "++show(ind)); print("\n") };

    // First do a "skew" check to detect when one accumulator has gotten to big. 
    for i = 0 to Array:length(accs)-1 {
      if width(accs[i]) > WARNSKEW
      then {
        print("WARNING: skewed sync, acc sizes: ");
	for j = 0 to Array:length(accs)-1 {
	  print( accs[j]`width ++ " ");
	};
	//	print("\n");
	if requests == [] 
	then print(" no requests.\n")
        else print(" waiting for "++ requests`head ++"\n");
      }
    };


    let (flag, strt, en, seg) = tup;
    
    // ========================================
    // Process the new data:
    if ind == 0 // It's the ctrl signal.
    then requests := append(requests, [(flag,strt,en)])
    else accs[ind-1] := joinsegs(accs[ind-1], seg);        

    // ========================================
    // Now we see if we can process the next request.
    if requests == []
    then {} // Can't do anything yet...
    else {
      let (fl, st, en) = requests`head;

      allready =
	Array:andmap(
	 fun (seg)
	 if (seg == nullseg         ||
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
	  size = int64ToInt(en - st) + 1; // Start,end are inclusive.
  	  emit List:map(fun (seg) subseg(seg,st,size), Array:toList(accs))
	} else {
	  if DEBUGSYNC then
	  print("SyncN: Discarding segment: " ++ show(st) ++ ":" ++ show(en) ++  "\n");
	};

	if (del || not(fl)) then {
	  // In either case, destroy the finished portions and remove the serviced request:
	  for j = 0 to accs`Array:length - 1 {
	    // We don't check "st".  We allow "destroy messages" to kill already killed time segments.
	    // [2007.07.01] JUST FIXED A BUG, We previously were trying to subseg data that wasn't there.
	    killuntil = max(accs[j]`start, en + 1`gint); // Make sure we don't try to subseg before the start.
	    //killuntil = en+1; // This was broken...	    
	    accs[j] := subseg(accs[j], killuntil, int64ToInt(accs[j]`end - killuntil) + 1);
	  };
	};
	requests := requests`tail;
      }
    }
  }
}

syncN = fun (ctrl, strms) { syncN_aux(ctrl, strms, true) }

syncN_no_delete = fun (ctrl, strms) { syncN_aux(ctrl, strms, false) }

fun thresh_extract(search,streamlist,thresh,_pad) {
  pad = _pad`intToInt64;
  ctrl = iterate (w in search) {
    state {
      skiptill = pad;
    }

    //println("in thresh, w.start " ++ w.start);

    for _i = 0 to w.width-1 {
      i = _i`intToInt64;
      if ((w.start + i) > skiptill && absF(w[[_i]]) > thresh) then {
        //println("about to emit t, " ++ w.start + i - pad ++ " " ++ w.start + i + pad - 1);
    	emit (true, w.start + i - pad, w.start + i + pad - 1`gint);
        skiptill := w.start + i + pad;
      }
    };
    
    if (w.start + w.width.intToInt64 - pad - 1`gint > skiptill) then {
      //println("about to emit f, " ++ w.start + w.width - pad - 1);
      emit (false, 0`gint, w.start + w.width.intToInt64 - pad - 1`gint);
    }
  };

  syncN(ctrl,streamlist);
}

fun window(S, len) 
  iterate(x in S) {
    state{ 
      arr = Array:null;
      ind = 0; 
      startsamp = 0`gint;
    }
    if ind == 0 then arr := Array:make(len, x);
    arr[ind] := x;
    ind := ind + 1;
    if ind == len
    then {
      emit toSigseg(arr, startsamp, nulltimebase);
      ind := 0;
      arr := Array:make(len, x); 
      startsamp := startsamp + len`intToInt64;
    }
  };

fun dewindow(s) {
  iterate w in s {
    for i = 0 to w `width - 1 {
      emit w[[i]];
    }
  }
}

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

   // This is INEFFICIENT!  We don't need to do this many subseg operations:
   go := true;
   while go {
     if need_feed then {
       if acc`width > gap // here we discard a segment:
       then {acc := subseg(acc, acc`start + gap`intToInt64, acc`width - gap);
	     need_feed := false; }
       else go := false
      } else {
	if acc`width > newwidth
	then {emit subseg(acc, acc`start, newwidth);
	      if gap > 0 
	      then { 
		acc := subseg(acc, acc`start + newwidth`intToInt64, acc`width - newwidth);
		need_feed := true; 
	      } else acc := subseg(acc, acc`start + feed`intToInt64, acc`width - feed);
	} else go := false
      }
   }
  }
}


// degap takes a stream of sigsegs
// any gaps in the stream will be replaced with sigsegs of init
fun degap(s, init)
{
  iterate w in s {
    state {
      next = gint(0);
    }
    if w != nullseg then {
      if w`start > next then {
	if (next != gint(0)) then {
	  arr = Array:make(int64ToInt(w`start - next), init);
	  emit(toSigseg(arr, next, w`timebase));
	}
      }
      else 
      if w`start == next then emit(w)
      else {
	wserror("gap we can't degap.. start is "++
		w`start++".. prev was "++next);
      };

      next := w`end + gint(1);
    }
  }
}



// This should take a buffer size argument:
// This is much like a zip:
/*
fun interleave(ls, bufsize) {
  using FIFO;
  n = ls`length;
  iterate (newi,newdat) in unionList(ls) {
    state { 
      //bufs = Array:build(n, fun(i) Array:makeUNSAFE(bufsize));
      // bufs = Array:make(n, ls);

      // An array of Queues:
      bufs = Array:build(n, fun(i) make(bufsize));
      ind = 0;
    }
    // Add the data.    
    enqueue(bufs[newi], newdat);
    if empty(bufs[ind])
    then {}
    else {
      emit dequeue(bufs);
      
    }
  }
}
*/

// This version takes a plain stream:
// 
// rrn: would be nice if iterator-merging could allow us to operate on
// raw streams more often, and automatically merge the "window"
// operators into something like "deinterleave".
//
// In fact (dewindow o dinterleave o window) should be efficient.
fun deinterleave(n, strm) {
 List:build(n,
   fun(offset) {
     iterate x in strm {
       state { counter = 0 }
       if counter == offset  then emit x; 
       counter += 1;
       if counter == n       then counter := 0;              
     }
   })
}

// This version takes the number of streams to split int, the desired
// size of output sigsegs, and stream of sigsegs.
fun deinterleaveSS(n, outsize, strm) {
 List:build(n,
   fun(offset) {
     iterate win in strm {
       state { counter = 0;
               newwin = Array:null;
	       firsttime = true;
               newind = 0 }
       // We can't use makeUNSAFE at meta-time currently:
       if firsttime then { firsttime := false; newwin := Array:makeUNSAFE(outsize) };
       for i = 0 to win`width - 1 {
         if counter == offset  
         then {
	   newwin[newind] := win[[i]]; 
	   newind += 1;
	   if newind == outsize then {
	     sampnum = (win`start + i`intToInt64) / n`gint - (outsize`gint - 1`gint);
	     emit toSigseg(newwin, sampnum, win`timebase);
	     newwin := Array:makeUNSAFE(outsize);
	     newind := 0;
	   }
	 };
         counter += 1;
         if counter == n then counter := 0; 
       }
     }
   })
}

fun one_deinterleaveSS2(n, offset, strm) {
  iterate win in strm {
    
    // emit nullsegs to indicate gaps
    state {
      last_counter = 0`gint;
      nulled = false;
      counter = 0;
      newind = 0;
    }
    
    // handle input nullsegs
    if (win == nullseg) then {
      if (nulled != true) then { emit(nullseg); };
      nulled := true;
    }
    
    else {
      
      // detect discontinuities
      if (last_counter != win`start) then {
	if (nulled != true) then { emit(nullseg); };
	nulled := true;
	last_counter := win`start;
      };
      last_counter := last_counter + win`width`intToInt64;
      
      counter := 0;
      newind := 0;
      outsize = win`width / n;
      newwin = Array:makeUNSAFE(outsize);
      
      for i = 0 to (outsize * n - 1) {	 
	if counter == offset  
	then {
	  newwin[newind] := win[[i]]; 
	  newind += 1;
	};
	counter += 1;
	if counter == n then counter := 0; 
      };
      
      sampnum = win`start / n`gint;
      emit toSigseg(newwin, sampnum, win`timebase);
      nulled := false;
    }
  }
}


// This version takes a stream of sigsegs:
// it outputs whole wins as they come, and deals with gaps OK
fun deinterleaveSS2(n, strm) {
  List:build(n,
	     fun(offset) { one_deinterleaveSS2(n, offset, strm); } )
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

// As currently designed this requires allocation!
fun rep_first(count, strm) {
  iterate x in strm {
    state { first_time = true }
    if first_time then { first_time := false; for i = 1 to count { emit x }};
    emit x;
    /*    if n < count then {
      if count == 0 then saved := [x];
      count += 1;      
      }*/
  }
}


/* 
 Experimental:
*/
/*

fun gapstream_state_machine(initstate, startcontig, continuecontig, endcontig) {
  fun (s)
   iterate win in s {
      state {
	lastsamp = -2; // Stream sampnums must be positive.
	userstate = initstate;
      }
      // Gap markers (nullsegs) are ignored.
      if win`width > 0 then 
        if win`start == lastsamp+1 then {
          let (tups, newstate) = continuecontig(win, userstate);
  	  List:foreach( fun(x) emit x, tups);
          userstate := newstate;	
          lastsamp := win`end;
	} else {
	  // We call end once for the gap itself, then start immediately.
          let (tups1, newstate1) = endcontig(win, userstate);
          let (tups2, newstate2) = startcontig(win, newstate1);

  	  List:foreach( fun(x) emit x, tups1);
  	  List:foreach( fun(x) emit x, tups2);
          userstate := newstate2; 
          lastsamp := win`end;
	}
   }
}

// Just an example... only works for positive gaps.
fun rewindow_gapstream(sig, newwidth, gap) {
  feed = newwidth + gap;
  fun dumpavail(acc) {
    howmany = acc`width / feed;
    output = Mutable:ref([]);
    for i = 0 to howmany-1 {
      output := subseg(acc, i * feed, newwidth) ::: output;      
    };
    newstart = howmany*feed;
    (List:reverse(output), 
     subseg(acc, newstart, acc`width - newpos))
  };
  f = gapstream_state_machine(
        nullseg,
        fun (w,acc) { // Start
	  // The remainder left in the acc goes out and we start over.
          let (tups, newacc) = dumpavail(w);
	  (acc ::: tups, newacc)
	}, 
	fun (w,acc) dumpavail(joinsegs(acc,w)); // Continue
	fun (st,en,acc) ([],acc) // Gap, do nothing
      );
  f(sig)
}


*/


// RRN: FIXME Currently untested!!!
// Makes the gaps explicit in a stream.  Inserts nullsegs where there are gaps.
fun make_gapstream(s) {
  iterate win in s {
    state { lastsamp = -2`gint }
      // Gap markers (nullsegs) are ignored.
    if win`width > 0 then {
        if win`start == lastsamp + 1`gint then {
	  emit win;
	} else {
	  emit nullseg;
	  emit win;
	};
	lastsamp := win`end;
    } 
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
      pos = 0`gint;
      lastout = nullseg;
    }
    first = f(ss[[0]]);
    output = Array:make(ss`width, first);
    if pos > ss`start then
      for i = 0 to int64ToInt(pos - ss`start) - 1 {
	// Copy old result:
	output[i] := lastout[[i + int64ToInt(ss`start - lastout`start)]];
      }	     
  };
  strm = rewindow(sss,100,0);
}


//======================================================================
// Useful aliases:

i2f = intToFloat;
i2d = intToDouble;
i2c = intToComplex;
f2i = floatToInt;
f2c = floatToComplex;
f2d = floatToDouble;
c2i = complexToInt;
c2f = complexToFloat;
c2d = complexToDouble;

to64 = intToInt64;
from64 = int64ToInt;

// These are the "advanced" versions.  They're curried.

//amap = 

fun merge3(a,b,c) merge(merge(a,b), c)

Curry:smap    = fun(f) fun(x) stream_map(f,x);
Curry:sfilter = fun(f) fun(x) stream_filter(f,x);  
Curry:deep_stream_map = fun(f) fun(x) deep_stream_map(f,x);  
Curry:sigseg_map      = fun(f) fun(x) sigseg_map(f,x);
Curry:stream_map      = Curry:smap;
Curry:stream_filter   = Curry:sfilter;

fun Curry:map(f)    fun(x) List:map(f,x)
fun Curry:filter(f) fun(x) List:filter(f,x)
fun Curry:fold(f,z) fun(x) List:fold(f,z,x)
fun Curry:fold1(f)  fun(x) List:fold1(f,x)



/* test1 = stream_map(fun(w) w[[0]], audio(0,1024,0)); */
/* test2 = stream_filter(fun (n) n > 300.0, test1); */
/* test3 = stream_iterate(fun (x,st) ([x +. st, 5.0, 6.0], st +. 100.0), */
/* 		       0.0, test2); */
/* test4 = deep_stream_map(fun(x) x /. 100.0, audio(0,10,0)) */
/* BASE <- test4; */

//BASE <- audio(0,10,0);


// Some standard type definitions.

/*
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
*/


/*

union Choose2 (a, b)          = OneOf2 a | TwoOf2 b
union Choose3 (a, b, c)       = OneOf3 a | TwoOf3 b | ThreeOf3 c
union Choose4 (a, b, c, d)    = OneOf4 a | TwoOf4 b | ThreeOf4 c | FourOf4 d
union Choose5 (a, b, c, d, e) = OneOf5 a | TwoOf5 b | ThreeOf5 c | FourOf5 d | FiveOf5 e


*/
