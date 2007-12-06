


//======================================================================
// First, some generally useful library functions.
// (These could potentially go in a separate module.


//========================================
// First some functions for manipulating functions.

// Here we write a generic multiplier for windows.  This is fancy!
gen_mult : (('a, 'a) -> 'a) -> (Sigseg 'a, Sigseg 'a) -> Array 'a;
fun gen_mult(f) {
  // Here I'm returning an anonymous function... but you could give it a name if you wanted.
    fun (w1, w2) {
      if w1.width != w2.width
      then error("mult: mismatching lengths")
      else if w1.width == 0
      then Array:null
      else {
	// RRN: Doing a window_integrate over two vars is a little confusing...
	// (i.e. it looks like it mean the cartesian product)
	  out = newarr(w1.width, w1[[0]]);
	  for i = 0 to w1.width - 1 {
	    out[i] := f(w1[[i]], w2[[i]]);
	  };
	  out
      }
    }
  }

fun arr2segfun(f) {
  fun (w) {
    f(to_array(w))
  }}


//========================================
// Specialized versions of generic procedures:

// These are the specialized multipliers for sigrefs of ints & sigrefs of floats.
// For now let's say * is for ints and *. is for floats (ML-style).  
mult   : (Sigseg int,   Sigseg int)   -> Sigseg int;
flmult : (Sigseg float, Sigseg float) -> Sigseg float;

mult   = gen_mult(( *));
flmult = gen_mult(( *.));
// We could potentially include a fancy mechanism called "type
// classes" for allowing overloading of operators, but that's
// something we have yet to consider. 


//========================================

rewindow : (Stream 'a, int, int) -> Stream 'a;
// rewindow takes a Stream of Sigsegs, a new window-width, and a new step/feed/slide:
fun rewindow(s, newwidth, step) {
  if step >= newwidth
  then error("rewindow won't allow the creation of non-contiguous output streams")
  else iterate w in s {
   state { acc = nullseg; }
   acc := joinsegs(acc, w);
   // We do this entirely with index numbers, no abstract Time objects are used:
   for i = 1 to w.width {
     if acc.width > newwidth
     then {emit subseg(acc, 0, newwidth);
	   acc := subseg(acc, step, acc.width - step)}
     else break;
}}}


/* This rewindow incorporates Lewis's suggestion of leveraging the
   size of the input window to provide an upper bound for the number
   of iterations in the loop.  (To maintain the property that all
   boxes must terminate when they fire.) */


arrmagnitude : Array complex -> Array float;
fun arrmagnitude(w) {
  // RRN: Note, We could include it, but don't think we need
  // window_integrate, because if we're going with the "One Array
  // Type" theory we need the ability to mutate Sigrefs anyway.
  //return window_integrate(c in w)
  // emit(sqrt(sqr(c.real) + sqr(c.imag)));

  out = newarr(w.length, 0.0);
  for i = 0 to w.length - 1 {
    out[i] := sqrt( sqr(w[i].realpart) +. sqr(w[i].imagpart));
  };
  out
}

// This version produces a new Sigref
magnitude : Stream (Sigseg complex) -> Stream (Sigseg float);
fun magnitude(s) {
  deep_iterate c in s {
    emit sqrt( sqr(c.realpart) +. sqr(c.imagpart));
  }
}


// RRN: I'm going to leave window_integrate in the rest of the document until we make a decision...

// RRN: Rewrote without "window_const"
// Just doing this with floats for now:
hanning : int -> array float;
// Multiplies a window by the hanning function:
fun hanning(numpoints) {
  out = newarr(numpoints, 0.0);
  np = intToFloat(numpoints-1);
  for i = 0 to numpoints - 1 {
    f = intToFloat(i);
    out[i] := 0.5 *. (1. -. cos(2. *. pi *. f /. np));
  };
  out;
}


//specgram : Stream (Sigseg Float) -> Stream (Sigseg Float, Sigseg Float)
fun specgram(s) {
  iterate(w in s) {
//    emit (magnitude (fft (flmult (to_array(w), hanning(w.width)))), w);
    // Where fft has type Sigseg Float -> Sigseg Complex
  }
}


// ======================================================================

main = audio(0, 1000, 500);

      
