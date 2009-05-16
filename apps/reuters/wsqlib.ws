

include "stdlib.ws"

/* NOTE: Some of these are just aliases for common functions in the
 * standard library, possibly with the arguments rearranged.  For this
 * library we make sure that the stream argument is always last.
 */

/* 
 * Select only certain tuples from a stream.
 */ 
//SELECT = stream_filter
fun SELECT(project, pred, strm) {
  iterate x in strm {
    if pred(x) 
    then emit project(x);
  }
}

/************************************************
 * MAP a function over every element of a stream.
 */ 
MAP = stream_map
FILTER = stream_filter

/* 
 * A function of a window (sigseg):
 */ 
// [2009.05.14] Getting too weak a type here, FIXME!!!
//AVG :: Sigseg #a -> #a;
//AVG :: Sigseg Int -> Int;
fun AVG(ss) {
  sum = 0;
  count = 0;

  // Apply to each element of the sigseg: 

  // These two version result in the WRONG TYPE!!!
  //fun f(x) { sum += x; count += 1 };
  //Sigseg:foreach(f, ss);
  //for i = 0 to ss`width - 1 {  f(ss[[i]])  };

  // This one results in the CORRECT TYPE!!
  for i = 0 to ss`width - 1 { sum += ss[[i]]; count += 1 };

  // Return value:
  sum / count
}

/************************************************
 * Window by time.  Assumes a TIME field.
 */ 

// FIXME: type too weak:
/*
TIMESTAMP_WINDOW :: 
     ((b | TIME:Int64) -> c, Int64, Stream (b | TIME:Int64))
     -> Stream (Sigseg c);
*/
TIMESTAMP_WINDOW :: 
     ((b | TIME:#a) -> c, #a, Stream (b | TIME:#a))
     -> Stream (Sigseg c);
fun TIMESTAMP_WINDOW(proj, size, strm) 
  iterate r in strm {
    state { edge = 0; 
            first = true;
	    buffer = [];
	    starttime = 0;
	    count = 0; }
    // Calibrate the time to the first tuple.
    if first then { edge := r.TIME + size; first := false };

    // Unless we maintain a timer, we can't produce output until we
    // get a tuple that falls OUTSIDE of the time range.
    // (TODO, maintain a timer)
    if r.TIME >= edge then {

      // emit toSigseg(List:toArray(buffer), starttime, nulltimebase);
      emit toSigseg(List:toArray(buffer), Int64! starttime, nulltimebase);
      
      buffer := [];
      count := 0;
      edge := edge + size;

      starttime := r.TIME;
    };

    count += 1;
    buffer := proj(r) ::: buffer;
  }


// This variant also supports grouping.
fun TIMESTAMP_WINDOW_GROUPBY(proj, groupby, size, strm) 
  iterate r in strm {
    state { edge = 0; 
            first = true;
	    buffer = [];
	    starttime = 0;
	    count = 0; }
    // Calibrate the time to the first tuple.
    if first then { edge := r.TIME + size; first := false };

    // Unless we maintain a timer, we can't produce output until we
    // get a tuple that falls OUTSIDE of the time range.
    // (TODO, maintain a timer)
    if r.TIME >= edge then {

      // emit toSigseg(List:toArray(buffer), starttime, nulltimebase);
      emit toSigseg(List:toArray(buffer), Int64! starttime, nulltimebase);
      
      buffer := [];
      count := 0;
      edge := edge + size;

      starttime := r.TIME;
    };

    count += 1;
    buffer := proj(r) ::: buffer;
  }



// This version does things based on arrival time, not on timestamp.
// It doesn't require a timestamp at all.
fun REALTIME_WINDOW(proj, size, strm) 
  iterate x in union2(strm, timer(1.0 / Float! size)) {
    state { count = 0; buffer = [] }
    case x {
      Left(r): {
      buffer := proj(r) ::: buffer;
        count += 1;
      }
      Right(_): {
        tmp = buffer;
	buffer := [];
	count := 0;
	emit tmp;
      }
    }
  }

// TEMP
//WINDOW = if false then REALTIME_WINDOW else TIMESTAMP_WINDOW
WINDOW = TIMESTAMP_WINDOW


//fun WINDOW(size, strm) = window(strm, size)
//fun REWINDOW(size,gap, strm) = rewindow(strm, size, gap




/******************************************************************************/
/*  Handling network communication */

/* 
 * Receive an input stream from a TCP Socket.
 */ 
fun TCPINPUT(str) {
 // TODO
}

fun TCPOUTPUT(str) {
 // TODO
}

