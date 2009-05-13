

include "stdlib.ws"

/* NOTE: Some of these are just aliases for common functions in the
 * standard library, possibly with the arguments rearranged.  For this
 * library we make sure that the stream argument is always last.
 */

/* 
 * Select only certain tuples from a stream.
 */ 
SELECT = stream_filter

/* 
 * Window by time.  Assumes a TIME field.
 */ 
fun WINDOW_notimeout(size, strm) 
  iterate r in strm {
    state { edge = 0; 
            first = true;
	    buffer = [];
	    count = 0; }
    // Calibrate the time to the first tuple.
    if first then { edge := r.TIME + size; first := false };

    // Unless we maintain a timer, we can't produce output until we
    // get a tuple that falls OUTSIDE of the time range.
    // (TODO, maintain a timer)
    if r.TIME >= edge then {
      emit buffer;
      buffer := [];
      count := 0;
      edge := edge + size;
    };

    count += 1;
    buffer := r ::: buffer;
  }


fun WINDOW(size, strm) 
  iterate x in union2(strm, timer(1.0 / size)) {
    state { count = 0; buffer = [] }
    case x {
      Left(r): {
        buffer := r ::: buffer;
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


//fun WINDOW(size, strm) = window(strm, size)
//fun REWINDOW(size,gap, strm) = rewindow(strm, size, gap

/* 
 * 
 */ 


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

