

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
      emit toSigseg(List:toArray(List:reverse(buffer)), Int64! starttime, nulltimebase);
      
      buffer := [];
      count := 0;
      edge := edge + size;

      starttime := r.TIME;
    };

    count += 1;
    buffer := proj(r) ::: buffer;
  }



// This variant also supports grouping.

// I have modified this version to not use the actual timestamp
// contents as the sigseg "start" metadata.  That doesn't really make
// sense.  Instead, we use simple sequence numbers here.
fun TIMESTAMP_WINDOW_GROUPBY(proj, groupby, sze, strm) {
  using HashTable;
  iterate r in strm {
    state { 
            edges      = make(100);
	    buffers    = make(100);
	    starttimes = make(100);
	    counts     = make(100);
	  }
    key = groupby(r);

    // Initialize on the first tuple in a group:
    if not(starttimes.contains(key)) then {
	edges.set_BANG(key, r.TIME + sze);
	buffers.set_BANG(key,[]);

        starttimes.set_BANG(key, (0::Int64));
        counts.set_BANG    (key, (0::Int64));
    };

    // Unless we maintain a timer, we can't produce output until we
    // get a tuple that falls OUTSIDE of the time range.
    // (TODO, maintain a timer)
    if r.TIME >= edges.get(key) then {

      emit toSigseg(List:toArray(List:reverse(buffers.get(key))), starttimes.get(key), nulltimebase);

      // Set the start-time for the next window to the index of 
      starttimes.set_BANG(key, counts.get(key));
      buffers.set_BANG(key, []);
      edges.set_BANG(key, edges.get(key) + sze);
    };
    buffers.set_BANG(key, proj(r) ::: buffers.get(key));
    counts.set_BANG(key, counts.get(key) + 1); 
  }
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
	emit List:reverse(tmp);
      }
    }
  }

// TEMP
//WINDOW = if false then REALTIME_WINDOW else TIMESTAMP_WINDOW
//WINDOW = TIMESTAMP_WINDOW


//fun WINDOW(size, strm) = window(strm, size)
//fun REWINDOW(size,gap, strm) = rewindow(strm, size, gap



fun REWINDOW_GROUPBY(groupby, newwidth, gap, sig) {
  feed = newwidth + gap;

  using HashTable;
  if (gap <= (0 - newwidth))
    then wserror("REWINDOW_GROUPBY cannot step backwards: width "++ show(newwidth) ++" gap "++show(gap))
    else 
        
   iterate (win in sig) {
    state { 
      acc = make(100);
      // This bool helps to handle an output streams with gaps.
      // We have two states, true means we're to "output" a gap next,
      // false means we're to output a sigseg next.
      need_feed = make(100);
      go = false; // Temp 
    }

    key = groupby(win);

    if not(acc.contains(key)) then { 
      acc.set_BANG(key, nullseg);
      need_feed.set_BANG(key, false);
    };

    newacc = joinsegs(acc.get(key), win);
    acc.set_BANG(key, newacc);
    //print("Acc "++show(acc`start)++":"++show(acc`end)++" need_feed "++show(need_feed)++"\n");

   // This is INEFFICIENT!  We don't need to do this many subseg operations:
   go := true;
   while go {

     temp = acc.get(key);
     //print(temp.width ++" ");

     if need_feed.get(key) then {
       if temp.width > gap // here we discard a segment:
       then { acc.set_BANG(key, subseg(temp, temp.start + Int64! gap, temp.width - gap));
	      need_feed.set_BANG(key, false);
	    }
       else go := false
      } else {
	if temp.width > newwidth
	then {emit subseg(temp, temp.start, newwidth);
	      if gap > 0 
	      then { 
	        acc.set_BANG(key, subseg(temp, temp.start + Int64! newwidth, temp.width - newwidth));
		need_feed.set_BANG(key, true); 
	      } else acc.set_BANG(key, subseg(temp, temp.start + Int64! feed, temp.width - feed));
	} else go := false
      }
   }
  }
}



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

