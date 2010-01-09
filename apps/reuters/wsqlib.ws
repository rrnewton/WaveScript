
include "stdlib.ws"
include "unix.ws"
include "socket.ws"


/* NOTE: Some of these are just aliases for common functions in the
 * standard library, possibly with the arguments rearranged.  For this
 * library we make sure that the stream argument is always last.
 */

/******************************************************************************/
/*  Interface.  */

// MAP, FILTER, AVG

TIMESTAMP_WINDOW :: 
     ((b | TIME:#a) -> c, #a, Stream (b | TIME:#a))
     -> Stream (Sigseg c);

TIMESTAMP_WINDOW_GROUPBY :: 
     ((b | TIME:#a) -> c,      // Projection
      (b | TIME:#a) -> key,    // Groupby function
      #a,                      // Window size
       Stream (b | TIME:#a))   // Input stream
     -> Stream (Sigseg c);

// TIMESTAMP_WINDOW_GROUPBY_MINSLIDE :: 

// REALTIME_WINDOW

// WINDOW
// REWINDOW_GROUPBY

// TIMESTAMP_JOIN




/******************************************************************************/

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

fun AVG_OF(proj, ss) {
  sum = 0;
  count = 0;
  for i = 0 to ss.width-1 { 
    sum += proj( ss[[i]] );
    count += 1 
  };
  // Return value:
  sum / count
}


/************************************************
 * Window by time.  Assumes a TIME field.
   Does a projection as well.
 */ 

// FIXME: type too weak:
/*
TIMESTAMP_WINDOW :: 
     ((b | TIME:Int64) -> c, Int64, Stream (b | TIME:Int64))
     -> Stream (Sigseg c);
*/

// Inferred: [2009.10.18]
// TIMESTAMP_WINDOW :: ((c | TIME:#b) -> 'd, #b, Stream (c | TIME:#b)) -> SS 'f;


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


//================================================================================

// Here is the WINDOW construct under StreamSQL... this is the functionality we want.

/*
window_identifier
A unique name for the window declaration.

window_specification
A description of the window in the format:

  SIZE size ADVANCE increment
  {TIME | TUPLES | ON field_identifier_w}
  [PARTITION BY field_identifier_p[,...]]
  [VALID ALWAYS]
  [OFFSET offset]
  [TIMEOUT timeout]
size
The size of the window, expressed as either the number of tuples, an interval of time, or a range of values within a tuple field.

increment
The amount each subsequent window is offset from the first window that opens on a stream.

field_identifier_w
The tuple field used to set window size and advance.

field_identifier_p
The tuple field to use to create windows for separate groupings of tuples.

offset
Indicates when to open the first window on a stream.

timeout
Indicates the amount of time (in seconds) after which a window should close regardless of whether a tuple has arrived.
*/

// TODO: VALID ALWAYS, OFFSET, TIMEOUT
// What about time/tuples?  

// Currently 
fun TIMESTAMP_WINDOW_GROUPBY_MINSLIDE(proj, groupby, timerng, strm) {
  using HashTable;
  iterate r in strm {
    state { 
            edges      = make(100);
	    buffers    = make(100);
	    starttimes = make(100);
	    counts     = make(100);
	  }
    key = groupby(r);

    fun crosses_edge(r) {
      r.TIME >= edges.get(key);
    };
    
    fun slice_current(buf) {
      using List;
      toArray( map(proj, buf.reverse))
    };

    // Initialize on the first tuple in a group:
    if not(starttimes.contains(key)) then {
	edges.set_BANG(key, r.TIME + timerng);
	buffers.set_BANG(key,[]);

        starttimes.set_BANG(key, (0::Int64));
        counts.set_BANG    (key, (0::Int64));
    };

    // if ADVANCE WINDOW
    if crosses_edge(r) then {

      // Slide the time window forward just enough to include the next tuple.
      newstart = r.TIME - timerng;

      // FIXME: need to pay attention to inclusive/exclusive.
      fun advance_edge(old) {
        //old + timerng;
	newstart + timerng;
      };

      // The problem with accumulating arrays of any kind, is that they have to be completely full when we pass them to toSigseg.
      // If we're forming windows with a fixed size this is no problem....
      // Otherwise, a doubly linked list wouldn't be bad here.  Or we could store both in a FIFO and a list.

      emit toSigseg( slice_current(buffers.get(key)), starttimes.get(key), nulltimebase);

      // SLIDE instead of reset.

      shed = 0;
      newbuf = {
	using List;
        flipped = buffers.get(key).reverse;
        while (not(flipped.is_null) && flipped.head.TIME < newstart) {
	  // println("  pruning out "++ flipped.head);
	  flipped := flipped.tail;
	  shed += 1;
	};
	//	println("");
        flipped.reverse; 
      };

      fun advance_start(old) {
        //counts.get(key);
        //old + 1
	//newstart
	old + shed
      };

      // Set the start-time for the next window to the index of 
      starttimes.set_BANG(key, advance_start(starttimes.get(key)));
      buffers.set_BANG   (key, newbuf);
      edges.set_BANG     (key, advance_edge( edges.get(key)) );
    };
    buffers.set_BANG(key, r ::: buffers.get(key));
    counts.set_BANG (key, counts.get(key) + 1); 
  }
}



//============================================================

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

fun WINDOW(size, strm) window(strm, size)
//fun REWINDOW(size,gap, strm) = rewindow(strm, size, gap

//================================================================================

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

//================================================================================

// Joining and Syncing

// Join on a one-to-one basis via timestamps.
//   ASSUMES: monotonically increasing timestamps
//   ASSUMES: record stream elements with TIME field
// If one stream has a tuple that doesn't have a counterpart in the
// other stream, it is simply DROPPED.

fun TIMESTAMP_JOIN(s1,s2) {
  using FIFO;
  iterate(x in union2(s1,s2)) {
    state {
      buf1 = make(10);
      buf2 = make(10);
    }

    case x { 
      Left  (a): buf1.enqueue(a)
      Right (b): buf2.enqueue(b)
    };

    //buf1.peek(0).FOOBAR + 1;

    while (buf1.elements > 0 && buf2.elements > 0) {

      a = buf1.peek(0);      
      //b = ( TIME= 99 );
      b = buf2.peek(0);      
      if (a.TIME == b.TIME) then {
        buf1.dequeue();
        buf2.dequeue();
        emit(a, b);
      } else {
        // The younger one is necessarily trash, because of monotonicity 
	//tm1 = a.TIME;
	//tm2 = b.TIME;
	//if tm1 < tm2 then buf1.dequeue() else ();
	// SUBTLE AND INTERESTING BUG!!  If forces the values to be the same here.
        if a.TIME < b.TIME                 
        then { buf1.dequeue(); () }
        else { buf2.dequeue(); () } 
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



/******************************************************************************/

/* [2009.10] Functions called by generated WSQ code (via the C API) */

fun discard(s) iterate _ in s { }

// This is a temporary fake data source with a temporary fake schema.
type DummySchema99 = (| SYM:String, TIME:Float, PRICE:Float, VOLUME:Int);

//wsq_reuterSource :: String -> Stream (| TIME : Float, SYM : String, PRICE : Float);
wsq_reuterSource :: String -> Stream DummySchema99;
fun wsq_reuterSource(schema) {
  syms = #["IBM", "GOOG", "GM", "F", "IMGN"];
  lastprice = Array:make(Array:length(syms), 50.0);
  iterate _ in timer(2) {
    state{ t = 0.0 }
    i = randomI(Array:length(syms));
    // Random walk:
    lastprice[i] += (Float! (randomI(200) - 100)) / 100;
    if lastprice[i] < 0
    then lastprice[i] := 0;

    emit (SYM= syms[i], 
          TIME= t, 
          PRICE= lastprice[i],
	  VOLUME= randomI(10) + 1
  	  );
    t += randomI(10).gint;
  }
}


wsq_filter  = stream_filter
wsq_project = stream_map

fun wsq_windowJoin(cmpr, combine, left, right, winsize) {
  iterate x in union2(left,right) {
    print(".");
    //    print(" -*|*- Running window joiner: \n")
 // GETTING A TYPE CHECKING ERROR!!!!
/*
    case x { 
      Left(t):  print(" -*|*- Running window joiner: LEFT\n")
      Right(t): print(" -*|*- Running window joiner: RIGHT\n")
    }
*/
    //println(" -*|*- Running window joiner: " ++ x);
  }
}

fun wsq_printer(str, s) {
  //stream_map(fun(x) { print(x); x }, s)
  iterate x in s { 
    print(str);
    print(x); 
    print("\n");    
    Unix:fflush(Unix:stdout); 
  }
}

fun wsq_connect_out(host, prt, strm) {
  //print("  **** wsq_connect_out not implemented yet! **** \n");
  //iterate _ in strm { }
  println(" <WSQ> Creating outgoing (server) socket, port: " ++ prt);
  socket_out(strm, prt);

  // Hmm... this was a typo, but why didn't it work??
  //iterate _ in timer(0) {}  
}

fun wsq_connect_in(host, prt) {
  /*
  print("  **** wsq_connect_in not implemented yet! **** \n");
  iterate _ in timer(1) {
    emit (| BAZ="blah", BAR=0.0 )
  }
  */
  println(" <WSQ> Creating incoming (client) socket, host "++ host ++" port: " ++ prt);
  //(socket_in(host, prt) :: Stream (| BAZ : String, BAR : Float ) )
  socket_in(host, prt)
}
