
// This provides a facility for pull-based streams.
// It uses the metaprogramming framework to thread "pull" wires through the query.

include "stdlib.ws";

type PullStream t = (Stream ()) -> Stream t;

// INTERFACE:

Pull:pullwith :: (PullStream t, Stream ()) -> Stream t;
Pull:counter  :: Int -> (Stream ()) -> Stream Int;
Pull:applyST  :: (Int, PullStream a, (Stream a -> Stream b)) -> PullStream b;
Pull:readFile :: (String, String) -> PullStream any;

// TODO FIX PARSE ERROR!!!!!!!!!!
//Pull:counter  :: Int -> Stream () -> Stream Int;


// ================================================================================
// Data type defs:

// For internal use only:
uniontype PullIterEvent a b = 
  UpstreamPull   ()   | 
  DownstreamPull ()   | 
  UpstreamResult a    | 
  FinalResult    b    ;

fun filtUpstreamPull(s) 
  iterate x in s { 
   case x { UpstreamPull  (_): emit ()  
            UpstreamResult(_): ()
            FinalResult   (_): ()
            DownstreamPull(_): ()
	  }
  }
fun filtFinalResult(s) 
  iterate x in s { 
   case x { UpstreamPull  (_): ()  
            UpstreamResult(_): ()
            FinalResult   (x): emit x
            DownstreamPull(_): ()
	  }
  }

// ================================================================================
// Operations on pull-streams:

fun Pull:pullwith(pstream, strm) pstream(strm)

//Pull:counter :: Int -> PullStream Int;
fun Pull:counter(strt)
 fun (pulls)
   iterate _ in pulls {
     state{ counter = strt - 1 }
     counter += 1;
     emit counter;
   }

fun Pull:applyST(qsize, src, transformer) {
  fun(pullstring) {        
    filtFinalResult$
    feedbackloop(stream_map(fun(x) DownstreamPull(()), pullstring),
      fun(loopback) {
	// First any pulls to the upstream we have to route appropriately:
        source = src(filtUpstreamPull(loopback));
	// We apply the user's stream transformer to that upwelling stream of results:
	results = stream_map(UpstreamResult, transformer(source));
	// This is the central event-dispatcher:
        iterate evt in merge(results,loopback) {
         state { buf = FIFO:make(qsize);
	         owed = 0 }
         case evt {
	   FinalResult(x)    : {} //emit evt
	   UpstreamPull (_)  : {} // This will have been handled above.

 	   DownstreamPull(_) : 
	     {
	       // If we have buffered data, respond with that:
	       if not(FIFO:empty(buf))
	       then emit FinalResult(FIFO:dequeue(buf))
	       else {
	         // Otherwise we have to pull our upstream to get some tuples.
	         owed += 1;
		 emit UpstreamPull(())
	       }
	     }
           UpstreamResult(x) : 
	     {
	       if owed > 0 then {
	         owed -= 1;
		 emit FinalResult(x);
	       } else 
                 FIFO:enqueue(buf,x)
	     }
         }
       }
     });
  }
}

fun Pull:map(fn,strm) Pull:applyST(0, strm, fun(s) stream_map(fn,s))

// Can't yet get away with this readFile without the immediate enclosing assert-type.
// The types get lost in static-elaborate.
fun Pull:readFile(file, mode)
  fun (pullstring) readFile(file, mode, pullstring)

/*
fun Pull:zip2(s1,s2)
  fun (pullstring)
   iterate x in _ {
     state { flip = false }
     if flip
     then emit PullFirst();
     else emit PullFirst();
   }
  */

// ================================================================================

// ALTERNATIVLY we could use this interface:
pstream = 
  Pull:map((+1000), 
     Pull:applyST(2, Pull:counter(10),
	    fun (s) iterate n in s {
	       emit n+100;
	       emit n+200;
	       emit n+300;
	     }));
BASE <- Pull:pullwith(pstream, timer(3.0))

//filesrc :: PullStream Int;
//filesrc = (Pull:readFile("pull.ws", "mode: binary") :: PullStream Int);
//BASE <- Pull:pullwith(filesrc, timer(3.0))


// SCRAP
/*


  // A Queue is pushed from one end and pulled from the other.
  fun queue(n, s) {
    fun (pullstring)
    iterate x in union2(s, pullstring) {
      state { buf = Array:makeUNSAFE(n) }
      case x {
        Oneof2(x): 
	  
        Twoof2(_): // This is a pull, we produce an output. 
      }
    }    
  }
*/

