

// This aims to emulate a streamit push/pop/peek interface to streams.
// (Implemented on top of streams of sigsegs.)

// The limitation is that if you want to have state... you have to
// accept and return it explicitly.  However, with interpret-meta that's not true!!
// We could just bind state over that iterate.  That is:
/*
{
  state = foo;
  slidingmagic( ... fun(...) { state := bar } ...)
}
*/
// Everything will work out alright

include "stdlib.ws";

uniontype SplitJoin = Duplicate () | RoundRobin Int;
Streamit:window = window;  // Normal sigseg window.
Streamit:run = dewindow;

namespace Streamit { 

DEFAULT_EXECUTION_SCALE = 1;

fun simple_filter(peekN,popN,pushN, fn)
 fun (strm) {
  iterate w in strm {
    state { 
      acc = nullseg;
      //buf = Array:makeUNSAFE(pushN);
      buf = Array:null;
      advance = 0;
      ind = 0;
      timestamp = 0;
    } 
    fun push(x)   { buf[ind] := x; ind += 1; };
    fun pop(k)    { advance += k; acc[[advance - 1]]; };
    fun peek(i)   acc[[i + advance]];
    assert("peek exceeds pop", peekN >= popN);
    // TEMP:
    if Array:length(buf) == 0 then buf := Array:makeUNSAFE(pushN);
    acc := joinsegs(acc,w);
    while acc.width > peekN {
      fn(peek, pop, push);      
      assert_eq("Popped as we were supposed to.", advance, popN);
      ss = toSigseg(buf, timestamp, nulltimebase);
      //println("  EMITTING: "++buf++"   sigseg: "++ss);
      emit ss;
      //println("SETTING "++acc++" to "++ subseg(acc, acc.start + popN.gint, acc.width - popN));
      acc := subseg(acc, acc.start + popN.gint, acc.width - popN);      
      advance := 0;
      ind := 0;
      buf := Array:makeUNSAFE(pushN);
      //println("Buf cleared: "++buf++" old sigseg "++ss);
      timestamp += pushN.gint;
      //emit ss;
    }
  }
}



// IMPROVEMENT of the above:

// This filter uses the granularity of the input stream to determine
// the granularity of the output stream.
fun filter_keepscale(peekN,popN,pushN, fn)
 fun (strm) {
  iterate w in strm {
    state { 
      acc = nullseg;
      //buf = Array:makeUNSAFE(pushN);
      buf = Array:null;
      advance = 0;
      ind = 0;
      timestamp = 0;
    } 
    fun push(x)   { buf[ind] := x; ind += 1; };
    fun pop(k)    { advance += k; acc[[advance - 1]]; };
    fun peek(i)   acc[[i + advance]];
    assert("peek exceeds pop", peekN >= popN);

    outsize = Mutable:ref(0);
    fun realloc() {
      // Ouch, a division could be expensive, better be scaling a lot!
      if 0 == outsize then {
	// Process all of our input before producing output:
        ////scalefactor = max(1, ((w.width - peekN) / popN) + 1); 
        //scalefactor = max(1, (w.width / popN)); 
	//outsize := pushN * scalefactor;
	
	// Different strategy, maintain approximate input window size,
	// while ensuring that outsize is a multiple of pushN.
	outsize := max(pushN, (w.width / pushN) * pushN);

        //println("Computed scale factor based on width "++ w.width ++" peek/pop/push "++peekN++"/"++popN++"/"++pushN++": "++scalefactor);
	//println("Computed outsize based on width "++ w.width ++" peek/pop/push "++peekN++"/"++popN++"/"++pushN++": "++outsize);
      };
      buf := Array:makeUNSAFE(outsize);
    };

    // TEMP:
    if Array:length(buf) == 0 then realloc();

    acc := joinsegs(acc,w);
    while acc.width > peekN {
      fn(peek, pop, push);      
      assert_eq("Popped as we were supposed to.", advance, popN);
      // OUCH, too many subsegs, might fix as in rewindow:
      acc := subseg(acc, acc.start + popN.gint, acc.width - popN);
      advance := 0;
      if ind ==	buf.Array:length then {
        emit toSigseg(buf, timestamp, nulltimebase);
        ind := 0;
        realloc(); //buf := Array:makeUNSAFE(outsize);
	timestamp += buf.Array:length.gint;
      }
    }
  }
}

// Lets use the better one.
filter = filter_keepscale;


fun print_filter(str)
 filter(1,1,1, fun(peek,pop,push) {
   println(str++" "++peek(0));
   push(pop(1));
   //push(peek(0));
   //pop(1);
 })

// Structured stream construction 


fun compose(f,g) fun(x) f(g(x));
fun pipeline(ls) List:fold(compose, fun(x) x, List:reverse(ls));
//fun pipeline(ls) { }

// TOFINISH
fun splitjoin(head, filters, tail) 
 fun (s) {
  println("Head "++head++"\n Tail "++tail);
  streams = case head { 
    Duplicate(_): 
      map(fun(filt) filt(s), filters)
    RoundRobin(N): 
      wserror("Not handling round robin yet")
  };
  println("Next case: "++tail);
  case tail {
    RoundRobin(n): {
      println("GOT ROUND ROBIN ON TAIL");
      assert_eq("round robin only works for 1 currenty",n,1);
      //roundRobinJoin(DEFAULT_ZIP_BUFSIZE, streams);  
      
      // HACK, throwing in constant here:

      interleaveSS(DEFAULT_ZIP_BUFSIZE, 
                   n * DEFAULT_EXECUTION_SCALE * streams.List:length, 
		   n, streams);
    }
    Duplicate(_):
      wserror("Only roundrobin on splitjoin's tail: "++tail)
    // [2008.02.23] Having problems with defaults:
    //_: wserror("Only roundrobin on splitjoin's tail: "++tail)
  }
}

} // End namespace



// Example program:


using Streamit;

namespace Testing {

f1_state = Mutable:ref(0);
f1 = filter(2, 1, 3, fun(peek,pop,push) {
    push(peek(0));
    push(peek(1));
    push(peek(0) + peek(1));
    pop(1);
  });


ident = filter(1, 1, 1, fun(peek,pop,push) {
    push(peek(0)); pop(1);
  });

add1 = filter(1, 1, 1, fun(peek,pop,push) {
    push(peek(0) + 1); pop(1);
  });

times10 = filter(1, 1, 1, fun(peek,pop,push) {
    push(peek(0) * 10); pop(1);
  });

s0 = COUNTUP(1).window(10);
//s0 = COUNTUP(1).window(1);
};

//main = s0;
//main = f1 $ s0;
//main = pipeline([f1,f1]) $ s0;
main = { 
  //  using Streamit;
  using Testing;
  pipeline([add1, add1, add1, times10]) $ s0;
}
//main = ident $ s0;
