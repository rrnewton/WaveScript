


// What would it actually take to expose a push/pop/peek interface to (Stream (Sigseg T))??

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

// Inefficient implementation:
fun filter(peekN,popN,pushN, fn)
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
    fun pop(k)    advance += k;
    fun peek(i)   acc[[i + advance]];
    assert("peek exceeds pop", peekN >= popN);

    // TEMP:
    if Array:length(buf) == 0 then buf := Array:makeUNSAFE(pushN);

    acc := joinsegs(acc,w);
    while acc.width > peekN {
      fn(peek, pop, push);      
      assert_eq("Popped as we were supposed to.", advance, popN);
      emit toSigseg(buf, timestamp, nulltimebase);
      println("SETTING "++acc++" to "++ subseg(acc, acc.start + popN.gint, acc.width - popN));
      acc := subseg(acc, acc.start + popN.gint, acc.width - popN);      
      advance := 0;
      ind := 0;
      buf = Array:makeUNSAFE(pushN);
      timestamp += pushN.gint;
    }
  }
}

// Structured stream construction 


fun compose(f,g) fun(x) f(g(x));
fun pipeline(ls) List:fold(compose, fun(x) x, ls);
//fun pipeline(ls) { }

uniontype SplitJoin = Duplicate () | RoundRobin Int;

fun splitjoin() {
}


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

//main = s0;
//main = f1 $ s0;
//main = pipeline([f1,f1]) $ s0;
//main = pipeline([add1, times10]) $ s0;
main = ident $ s0;
