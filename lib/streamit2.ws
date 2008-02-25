

/*

 The sigseg based version (streamit.ws) could handle input windows of
 arbitrarily granularity, and would produce output windows based no
 the size of inputs.

 This version does more work at metaprogram eval and uses data rates
 to compute static communication buffer sizes.

 .author Ryan Newton

 */

include "stdlib.ws";
include "math.ws";

uniontype SplitJoin = Duplicate () | RoundRobin Int;
Streamit:window = ArrayStream:window;  // Normal sigseg window.

//type StreamOp = ((Int,Int,Int) * (Stream a -> Stream b));

//uniontype StrmProgram = Pipeline (List ) | RoundRobin Int;



type Rates = (Int * Int * Int);
type Peek (a,b) = Int -> a;
type Pop  (a,b) = Int -> a;
type Push (a,b) = b   -> ();
type Filter (a,b) = ((List Rates) * (((List Int), Stream a) -> ((List Int) * Stream b)));

// The user function passed to "filter":
type FiltFun (a,b) = (Peek (a,b), Pop (a,b), Push (a,b)) -> ();

namespace Streamit { 

filter :: (Int,Int,Int, FiltFun (a,b)) -> Filter (a,b);
run    :: (Filter (a,b), Stream a) -> Stream b;

/* For lack of a sophisticated model of data and instruction caches,
 * we simply have some parameters here.  The algorithm will scale up
 * an execution until the lowest-rate channel passes MIN_ELEMENTS per
 * iteration, unless doing so would cause the maximum-rate channel to
 * exceed MAX_ELEMENTS.
 */
MIN_ELEMENTS = 128
MAX_ELEMENTS = 2048

// Compute a list of execution scaling factors.
fun compute_schedule(rates) {
  println(" Computing schedule: "++rates);
  acc  = ref$ [1];
  prev = ref$ rates.head;
  using List;
  foreach(fun(this) {
    let (peek,pop,push) = this;
    let (oldpeek,oldpop,oldpush) = prev;
    println("\nAdding in : "++(peek,pop,push));
    println("  acc is "++acc++"  prev "++prev);
    println("  prev scaled is: "++ oldpush * acc.head);
    println("  LCM is: "++ lcm(oldpush * acc.head, pop));

    mult = lcm(oldpush * acc.head, pop);    
    scalefactor = mult / pop;
    println("  which is scale factor: "++scalefactor);
    scalethem = mult / (oldpush * acc.head);

    acc := scalefactor ::: map((* scalethem), acc);
    prev := this;
  }, rates.tail);

  final = reverse$ acc;
  println("Final scale factors: "++ final);
  println("Final schedule: "++ 
          map2(fun(k,(a,b,c)) (k*a, k*b, k*c), final, rates));
  final;
}

fun roundup_schedule(schedule, rates) {
  using List;
  println(" Rounding up "++ schedule++" " ++ rates);

  maxs = map2(fun((pk,pp,psh), k) k * max(pp,psh), rates, schedule);
  mins = map2(fun((pk,pp,psh), k) k * min(pp,psh), rates, schedule);
  mx =  fold1(max,maxs);
  mn =  fold1(min,mins);
/*   morescale = ref$ 1; */
/*   while mn < MIN_ELEMENTS && mx < MAX_ELEMENTS { */
/*     println("Scaling up schedule: ");     */
/*   }; */

  morescale = MIN_ELEMENTS / mn;
  maxscale  = MAX_ELEMENTS / mx;  
  finalscale = min(morescale,maxscale);
  newsched = map((* finalscale), schedule);
  println("Rounding up scaling from "++ schedule ++" to: "++ newsched);
  newsched
}

// This computes a schedule and instantiates the stream graph.
fun run((rates, tform), source) {
  schedule = compute_schedule(rates);
  sched2   = roundup_schedule(schedule, rates);
  let (_,pop,_) = rates.head;
  inputrate = sched2.head * pop;
  let (_,strm) = tform(sched2, source.window(inputrate));
  strm
}

fun filter(peekN,popN,pushN, fn) {
 assert("peek exceeds pop", peekN >= popN);
 assert("peek currently must equal pop", peekN == popN);
 fun transform(scales, strm) {
  println("Building filter with scale: "++scales);
  //outbuf = Array:makeUNSAFE(scale * pushN);
  scale = scales.head;
  results = iterate arr in strm {
    println("Received buf of size: "++arr.Array:length);
    outbuf = Array:make(scale * pushN, 0);
    outind  = Mutable:ref(0);
    advance = Mutable:ref(0);
    fun push(x)   { outbuf[outind] := x; outind += 1; };
    fun pop(k)    { advance += k; arr[advance - 1]; };
    fun peek(i)   arr[i + advance];
    //println(" Executing in, scale "++scale++": "++arr);
    for i = 1 to scale {
      //println(" Executing "++i++" of "++scale);
      fn(peek, pop, push);
      //println(" Executed out:  "++outbuf);
    };
    emit outbuf;
  };
  (scales.tail, results)
 }; // end transform function
 ([(peekN,popN,pushN)], transform)
}

// Structured stream construction 
fun pipeline(ls) {
   using List;
   rates = fold(append,[], map(fst, ls));
   fun tform(scales, strm) {
     scls = Mutable:ref(scales);
     stm  = Mutable:ref(strm);     
     foreach(fun((_,fn)) {
       let (newls, newstrm) = fn(scls, stm);
       scls := newls;
       stm  := newstrm;
     }, ls);
     (scls, stm)
   };
   (rates, tform)
 }


/*      fns   = List:map(thd, ls); */
/*      p1 = Mutable:ref(scales); */
/*      p2 = Mutable:ref(fns); */
/*      while p1 != [] { */
/*        println("   Hmm "++p2.head); */
/*        println("   hrm "++p1.head); */

/*        p1 := p1.tail; */
/*        p2 := p2.tail; */
/*      }; */




//fun pipeline(ls) { }

/*

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

*/

} // End namespace


using Streamit;

/*

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

//main = s0a;
//main = f1 $ s0;
//main = pipeline([f1,f1]) $ s0;
main = pipeline([add1, add1, add1, times10]) $ s0;
//main = ident $ s0;
*/



s0 = COUNTUP(1);
add1 = filter(1, 1, 1, fun(peek,pop,push) {
    //println("  Add1 "++peek(0));
    push(peek(0) + 1); 
    pop(1);
  });

sum = filter(3, 3, 1, fun(peek,pop,push) {
    push(pop(1) + pop(1) + pop(1));
  });

//Streamit:pipeline :: List (List (a, b) -> b) -> (List (a, b) -> b * (List lpq, lpj) -> lpj);

let pipe = pipeline([add1, sum, add1]);

test_schedule :: List Rates;
test_schedule = [(1,1,7), (3,3,2), (1,1,1), (5,5,7)];

main = { 
  //println("Test Schedule: "++ compute_schedule(test_schedule));
  run(pipe,s0);
}

//main = s0;
