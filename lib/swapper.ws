






// This is a library that implements "swappable" components for the
// stream graph.

include "stdlib.ws";

// ================================================================================
// Data type defs:

type ST (a,b) = Stream a -> Stream b;
type Quiesce = ();
type Activate st = st;
type ACK = ();

// A swappable component is a normal stream transformer *except* that
// it can also take a Queisce request and produce an Activate (state bundle).
// OR it can take an Activate and produce an ACK.
type Swapable (a, b, st) = 
     Stream (Union3 (a,  Quiesce,     Activate st))
  -> Stream (Union3 (b,  Activate st,  ACK));

/* type Goo = Stream List Int -> Stream Float; */
/* type Bar = Stream List Sigseg (Int * Float); */
/* type Foo = Stream (Int); */
/* type Foo = Stream ((Int * Float)); */

// The types of events handled by the main loop.
// For internal use only:
uniontype SwapperEvent a b = 
  OrigInput   a  |  // We get input data
  AckBack    ()  |  // We hear back that the switch is complete.
  FlushBack  ()  |  // We hear back that the flush is complete.
  OutA        b  |
  OutB        b  ;
  


// ================================================================================

// build_swapper takes two swapable components and builds a single
// aggregate component.
//build_swapper :: (Swapable(a,b), Swapable(a,b)) -> Swapable(a,b);
//fun build_swapper(aST, bST) aST





// The merged components create a stream transformer that accepts a
// "Switch!" message, and produces an ACK indicating that the switch
// has completed.
build_toggle :: (Swapable(a,b,st), Swapable(a,b,st)) 
   -> ST (Union2(a,()), Union2(b,ACK));


fun build_toggle(abox, bbox) 
 fun (strm) {
  empty = iterate _ in timer(3.0) {};

  //  instream :: Stream SwapperEvent (a);
  instream = smap(OrigInput,strm);

  // Input strm is Left for data and Right for SWITCH! command.
  // This is confusing because we wrap that in another union:
  loopout = feedbackloop(instream,
   fun(loopback) {
     iterate x in loopback {
       state { 
         buf = FIFO:make(10);
	 locked = false;
	 swtch = true;
       }
       
       case x {
         OrigInput(y): case y {
           // Data:
           Left (data): 
	     if locked 
	     then FIFO:enqueue(buf,data)
             else if swtch
	     then emit OutA(data)
             else emit OutB(data)
           Right(_): {}	   
  	  }
         AckBack(_): {}
         FlushBack(_): {}

	 OutA(_): {}
	 OutB(_): {}
       }
 
     }
  });


  fun filtA(s) iterate x in s { case x { OutA(a): emit a } };
  fun filtB(s) iterate x in s { case x { OutB(b): emit b } };

  s1 = smap(Oneof3, abox(filtA(loopout)));
  s2 = smap(Oneof3, bbox(filtB(loopout))); 

  merged = merge(s1,s2);

  empty


  /*
  result = iterate sum in strm {
    // First we need a buffer to handle input while we're doing the switch.
    state { buf = FIFO:make(10) }
    case sum {
      Left(elemA): {
      }        
      Right(_): ()
    }  
  };
  result
  */
 }


