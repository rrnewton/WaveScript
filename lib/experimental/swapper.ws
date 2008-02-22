






// This is a library that implements "swappable" components ("blocks") for the
// stream graph.

include "stdlib.ws";

// ================================================================================
// Data type defs:

type ST (a,b) = Stream a -> Stream b;
//type Quiesce = ();
//type Activate st = st;
//type ACK = ();

// The types of events handled by the main loop.
// For internal use only:
uniontype SwapperEvent a b = 
  OrigInput   a  |  // We get input data
  AckBack    ()  |  // We hear back that the switch is complete.
  FlushBack  ()  |  // We hear back that the flush is complete.
  OutA        b  |
  OutB        b  ;
uniontype SwapBlockMsg a st =
  SB_data      a  |
  SB_quiesce  ()  |
  SB_ack      ()  |
  SB_activate st  ;
 
// These are the messages that get sent to and from swappable blocks.  
uniontype SwapBlockMsg a st =
  SB_data      a  |
  SB_quiesce  ()  |
  SB_ack      ()  |
  SB_activate st  ;

//type SwapBlock (a,b,st) = ST (SwapBlockMsg(a,st), SwapBlockMsg(b,st));

// A swappable component is a normal stream transformer *except* that
// it can also take a Queisce request and produce an Activate (state bundle).
// OR it can take an Activate and produce an ACK.
type Swapable (a, b, st) = 
     Stream (SwapBlockMsg (a, st))
  -> Stream (SwapBlockMsg (b, st));

 

// ================================================================================

// build_swapper takes two swapable components and builds a single
// aggregate component.
//build_swapper :: (Swapable(a,b), Swapable(a,b)) -> Swapable(a,b);
//fun build_swapper(aST, bST) aST





// The merged components create a stream transformer that accepts a
// "Switch!" message, and produces an ACK indicating that the switch
// has completed.

//build_toggle :: (Swapable(a,b,st), Swapable(a,b,st))
//   -> ST (Union2(a,()), Union2(b,ACK));

//build_toggle :: (swa, swb) -> ST (Union2(a,()), Union2(b,()));
fun build_toggle(abox, bbox) 
 fun (strm) {

  //  instream :: Stream SwapperEvent (a);
  // Input strm is Left for data and Right for SWITCH! command.
  // This is confusing because we wrap that in another union:
  instream = smap(OrigInput,strm);

  loopout = feedbackloop(instream,
   fun(loopback) {
     iterate x in loopback {
       state { 
         buf = FIFO:make(10);
	 locked = false;
	 swtch = true;
       }
       
       print("      ITERATING, locked "++locked++" swtch "++swtch++"\n");
       case x {
         OrigInput(y): case y {           
           Left (data): // Data
	     if locked 
	     then FIFO:enqueue(buf,data)
             else if swtch
	     then emit OutA(data)
             else emit OutB(data)
           Right(_): {}	// Switch! command
  	  }
         AckBack(_): {}
         FlushBack(_): {}

	 OutA(_): {}
	 OutB(_): {}
       }
 
     }
  });


  fun filtA(s) iterate x in s { case x { OutA(a): emit SB_data(a) | _: () } };
  fun filtB(s) iterate x in s { case x { OutB(b): emit SB_data(b) | _: () } };
  fun filtDat(s) iterate x in s { case x { SB_data(x): emit x | _: () } };
				  /*
  s1 = smap(Oneof3, abox(filtA(loopout)));
  s2 = smap(Oneof3, bbox(filtB(loopout))); 
  merge(s1,s2);
				  */
  merge(filtDat(abox(filtA(loopout))), 
        filtDat(bbox(filtB(loopout))))

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


//foo :: Swapable (Int,Int,Int);
//foo = wserror("");

//testblock1 :: Int -> Swapable (a,b,st);
testblock1 = fun(ind) fun(strm) {
  iterate x in strm {
    state { cnt=0 }
    case x {
      SB_data (dat): // data to process
      {
	println("Block "++ind++" processing dat "++dat++" with state= "++cnt);
	// Doesn't work, something's broken!!!!!!
	//emit Oneof3(intToFloat(dat))
	//emit Oneof3(intToFloat(100 + dat))
	emit SB_data(1000 + dat);
      }
      SB_quiesce (_): // a Quiesce message, go to sleep.
      {
        println("Block "++ind++" going to sleep...");
        tmp = cnt;
	cnt := 0;
        emit SB_activate(tmp);
      }
      SB_activate(st): // a startup message with a payload
      {
        println("Block "++ind++" starting up!!");
        cnt := st;
	emit SB_ack(()); // An ACK
      }
    }
  }
}


switchable = build_toggle(testblock1(777), testblock1(888))

test = smap(Left,COUNTUP(100))
BASE <- switchable(test)
