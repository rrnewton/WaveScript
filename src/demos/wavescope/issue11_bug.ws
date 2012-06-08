

// BUG this example RECORD types snuck through the records-to-tuples pass.


// Some helper functions to avoid a dependence on stdlib.ws:
fun stream_map(f,s) {
  iterate (x in s) {
    emit f(x);
  }
}
uniontype Union2 a b       = Left a | Right b;
fun union2(s1,s2) 
  merge(stream_map(Left,s1),
	stream_map(Right,s2))

// ================================================================================

// Here we use a RECORD type for FIFOs:
type FIFO t = (| BUFFER : Array (Array t), 
                  START  : Array Int,
                  ELEMS  : Array Int );

FIFO:make       ::  Int -> FIFO t;
FIFO:empty      ::  FIFO t -> Bool;
FIFO:enqueue    :: (FIFO t, t) -> ();
FIFO:dequeue    ::  FIFO t -> t;
FIFO:peek       :: (FIFO t,Int) -> t;
FIFO:elements   ::  FIFO t -> Int;

__DEBUG = false;
namespace FIFO {

  // Construct an empty array. #[] is an array-constant / literal:
  fun make(n)   (| BUFFER = #[ Array:makeUNSAFE(n) ], 
                   START = #[0],
                   ELEMS = #[0] );

  // Is a FIFO Queue empty?
  fun empty(q)  (q.ELEMS)[0] == 0;


  enqueue    :: (FIFO t, t) -> ();

  // Enqueing may need to increase the buffer size.
  fun enqueue(q, newitem) {
    if __DEBUG then print("\n   * Enqueing "++ newitem ++" "++ q ++"\n");

    let arrarr = q.BUFFER;
    let arr = arrarr[0];
    let elems = q.ELEMS;
    let start = q.START;

    // How much actual storage do we have?
    let len = Array:length( arr );

    // Buffer Growing:
    // ----------------------------------------
    if elems[0] == len then {
      if __DEBUG then print("  --> GROWING from "++ len ++ " TO " ++ len*2 ++ "\n");
      let newarr = Array:makeUNSAFE( len * 2 );

      // The current contents may be "wrapped around", but the new
      // array one won't be.  Note, Array:blit is like a memcpy:
      let firstblit = elems[0];
      let numwrapped = start[0] + elems[0] - len;
      if (numwrapped > 0) then {
        firstblit -= numwrapped;
        // If the elements WERE wrapped around, copy the wrapped bunch here:
	Array:blit(newarr, firstblit, arr, 0, numwrapped);
      };
      // Copy the initial segment here (this is out of order):
      Array:blit(newarr, 0, arr, start[0], firstblit);

      // Finally, replace the old array with the new array:
      arrarr[0] := newarr;
      newarr[elems[0]] := newitem;
      elems[0] += 1;
      start[0] := 0;   // added by Yuan, start be reset
    }
    // ----------------------------------------
    else 
    {
      let ind = start[0] + elems[0];

      // Next write the element into the correct position, possibly
      // wrapping around:
      if ind >= len
      then arr[ind-len] := newitem
      else arr[ind]     := newitem;
      // Increase the count by one:
      elems[0] := elems[0] + 1;
    };
    if __DEBUG then print("   * Finished Enqueueing "++ newitem ++" "++ q ++"\n");
  }


  // Pop an element and return it.
  // Should only succeeed if the FIFO is nonempty, otherwise error.
  dequeue    :: (FIFO t) -> t;

  fun dequeue( q ) {
    if __DEBUG then print("   * Dequeuing from: "++ q ++"\n");
    if empty(q)
    then wserror("FIFO:dequeue - queue empty!")
    else {
      let arrarr = q.BUFFER;
      let arr = arrarr[0];
      let elems = q.ELEMS;
      let start = q.START;
      let st = start[0];

      start[0] := start[0] + 1;
      elems[0] := elems[0] - 1;

      arr[st];       
    };    
  }

  // Peek ahead without popping.  peek(q,0) should show the next
  // element to be dequeued.
  peek       :: (FIFO t,Int) -> t;
  
  fun peek(q, index) {
    if __DEBUG then print("   * Peeking from: "++ q ++"\n");
    if empty(q)
    then wserror("FIFO:peek - queue empty!");
        
    let arrarr = q.BUFFER;
    let arr = arrarr[0];
    let len = Array:length( arr );
    
    let start = q.START;
    let elems = q.ELEMS;
    
    let idx = start[0] + index;

    if index >= elems[0]
    then wserror("FIFO:peek - out of boundary!")
    else {   
      if idx >= len
      then idx := idx - len;           
      
      arr[idx];  
    }    
  }


  // O(1) -- return the number of elements in the FIFO:
  elements   ::  FIFO t -> Int;	 
  
  fun elements(q) (q.ELEMS)[0];

}


// This is a temporary fake data source with a temporary fake schema.
type DummySchema99 = (| SYM:String, TIME:Float, PRICE:Float, VOLUME:Int );

all_syms = #["IBM", "GOOG", "GM", "F", "IMGN", "MSFT",
             "AAPL", "AAUKY", "AAV", "AAWW", "AB", "ABAX", "ABB", "ABC", "ABFS", "ABG", "ABM", "ABMD", "ABT", "ABV", "ABVT", "ABX",
             "ACC", "ACCL", "ACE", "ACET", "ACF", "ACGL", "ACGY", "ACH", "ACI", "ACIW", "ACL", "ACLI", "ACM", "ACN", "ACO", "ACOM",
             ];

// NON-Random version.  
wsq_nonRandomSource :: (Float, String) -> Stream DummySchema99;
fun wsq_nonRandomSource(freq, schema) {
  iterate _ in timer(freq) {
    state{ i = 0 }
    i += 1;
    ind = moduloI(i, Array:length(all_syms));

    f = Float! i;
    emit (SYM= all_syms[ind], 
          TIME= f,
          PRICE= f,
	      VOLUME= i
  	      );
  }
}

// Requires streams to be the same type.  The first argument to the
// function extracts a COMPARABLE quantity.  That is used to determine
// monotonicity.
wsq_mergeMonotonic :: ((a -> #b), Stream a, Stream a) -> Stream a ;
fun wsq_mergeMonotonic(extractor, s1, s2) {
  using FIFO;
  iterate(x in union2(s1,s2)) {
   state {
     buf1 = make(10); // ARBITRARY -- FIFO sizes
     buf2 = make(10);
   }
   case x { 
     Left  (a): buf1.enqueue(a)
     Right (b): buf2.enqueue(b)
   };
   while (buf1.elements > 0 && buf2.elements > 0) {
     a = buf1.peek(0);      
     b = buf2.peek(0);      
     if a.extractor <= b.extractor  then {
       buf1.dequeue();
       emit a;
     } else {
       buf2.dequeue();
       emit b;
     }
   }
 }
}

src = wsq_nonRandomSource(1.0, "")
main = wsq_mergeMonotonic( fun(x) { x.VOLUME }, src, src )
