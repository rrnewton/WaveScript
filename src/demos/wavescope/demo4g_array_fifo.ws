

// This implementation will use a GROWING buffer.


// Because Refs are not first class, we use arrays as refs:

// Our queue datatype contains:
//   (1) A mutable reference to an array (the buffer).
//       (To keep the array pointer itself mutable this is a nested array.)
//   (2) The index of the starting element.
//   (3) The number of elements currently residing in the buffer.
// type Queue t = (Array (Array t) * Array Int);

// Here we use a RECORD type:
type FIFO t = (| BUFFER : Array (Array t), 
                  START  : Array Int,
                  ELEMS  : Array Int );

__DEBUG = false;

FIFO:make       ::  Int -> FIFO t;
FIFO:empty      ::  FIFO t -> Bool;
FIFO:enqueue    :: (FIFO t, t) -> ();
FIFO:dequeue    ::  FIFO t -> t;

FIFO:peek       :: (FIFO t,Int) -> t;
FIFO:elements   ::  FIFO t -> Int;



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


// Simple enqueue and deque a couple elements every iteration: 
main = iterate _ in timer(3.0) {
  state { myfifo = FIFO:make(3);
          cntr   = (0 :: Int);
        }

  // Push two elements and then pop one:
  cntr += 1;
  FIFO:enqueue(myfifo, cntr);

  cntr += 1;
  FIFO:enqueue(myfifo, cntr);

  emit myfifo;

  print("---- before and after deque -----");
  FIFO:dequeue(myfifo);

  // Then in this test we emit the FIFO *itself* so we can peek at its
  // representation:
  emit myfifo;

  // Or we could just peek:
  print("peek is: " ++ FIFO:peek( myfifo, 0 ) ++ "\n");

  print("Number of Elements: " ++ FIFO:elements(myfifo) ++ "\n");
}
