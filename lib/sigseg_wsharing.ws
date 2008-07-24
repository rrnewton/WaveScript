
// Implementing sigsegs in the language:

// This version is analogous to sigseg_wsharing.sml in the mlton backend.

/* NOTE: comparison is tricky.  We can still use the normal WS equals
 * function, but we have to be careful.  Two equivalent sigsegs must
 * have the same physical representation.
 */ 

type Timebase = Int;
type SampleNum = Int64;

// Sigseg consists of : data segments, start, size, offset, timebase
type Sigseg t = (List (Array t) * SampleNum * Int * Int * Timebase);

namespace Sigseg {
  using Array; using Mutable;

  nulltimebase :: Timebase;
  nulltimebase = 0;

  nullseg  :: Sigseg t;
  nullseg = ([], 0, 0, 0, nulltimebase)
  //nullseg = (([], 0, 0, 0, nulltimebase) :: Sigseg t);

  make_nullseg :: () -> Sigseg t;
  fun make_nullseg() nullseg

  timebase :: Sigseg t -> Timebase;
  width    :: Sigseg t -> Int;
  start    :: Sigseg t -> Int64;
  end      :: Sigseg t -> Int64;

  fun timebase ((l,st,sz,off,tb))  tb;
  fun width    ((l,st,sz,off,tb))  sz;
  fun start    ((l,st,sz,off,tb))  st;
  fun end      ((l,st,sz,off,tb))  st + intToInt64(sz - 1);

  seg_get  :: (Sigseg t, Int) -> t;  
  fun seg_get((l,st,sz,off,tb), index) {
    ptr = ref(l);
    ind = ref(index + off);
    while ind >= ptr.head.length {
      ind -= ptr.head.length;
      ptr := ptr.tail;
    };
    (ptr.head)[ind]
  }

  fun slice(arr, offset, len) {
    newarr = makeUNSAFE(len);
    blit(newarr, 0, arr, offset, len);
    newarr
  }
  fun reallocFirstChunk(ss) {
    let (l,st,sz,off,tb) = ss;
    if off == 0 then l
    else slice(l.head, off, l.head.length-off) ::: l.tail      
  }

  /* 
    ASSUMPTION: we assume that if two sigsegs the two sigsegs being
    joined have overlapping garbage regions (at the tail of the first,
    head of the second), then those overlapping garbage regions have
    identical contents (because of common descent).
    
    In debug mode we should verify this.
   */
  joinsegs :: (Sigseg t, Sigseg t) -> Sigseg t;
  fun joinsegs(a, b) {
    let (l1,s1,z1,off1,tb1) = a;
    let (l2,s2,z2,off2,tb2) = b;
    if z1 == 0 then b else 
    if z2 == 0 then a else {
      //end1 = s1 + SampleNum! z1;
      assert_eq("", s2, s1 + SampleNum! z1);
      assert_eq("", tb1, tb2);

      /*
      println("JOINING: l1: ");
      println(l1);
      println("And l2: ");
      println(l2);
      */

      //pos = ref$ s1;
      rem = ref$ z1 + off1;
      ptr = ref$ l1;
      acc = ref$ [];
      // Scroll to the end of the first sigseg:
      while (ptr.tail != []) {        
	//pos += ptr.head.length.gint;
	rem -= ptr.head.length;
	acc := ptr.head ::: acc;
	ptr := ptr.tail;
      };
      // Now, ptr points to the last seg of the first sigseg.
      trailing_garbage = ptr.head.length - rem;
      //real_end = s1 + SampleNum! (z1 + trailing_garbage);
           
      // Next take a bite out of the second sigseg, cutting off leading
      // garbage, and canceling the trailing garbage from the first sigseg.
      ptr2   = ref$ l2;
      junk   = ref$ off2 + trailing_garbage;
      p2head = ptr2.head;
      len    = p2head.length;     
      /* We temporarily assume that the physical representations share
         common descent.  Thus it will not be the case that the
         garbage end of the first sigseg extends PAST the first seg in
         the second.  */
      if junk > len then wserror("SIGSEG invariant violated");
      if junk == len 
      then ptr2 := ptr2.tail  // A clean cut!!
      // Otherwise we carve the garbage end off of ptr2:
      else ptr2 := slice(p2head, junk, len-junk) ::: ptr2.tail;
            
      // Now reassemble acc, ptr, ptr2:
      /* TODO: OPTIMIZATION: if we have not actually added anything to
         the end of the first sigseg, we do not need to rebuild it's
         spine */
      ptr2 := ptr.head ::: ptr2;
      while (acc != []) {
        ptr2 := acc.head ::: ptr2;
        acc  := acc.tail;
      };

      //println("And l2NEW: ");
      //println(l2new);
/*       (List:append(l1,l2new), s1, z1+z2, off1, tb1) */
      (ptr2, s1, z1+z2, off1, tb1)
    }
  }

  toSigseg :: (Array t, Int64, Timebase) -> Sigseg t;
  fun toSigseg(arr, st, tb) ([arr], st, arr.length, 0, tb)

  // Does not cache results yet...
  toArray  :: Sigseg t -> Array t;
  fun toArray((l,st,sz,off,tb))
    if sz == 0 then null else 
    // Optimization:         
    if off==0  &&  l.tail==[]  &&  l.head.length==sz
    then l.head // SHOULD NOT BE MUTATED.
    else {
      arr = makeUNSAFE(sz);
      // Copy the first seg, deal with the offset:
      h = l.head;
      first = min(sz, h.length - off);
      blit(arr, 0, h, off, first);
      i   = ref(first); // output index      
      ptr = ref(l.tail);
      while ptr != [] {
	h   = ptr.head; 
	len = h.length;
        blit(arr, i, h, 0, len);
	i += len;
        ptr := ptr.tail;
      };
      arr
    }

  subseg   :: (Sigseg t, Int64, Int) -> Sigseg t;
  fun subseg((l,st,sz,off,tb), pos, len) 
    if len == 0 then nullseg else {
      assert("", pos - st + len.gint <= sz.gint);
      segptr = ref$ l;
      acc    = ref$ [];
      // This is the first position we are interested in:
      skip   = ref$ off + Int!(pos - st);
      // This is a counter for how many we need.
      remain = ref$ len;
      go     = ref$ true;
      // First, scroll forward to the start.
      while (go) {
	h = segptr.head;
	hlen = h.length;
        if skip < hlen then {
 	  acc := [h];
          // Next, grab as many segs as we need, copy the spine.
	  // TODO: It is worth trying to not reallocate the spine, at
	  // the expense of keeping a useless (potentially large), dangling tail.
	  // We could apply that opt if the tail would be less than a certain size...
	  if skip+len > hlen then {
            remain -= (hlen - skip);	   
	    while(remain > 0) {
       	      segptr := segptr.tail;
  	      h       = segptr.head;
	      acc    := h ::: acc;
	      remain -= h.length;
	    }	    
	  };
	  go := false; // Break out of the loop.
	} else {
	  segptr := segptr.tail;
	  skip   -= hlen;
	}
      };
      (List:reverse(acc), pos, len, skip, tb)
    }

} // End namespace
