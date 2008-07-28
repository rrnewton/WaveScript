
// These are externally defined primitives that are currently only
// applied to the new wsc2 backend.


// [2008.07.28] New system.  There's now a scheme parameter
// (wsc2-sigseg-mode) that controls which sigseg implementation is included:
//include "sigseg_copyalways.ws"
//include "sigseg_wsharing.ws"
//include "sigseg.ws"


using Sigseg;

// Also, for now we just define unionList using merge:

fun unionList(ls) {
  using List; using Mutable; 
  ptr = ref$ ls;
  cnt = ref$ 0;
  acc = ref$ [];
  while not(is_null(ptr)) {
    ind = cnt;
    acc := iterate x in ptr`head { emit(ind, x) } ::: acc;
    ptr := ptr`tail;
    cnt += 1;    
  };
  fold(merge, acc`head, acc`tail)
}


/*
main = iterate _ in timer(3.0) { 
  using Sigseg;
  s :: SS Int = nullseg;
  s2 = joinsegs(s,s);

  w :: Sigseg Int = toSigseg(Array:make(100, 90), 50, nulltimebase);

  //nl = nullseg;
  v :: Sigseg Int16 = make_nullseg();
  x :: Sigseg Int64 = make_nullseg();
  //x = nullseg;

  a :: List Int16 = [];
  b :: List Int64 = [];
  null = [];
  c :: List Int = null;
  d :: List Float = null;

  emit w;
}

*/
