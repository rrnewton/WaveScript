
// These are externally defined primitives that are currently only
// applied to the new wsc2 backend.

// [2008.08.20] Currently loading this BEFORE internal.ws


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

// Cannot currently keep track of missed input tuples for a node running wsc2:
fun getDroppedInputCount() (0::Int32)
fun led0Toggle() {}
fun led1Toggle() {}
fun led2Toggle() {}
fun led0On() {}
fun led1On() {}
fun led2On() {}
fun led0Off() {}
fun led1Off() {}
fun led2Off() {}

fun read_default_audio(bufsize,rate) 
  iterate _ in timer(1) { 
    wserror("read_default_audio: no generic audio support in wsc2 backend")
  }

// This is here instead of internal.ws because it's still primitive in
// some other backends (namely, MLton).
fun List:toArray(ls) {
  /*
  fun List:length(ls) {
    using List; using Mutable;
    count :: Ref Int = ref$ 0;
    ptr   = ref$ ls;
    while ptr != [] {
      ptr := ptr.tail;
      count += 1;
    };
    count
  };
  */

  len = List:length(ls);
  arr = Array:makeUNSAFE(len);
  ptr = ls;
  for i = 0 to len-1 {
    arr[i] := ptr.head;
    ptr := ptr.tail;
  };
  arr
}


// No reason to add a primitive for this.  It is defined in wsc2.h
lshiftC :: (Char,Int)  -> Char = foreign("ws_lshiftC", []);
lorC    :: (Char,Char) -> Char = foreign("ws_lorC", []);

wsexit :: (Int) -> a = foreign("wsexit_fun", []);
exit = wsexit;


// FIXME : I can't currently define this locally (inside an iterate)... having problems. [2008.08.27]
__getcmdln :: () -> Array String = foreign("ws_get_command_line", []);
commandLine :: () -> Array String;
fun commandLine() {
  __getcmdln();
}
//argv = commandLine(); // An alias.
