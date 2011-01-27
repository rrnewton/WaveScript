
/* [2008.04.05] Fixed the problems with meta-eval that stopped this
 * from working.  This version of FIFOs is useful for tinyOS, or
 * anywhere else that static allocation is needed.
 *
 * However, when using this library, one must be mindful of the
 * scheduler in question.  If there are any "amplifying" iterates --
 * ones that produce many output tuples in a single execution -- they
 * will swamp "zip" operators based on these FIFOs.
 * 
 * It would also be useful to have an array-based version with
 * growable storage.  In fact, that should be the default.
 */

// [2007.07.29] This array based implementation exposed some problems
// with the meta-evaluation framework.

// Contains start (inclusive) and count
// Because Refs are not first class, we use an array to store the [start,count].
type Queue t = (Array t * Array Int);
namespace FIFO {
  fun make(n)   (Array:makeUNSAFE(n), Array:build(2, fun(_) 0));
  fun empty((_,stcnt))  stcnt[1] == 0
  fun enqueue((arr,stcnt), x) {
    len = arr`Array:length;
    if stcnt[1] == len
    then wserror("FIFO:enqueue - queue full! ")
    else {
      ind = stcnt[0] + stcnt[1];
      if ind >= len
      then arr[ind-len] := x
      else arr[ind]     := x;
      // Increase the count by one:
      stcnt[1] := stcnt[1] + 1;
    };
  }
  fun dequeue((arr,stcnt)) {
    len = arr`Array:length;
    if stcnt[1] == 0    
    then wserror("FIFO:dequeue - queue empty!")
    else {
      // Reduce the count:
      stcnt[1] := stcnt[1] - 1;
      // Bump the start pointer:
      st = stcnt[0];
      if st + 1 == len 
        then stcnt[0] := 0
        else stcnt[0] := st + 1;
      // Return the orig:
      arr[st];
    }
  }
  fun peek((arr,stcnt), i) {
    len = arr`Array:length;
    ind = i + stcnt[0];
    if ind >= len 
    then arr[ind-len]
    else arr[ind]
  }
  fun elements((_,stcnt)) stcnt[1];
  fun andmap(fn,(arr,stcnt)) {
    len = Array:length(arr);
    elems = stcnt[1];
    println("Handling elems "++elems);
    go = Mutable:ref(true);
    i  = Mutable:ref(stcnt[0]);
    cnt = Mutable:ref(0);
    while go && cnt < elems {
      println("  Looping, i = "++i);
      if not(fn$arr[i]) then go := false;
      i += 1;
      cnt += 1;
      if i >= len then i := 0;
    };
    go
  }
  makefifo = FIFO:make;
}

