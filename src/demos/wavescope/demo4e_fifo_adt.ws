
// This implementation uses a circular buffer:
// Contains start (inclusive) and count
// Because Refs are not first class, we use an array to store the start/count.
type Queue t = (Array t * Array Int);

PRINT = true;
//PRINT = false;

namespace FIFO {
  fun make(n)   (Array:makeUNSAFE(n), Array:build(2, fun(_) 0));
  fun empty((_,stcnt))  stcnt[1] == 0
  fun enqueue((arr,stcnt), x) {
    if PRINT then print("\n    ENQUEUEING "++x++" "++(arr,stcnt)++"\n");
    len = arr`Array:length;
    if stcnt[1] == len
    then wserror("FIFO:enqueue - queue full!")
    else {
      ind = stcnt[0] + stcnt[1];
      if ind >= len
      then arr[ind-len] := x
      else arr[ind]     := x;
      // Increase the count by one:
      stcnt[1] := stcnt[1] + 1;
    };
    if PRINT then print("    FIN ENQUEUEING "++x++" "++(arr,stcnt)++"\n");
  }
  fun dequeue((arr,stcnt)) {
    if PRINT then print("DEQUEUEING: "++(arr,stcnt)++"\n");
    len = arr`Array:length;
    if stcnt[1] == 0    
    then wserror("FIFO:dequeue - queue empty!")
    else {
      st = stcnt[0];
      if st + 1 == len 
        then stcnt[0] := 0
        else stcnt[0] := st + 1;
      arr[st];
    }
  }
}

// Simple enqueue and deque a couple elements every iteration: 
main = iterate _ in timer(3.0) {
  foo = FIFO:make(3);
  if PRINT then print("  FOO1: "++foo++"\n");
  FIFO:enqueue(foo,39);
  if PRINT then print("  FOO2: "++foo++"\n");
  FIFO:enqueue(foo,99);
  if PRINT then print("  FOO3: "++foo++"\n");
  emit FIFO:dequeue(foo);
  emit FIFO:dequeue(foo);
}
