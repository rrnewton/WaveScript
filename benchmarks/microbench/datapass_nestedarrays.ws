
// This demo allocates an array and then passes it repeatedly. 

// [2007.12.06] It's an overly simple demo, but it really brings out the differences between the backends:
// Running on faith:
//   wsc2     - 24ms   (with naive reference counting)
//   mlton    - 475ms
//   schemeO3 - 1100 ms
// WSC is very inconsistent right now.
//   wsc.new  - 1634/908  real/user  (this is O3 but with thread support)
//   wsc.new  - 3064/1768 real/user  (this is O3 but with thread support)
//   wsc.new  - 2886/1624 real/usr   (O2)

//printevery = 20 * 1000;
printevery = 20;
//printevery = 1;

size = 1000

source = iterate _ in timer(10.0) {
  // Don't know how makeUNSAFE can work with the reference counting scheme:
  //arr = Array:build(size, fun(i) Array:make(size, 0));
  arr = Array:make(size, Array:null);
  for i = 0 to size-1 { arr[i] := Array:make(size,0); };
  arrinner = arr[size/2];
  arrinner[size/2] := 39;
  emit arr;
}
// We don't want to print too much output, only produce an output every once in a while
BASE <- iterate arr in source {
  state { count :: Int = 0 }
  count += 1;
  if count == printevery then {
    count := 0;
    emit (arr[size/2])[size/2]; 
    //emit 99;
  }
}


/*

export LD_PRELOAD=/usr/lib/libhoard.so

export LD_PRELOAD="/home/newton/build/hoard-37/src/libhoard.so"

export LD_PRELOAD="/home/newton/build/hoard-37/src/libhoard.so:/lib/libdl-2.7.so"

export LD_PRELOAD="/usr/lib/libtcmalloc.so.0"


*/
