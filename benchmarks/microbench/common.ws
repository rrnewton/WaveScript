include "stdlib.ws"

//looks in compile-time environment for str, otherwise it defaults to def
fun tryLookup(str, def) {
  if GETENV(str) == "" then def else {
    println("Using " ++ str ++ ": " ++ GETENV(str));
    stringToInt(GETENV(str))
 
 }
}

//create a n-sized pipeline of identical functions over a stream
op_pipe :: (a -> b, Int, Stream a) -> Stream b;
fun op_pipe(f1, n, s) {
  fun f (n, s) if n == 0 then s else smap(f1, f(n-1, s));
  f(n, s)
}

//create a n-sized pipeline of 2 different functions over a stream
op2_pipe :: (b -> c, a -> b, Int, Stream a) -> Stream c;
fun op2_pipe(f1, f2, n, s) {
  fun f (n, s) if n == 0 then s else smap(f1, smap(f2, f(n-1, s)));
  f(n, s)
}

//create a n-sized pipeline of 2 different functions over a stream
op3_pipe :: (c -> d, b -> c, a -> b, Int, Stream a) -> Stream c;
fun op3_pipe(f1, f2, f3, n, s) {
  fun f (n, s) if n == 0 then s else smap(f1, smap(f2, smap(f3, f(n-1, s))));
  f(n, s)
}

//create a n-sized pipeline of 2 different functions over a stream
op4_pipe :: (d -> e, c -> d, b -> c, a -> b, Int, Stream a) -> Stream c;
fun op4_pipe(f1, f2, f3, f4, n, s) {
  fun f (n, s) if n == 0 then s else smap(f1, smap(f2, smap(f3, smap(f4, f(n-1, s)))));
  f(n, s)
}

;; split a single stream evenly into n streams
createNStreams :: (Int, Stream Int) -> List (Stream Int);
fun createNStreams (n, src) {
  fun f (nth) {
    iterate x in src {
      state { cnt = 1 }
      if moduloI(cnt + (3 - nth), n) == 0 then {
        emit x;
      };
      cnt += 1;
    }
  };
  fun g (i) if i==n+1 then [] else f(i):::g(i+1);
  g(1); 
}

// create a incremental stream given a start int and a successor function
createCntStream :: (Int -> Int, Int) -> Stream Int;
fun createCntStream (succ, start) {
  iterate _ in timer(100) {
    state { cnt = start }
    emit cnt;
    cnt := succ(cnt);
  }
}