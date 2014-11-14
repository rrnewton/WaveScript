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

// split a single stream evenly into n streams
splitStream :: (Int, Stream Int) -> List (Stream Int);
fun splitStream (n, src) {
  fun f (nth) {
    iterate x in src {
      state { cnt = 1 }
      if moduloI(cnt + (n - nth), n) == 0 then {
        emit x;
      };
      cnt += 1;
    }
  };
  fun g (i) if i==n+1 then [] else f(i):::g(i+1);
  g(1); 
}

// copy a single stream to n streams
copyStream :: (Int, Stream Int) -> List (Stream Int);
fun copyStream (n, src) {
  fun f (nth) {
    iterate x in src { emit x }
  };
  fun g (i) if i==n+1 then [] else f(i):::g(i+1);
  g(1); 
}

// straight line merge
mergeStreams1 :: List (Stream Int) -> Stream Int;
fun mergeStreams1(streams) {
  fun f (ls) if tail(ls)==[] then head(ls) else merge(head(ls), f(tail(ls)));
  f(streams)
}

// tree merge
mergeStreams2 :: List (Stream Int) -> Stream Int;
fun mergeStreams2(streams) {
  fun f (ls) {
    if tail(ls)==[] then
      head(ls)
    else
      if tail(tail(ls))==[] then 
        merge(head(ls), head(tail(ls)))
      else
        merge(merge(head(ls), head(tail(ls))), f(tail(tail(ls))))
  };
  f(streams)
}

pull :: (Int, List a) -> List a;
fun pull(n, ls) {
  fun f(n, ls) {
    if n == 0 then [] else head(ls):::f(n-1, tail(ls))
  };
  f(n, ls);
}

// create a incremental stream given a start int and a successor function
createStream :: (Int -> Int, Int) -> Stream Int;
fun createStream (succ, start) {
  iterate _ in timer(100) {
    state { cnt = start }
    emit cnt;
    cnt := succ(cnt);
  }
}

// create a incremental stream given a start int and a successor function
createStreams :: (Int -> Int, Int, Int) -> List (Stream Int);
fun createStreams (succ, start, n) {
  fun f (n) {
    if n==0 then
      []
    else createStream(succ, start):::f(n-1)
  };
  f(n)
}