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

