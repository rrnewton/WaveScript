include "../common.ws"

numFilters = tryLookup("NUMFILTERS", 100)

src = iterate _ in timer(100) {
  state { cnt = 0 }
  emit cnt;
  cnt += 1;
}

createNStreams :: (Int, Stream Int) -> List (Stream Int);
fun createNStreams(n,s) {
  fun f (n) iterate x in s { if moduloI(x,n) == 0 then emit x };
  fun g (n) if n == 1 then [] else f(n):::g(n-1);
  g(n)
}

createNewStreams :: (Int, Stream Int) -> List (Stream Int);
fun createNewStreams(n,s) {
  fun f (n) iterate x in s { emit x; };
  fun g (n) if n == 1 then [] else f(n):::g(n-1);
  g(n)
} 

mergeN :: List (Stream Int) -> Stream Int;
fun mergeN(l) {
  fun f (l) if tail(l)==[] then head(l) else merge(head(l), f(tail(l)));
  f(l)
}

//main = head(createNStreams(numFilters,src)) //use for dead code analysis

//main = zipN_sametype(10, createNStreams (5, src))

main = mergeN(createNewStreams(numFilters,src))