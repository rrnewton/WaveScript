include "common.ws"

numFilters = tryLookup("NUMFILTERS", 5)

src = iterate _ in timer(100) {
  state { cnt = 0 }
  emit cnt;
  cnt += 1;
}

fun createNStreams(n,s) {
  fun f (n) iterate x in s { if moduloI(x,n) == 0 then emit x };
  fun g (n) if n == 1 then [] else f(n):::g(n-1);
  g(n)
}

//says the head isn't a stream. Hmmmm...
//main = head(createNStreams(numFilters,src)) 
main = src