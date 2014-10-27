include "../common.ws"

numFilters = tryLookup("NUMFILTERS", 10000)

src = iterate _ in timer(100) {
  state { cnt = 1; }
  emit cnt;
  cnt := cnt + 1
}

//filter out primes... Broken.
main = {
  fun f (n, s) {
    if n == 1 then s else
    iterate x in f(n-1, s) { if (moduloI(x,n) > 0) then emit x else if x == n then emit x }
  };
  iterate x in f(numFilters, src) {
    state { cnt = 1 }
    if cnt == 10001 then emit x;
    cnt := cnt + 1
  }
}