include "../common.ws"

numFilters = tryLookup("NUMOPS", 100)

// iota stream 
src = iterate _ in timer(100) {
  state { cnt = 1; }
  emit cnt;
  cnt := cnt + 1
}

main = {
  // f is the sieve of eratosthenes
  //   places a filter on 1..n
  fun f (n, s) {
    if n == 1 then s else
    iterate x in f(n-1, s) { if (moduloI(x,n) > 0) then emit x else if x == n then emit x }
  };
  f(numFilters, src);
}
