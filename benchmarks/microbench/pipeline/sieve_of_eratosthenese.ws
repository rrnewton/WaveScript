include "../common.ws"

numFilters = tryLookup("NUMFILTERS", 100)

// iota stream 
src = createCntStream(fun (x) x + 1, 1)

main = {
  // f is the sieve of eratosthenes
  //   places a filter on 1..n
  fun f (n, s) {
    if n == 1 then s else
    iterate x in f(n-1, s) { if (moduloI(x,n) > 0) then emit x else if x == n then emit x }
  };
  f(numFilters, src);
}