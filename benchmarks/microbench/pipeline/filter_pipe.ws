include "math.ws"
include "common.ws"

numFilters = tryLookup("NUMFILTERS", 100)

src = iterate _ in timer(100) {
  state { cnt = 1; }
  emit cnt;
  cnt := cnt + 1
}

//filter out primes... Broken.
main = {
  fun f (n, s) {
    if n == 1 then s else
    iterate x in f(n-1, s) { if x > n then
    	      	 	       if (x.moduloI(n) > 0) then 
			         emit x }
  };
  f(numFilters, src);
}