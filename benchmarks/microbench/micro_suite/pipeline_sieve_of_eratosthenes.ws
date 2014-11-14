include "common.ws"

numOps = tryLookup("NUMOPS", 100)

src = createCntStream(fun (x) x + 1, 1)

main = {
  // f is the sieve of eratosthenes
  //   places filters on 2..n
  fun f (n, s) {
    if n == 1 then s 
    else
      iterate x in f(n-1, s) { 
        if (moduloI(x,n) > 0) then 
    	  emit x 
        else if x == n then 
    	  emit x 
      }
  };
  f(numOps, src);
}