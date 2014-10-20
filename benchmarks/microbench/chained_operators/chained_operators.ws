include "stdlib.ws"

fun trylookup(str,def){
    if GETENV(str) == ""
    then def
    else stringToInt(GETENV(str));
}

numOps = trylookup("NUMOPS", 1000)

s1 = iterate _ in timer(100) {
   state { cnt = 0; }
   emit cnt;
   cnt := cnt + 1;
}

main = {
  fun f(n,s)
    if n == 0 then s else
    iterate x in f(n-1,s) { emit x+1 };
  f(numOps,s1)
}