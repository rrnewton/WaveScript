include "stdlib.ws"

fun trylookup(str,def){
    if GETENV(str) == ""
    then def
    else stringToInt(GETENV(str));
}

numOps = trylookup("NUMOPS", 1000);

s1 = iterate _ in timer(100) {
   state { cnt = 0; }
   emit cnt;
   cnt += 1;
}

main = for i = 1 to numOps {
         smap(fun(x) (2 * x), s1);
       };