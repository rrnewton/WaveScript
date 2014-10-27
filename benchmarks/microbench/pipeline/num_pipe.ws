include "../common.ws"

//default to 100 chained operators in pipeline. set environment
//variable NUMOPS at compile-time to change this.
numOps = tryLookup("NUMOPS", 100)

src = iterate _ in timer(100) {
   state { cnt = 0; }
   emit cnt;
   cnt := cnt + 1;
}

fun f (x) x + 1
fun g (x) x * 10
fun h (x) x / 9
fun i (x) x - 2

//
main = op4_pipe(i, h, g, f, numOps / 4, src)