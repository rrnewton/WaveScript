include "common.ws"

numOps = tryLookup("NUMOPS", 100)

src = createStream(fun(x) x, 10)

fun f (x) x * 10
fun g (x) x - 1
fun h (x) x / 9
fun i (x) x - 1

main = op4_pipe(i, h, g, f, numOps / 4, src)