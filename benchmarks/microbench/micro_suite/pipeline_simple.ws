include "common.ws"

numOps = tryLookup("NUMOPS", 100)

src = createStream(fun(x) x, 0)

fun f (x) x + 1

main = op_pipe(f, numOps, src)