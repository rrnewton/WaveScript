include "../common.ws"

numOps = tryLookup("NUMOPS", 100)
bufsize = tryLookup("BUFSIZE", 100)

src = iterate _ in timer(100) {
  emit Array:build(bufsize, fun (i) 10);
}

fun f (x) Array:map(fun (x) x + 1, x)
fun g (x) Array:map(fun (x) x * 10, x)
fun h (x) Array:map(fun (x) x / 9, x)
fun i (x) Array:map(fun (x) x - 2, x)

main = op4_pipe(i, h, g, f, numOps / 4, src)