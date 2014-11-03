include "../common.ws"

numOps = tryLookup("NUMOPS", 100)
bufsize = tryLookup("BUFSIZE", 100)

src = iterate _ in timer(100) {
  emit List:build(bufsize, fun (i) 10);
}

fun f (x) List:map(fun (x) x + 1, x)
fun g (x) List:map(fun (x) x * 10, x)
fun h (x) List:map(fun (x) x / 9, x)
fun i (x) List:map(fun (x) x - 2, x)

main = op4_pipe(i, h, g, f, numOps / 4, src)