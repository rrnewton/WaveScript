include "../common.ws"

//default 
numOps = tryLookup("NUMOPS", 100)
bufsize = tryLookup("BUFSIZE", 100)

src = iterate _ in timer(100) {
  emit Array:build(bufsize, fun (i) i)
}

fun f (x) Array:fold((+),0,x)
fun g (x) Array:build(x, fun (i) i)
fun h (x) Array:fold((+),0,Array:map(fun (x) x*x, x))
fun i (x) Array:build(if (moduloI(x,bufsize) < bufsize/2) then bufsize else moduloI(x,bufsize), fun (i) i)

main = op4_pipe(i, h, g, f, numOps / 4, src)

