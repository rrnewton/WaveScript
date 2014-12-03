include "common.ws"

src = createStream(fun(x) x, 0)

main = smap(fun (x) x + 1000, src)