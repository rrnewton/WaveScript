include "common.ws"

numOps = tryLookup("NUMOPS", 100)

srcs = createStreams(fun (x) x + 1, 1, numOps)

// tree merge
main = mergeStreams2(srcs)