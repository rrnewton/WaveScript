include "common.ws"

numOps = tryLookup("NUMOPS", 100)

srcs = createStreams(fun (x) x + 1, 1, numOps)

// linear merge
main = mergeStreams1(srcs)