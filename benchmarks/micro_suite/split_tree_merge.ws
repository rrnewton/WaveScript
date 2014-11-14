include "common.ws"

numOps = tryLookup("NUMOPS", 100)

src = createStream(fun (x) x + 1, 1)

streams = splitStream(numOps, src)

// tree merge
main = mergeStreams2(streams)