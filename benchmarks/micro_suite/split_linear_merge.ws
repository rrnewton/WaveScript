include "common.ws"

numOps = tryLookup("NUMOPS", 100)

src = createStream(fun (x) x + 1, 1)

streams = splitStream(numOps, src)

// linear merge
main = mergeStreams1(streams)