include "../common.ws"

numStreams = tryLookup("NUMSTREAMS", 100)
numMerges = tryLookup("NUMMERGES", 1)

src = createCntStream(fun (x) x + 1, 1)

streams = createNStreams(numStreams, src)

// straight merge
main = mergeNStreams1(pull(numMerges, streams))