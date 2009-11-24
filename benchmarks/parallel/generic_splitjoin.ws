
// Workers in parallel rather than series.

include "generic_chain.ws"
include "stdlib.ws"

// Override main:
//main = parmap(workers, work, src)
main = smap(fun(_) (), parmap(numcpus, work, src))
